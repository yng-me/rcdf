# RCDF archive module: the only place that knows the on-disk layout.
#
#   <name>.rcdf (zip)
#   └── <log_id>/
#       ├── metadata.json
#       └── lineage.zip  ->  lineage/<record>.parquet

# Folder, inside a staging or extract dir, that holds the record parquet files.
rcdf_lineage_dir <- function(dir) file.path(dir, "lineage")

# Record name of a parquet file: "path/d.parquet" or "d" -> "d".
rcdf_record_name <- function(file) sub("\\.parquet$", "", basename(file))

# Open an archive into a fresh temp dir. Returns list(meta, dir, files, cleanup);
# `files` are the record parquet files (empty when meta_only).
extract_rcdf <- function(path, meta_only = FALSE) {

  if (!file.exists(path)) {
    stop(paste0("Specified RCDF file does not exist: ", path))
  }

  if (!grepl("\\.rcdf$", path)) {
    stop(paste0("Not a valid RCDF file: ", path))
  }

  extract_dir <- tempfile()
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  cleanup <- function() unlink(extract_dir, recursive = TRUE, force = TRUE)

  zip::unzip(path, exdir = extract_dir, junkpaths = TRUE)

  lineage_zip <- file.path(extract_dir, "lineage.zip")

  if (file.exists(lineage_zip)) {
    if (!meta_only) zip::unzip(lineage_zip, exdir = extract_dir)
    unlink(lineage_zip, force = TRUE)
  }

  meta_file <- file.path(extract_dir, "metadata.json")

  if (!file.exists(meta_file)) {
    cleanup()
    stop("metadata.json not found inside RCDF archive.")
  }

  list(
    meta = jsonlite::read_json(meta_file, simplifyVector = TRUE),
    dir  = extract_dir,
    files = list.files(rcdf_lineage_dir(extract_dir), pattern = "\\.parquet$", full.names = TRUE),
    cleanup = cleanup
  )
}

# Pack the parquet files in rcdf_lineage_dir(staging_dir) and `meta` into an archive at `path`.
pack_rcdf <- function(staging_dir, meta, path) {

  dir_zip <- file.path(staging_dir, meta$log_id)
  dir.create(dir_zip, recursive = TRUE, showWarnings = FALSE)

  jsonlite::write_json(
    x = meta,
    path = file.path(dir_zip, "metadata.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  zip::zip(
    zipfile = file.path(dir_zip, "lineage.zip"),
    files = rcdf_lineage_dir(staging_dir),
    include_directories = FALSE,
    mode = "cherry-pick",
    # Store only: encrypted parquet does not compress, and deflate made zipping ~10x slower.
    compression_level = 0
  )

  zip::zip(
    zipfile = path,
    files = dir_zip,
    include_directories = FALSE,
    mode = "cherry-pick",
    compression_level = 0
  )

  invisible(path)
}

# Fold one archive's metadata into an accumulator (start with list()).
# Besides the fields read_rcdf(return_meta = TRUE) attaches, it collects
# `dictionary` and the `primary_key` rows stored in each archive's checksum,
# so a multi-file read sees every file's dictionary and keys.
combine_rcdf_meta <- function(acc, meta) {

  acc$log_id     <- c(acc$log_id, meta$log_id)
  acc$created_at <- c(acc$created_at, meta$created_at)
  acc$version    <- c(acc$version, meta$version)

  acc$area_names <- merge_by_name(acc$area_names, meta$area_names)
  acc$meta <- merge_by_name(acc$meta, meta$meta)
  acc$summary_statistics <- merge_by_name(acc$summary_statistics, meta$summary_statistics)
  acc$dictionary <- merge_by_name(acc$dictionary, meta$dictionary)

  if ("pk_field_name" %in% names(meta$checksum)) {
    stored <- meta$checksum[!is.na(meta$checksum$pk_field_name), c("file", "pk_field_name"), drop = FALSE]
    acc$primary_key <- merge_by_name(acc$primary_key, stored)
  }

  acc
}

# Merge metadata from two archives without dropping either side:
# data frames are stacked, named lists are merged key by key, plain vectors are
# combined (repeats removed), and anything else of different shapes is kept as both.
merge_by_name <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  if (is.data.frame(a) && is.data.frame(b)) return(dplyr::bind_rows(a, b))
  if (is.list(a) && is.list(b) && !is.null(names(a)) && !is.null(names(b))) {
    keys <- union(names(a), names(b))
    return(stats::setNames(lapply(keys, function(k) merge_by_name(a[[k]], b[[k]])), keys))
  }
  if (is.atomic(a) && is.atomic(b)) return(unique(c(a, b)))
  list(a, b)
}

# Stop if any extracted parquet file differs from the md5 recorded at write time.
# Detects corruption only: the md5 sits in unsigned metadata.json, so it is not tamper-proof.
# Files missing from the recorded checksum (e.g. older archives) are skipped.
verify_rcdf_checksum <- function(pq_files, checksum) {
  if (is.null(checksum) || !all(c("file", "value") %in% names(checksum))) return(invisible())
  expected <- stats::setNames(checksum$value, checksum$file)
  actual <- tools::md5sum(pq_files)
  names(actual) <- basename(pq_files)
  known <- intersect(names(actual), names(expected))
  bad <- known[actual[known] != expected[known]]
  if (length(bad) > 0) {
    stop("RCDF archive is corrupted (checksum mismatch): ", paste(bad, collapse = ", "), call. = FALSE)
  }
  invisible()
}
