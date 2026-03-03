extract_rcdf_meta <- function(path, key) {

  info <- extract_rcdf(path, meta_only = TRUE)
  info$cleanup()

  info$meta[[key]]

}

extract_rcdf <- function(path, meta_only = FALSE) {

  if (!fs::file_exists(path)) {
    stop(glue::glue("RCDF file does not exist: {path}"))
  }

  extract_dir <- fs::file_temp(pattern = rcdf_temp_root())
  fs::dir_create(extract_dir)

  zip::unzip(path, exdir = extract_dir, junkpaths = TRUE)

  lineage_zip <- file.path(extract_dir, "lineage.zip")

  if (!meta_only && fs::file_exists(lineage_zip)) {
    zip::unzip(lineage_zip, exdir = extract_dir)
  }

  if (fs::file_exists(lineage_zip)) {
    unlink(lineage_zip, force = TRUE)
  }

  meta_file <- file.path(extract_dir, "metadata.json")

  if (!fs::file_exists(meta_file)) {
    unlink(extract_dir, recursive = TRUE, force = TRUE)
    stop("metadata.json not found inside RCDF archive.")
  }

  meta <- jsonlite::read_json(meta_file, simplifyVector = TRUE)

  list(
    meta = meta,
    dir  = extract_dir,
    cleanup = function() unlink(extract_dir, recursive = TRUE, force = TRUE)
  )
}

rcdf_temp_root <- function() {
  file.path(tempdir(), "__rcdf_temp__")
}

cleanup_rcdf_temp <- function() {
  root <- rcdf_temp_root()
  if (fs::dir_exists(root)) {
    unlink(root, recursive = TRUE, force = TRUE)
  }
}

resolve_rcdf_files <- function(path, recursive) {

  if (any(!fs::file_exists(path))) {
    stop(glue::glue(
      "Specified RCDF file does not exist: {paste(path, collapse = ', ')}"
    ))
  }

  if (length(path) == 1 && fs::is_dir(path)) {

    files <- list.files(
      path,
      pattern = "\\.rcdf$",
      full.names = TRUE,
      recursive = recursive
    )

    if (length(files) == 0) {
      stop(glue::glue(
        "No valid RCDF files in the path specified: {path}"
      ))
    }

    return(files)
  }

  if (all(!grepl("\\.rcdf$", path))) {
    stop(glue::glue(
      "Not a valid RCDF file: {paste(path, collapse = ', ')}"
    ))
  }

  path
}

normalize_credentials <- function(rcdf_files, decryption_key, password) {

  lf <- length(rcdf_files)
  ld <- length(decryption_key)
  lp <- length(password)

  if ((ld > 1 && lp > 1) &&
      (lf != ld || lf != lp || ld != lp)) {
    stop("Mismatched number of decryption keys provided.")
  }

  dk <- if (ld == 1 && lf > 1) rep(decryption_key, lf) else decryption_key
  pw <- if (lp == 1 && lf > 1) rep(password, lf) else password

  list(keys = dk, passwords = pw)
}

process_rcdf_file <- function(conn, rcdf_file, key, password, metadata, ignore_duplicates) {

  ext <- extract_rcdf(rcdf_file)
  on.exit(ext$cleanup(), add = TRUE)

  meta <- ext$meta
  secret <- decrypt_key(meta, key, password)

  pq_files <- list.files(
    file.path(ext$dir, "lineage"),
    pattern = "\\.parquet$",
    full.names = TRUE
  )

  pk <- resolve_primary_key(meta, metadata, ignore_duplicates)

  for (pq_file in pq_files) {
    process_parquet_file(conn, pq_file, secret, pk)
  }

  list(meta = meta, dir_base = ext$dir_base)

}

resolve_primary_key <- function(meta, metadata, ignore_duplicates) {

  if (ignore_duplicates) return(NULL)

  pk <- metadata$primary_key %||% metadata$primary_keys

  if (is.null(pk) &&
      "pk_field_name" %in% names(meta$checksum)) {
    pk <- meta$checksum
  }

  pk

}

process_parquet_file <- function(conn, pq_file, secret, pk) {

  record <- fs::path_ext_remove(basename(pq_file))

  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "PRAGMA add_parquet_key({secret$key}, {secret$value})",
      .con = conn
    )
  )

  if (!DBI::dbExistsTable(conn, record)) {

    DBI::dbExecute(
      conn,
      glue::glue_sql(
        "CREATE TABLE {`record`} AS
         SELECT * FROM read_parquet({pq_file},
         encryption_config = {{ footer_key: {secret$key} }});",
        .con = conn
      )
    )

    return(invisible())
  }

  record_temp <- paste0(record, "_temp")

  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "CREATE TABLE {`record_temp`} AS
       SELECT * FROM read_parquet({pq_file},
       encryption_config = {{ footer_key: {secret$key} }});",
      .con = conn
    )
  )

  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "INSERT INTO {`record`}
       SELECT * FROM {`record_temp`};",
      .con = conn
    )
  )

  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "DROP TABLE IF EXISTS {`record_temp`};",
      .con = conn
    )
  )
}

collect_tables <- function(conn, data_dictionary) {

  pq <- rcdf_list()

  for (record in DBI::dbListTables(conn)) {

    df <- dplyr::collect(dplyr::tbl(conn, record))
    if (nrow(df) == 0) next

    if (!is.null(data_dictionary) && length(data_dictionary) > 0) {
      df <- add_metadata(df, data_dictionary)
    }

    pq[[record]] <- df
  }

  pq
}
