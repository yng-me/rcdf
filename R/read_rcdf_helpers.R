extract_rcdf_meta <- function(path, key) {

  info <- extract_rcdf(path, meta_only = TRUE)
  info$cleanup()

  info$meta[[key]]

}

extract_rcdf <- function(path, meta_only = FALSE) {

  if (!file.exists(path)) {
    stop(paste0("RCDF file does not exist: ", path))
  }

  extract_dir <- tempfile()
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

  zip::unzip(path, exdir = extract_dir, junkpaths = TRUE)

  lineage_zip <- file.path(extract_dir, "lineage.zip")

  if (!meta_only && file.exists(lineage_zip)) {
    zip::unzip(lineage_zip, exdir = extract_dir)
  }

  if (file.exists(lineage_zip)) {
    unlink(lineage_zip, force = TRUE)
  }

  meta_file <- file.path(extract_dir, "metadata.json")

  if (!file.exists(meta_file)) {
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
  if (dir.exists(root)) {
    unlink(root, recursive = TRUE, force = TRUE)
  }
}

resolve_rcdf_files <- function(path, recursive) {

  if (any(!file.exists(path))) {
    stop(paste0(
      "Specified RCDF file does not exist: ",
      paste(path, collapse = ", ")
    ))
  }

  if (length(path) == 1 && isTRUE(file.info(path)$isdir)) {

    files <- list.files(
      path,
      pattern = "\\.rcdf$",
      full.names = TRUE,
      recursive = recursive
    )

    if (length(files) == 0) {
      stop(paste0("No valid RCDF files in the path specified: ", path))
    }

    return(files)
  }

  if (all(!grepl("\\.rcdf$", path))) {
    stop(paste0("Not a valid RCDF file: ", paste(path, collapse = ", ")))
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

  # Track registered key labels to avoid issuing duplicate PRAGMA calls
  # for multiple parquet files that share the same AES key.
  registered_keys <- new.env(parent = emptyenv())

  for (pq_file in pq_files) {
    process_parquet_file(conn, pq_file, secret, pk, registered_keys)
  }

  meta$dir_base = ext$dir_base

  return(meta)

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

process_parquet_file <- function(conn, pq_file, secret, pk, registered_keys = NULL) {

  record <- tools::file_path_sans_ext(basename(pq_file))

  # Register the AES key with DuckDB only once per unique key label.
  key_label <- secret$key
  if (is.null(registered_keys) || is.null(registered_keys[[key_label]])) {
    tryCatch(
      DBI::dbExecute(
        conn,
        sprintf(
          "PRAGMA add_parquet_key(%s, %s)",
          sql_literal(conn, secret$key),
          sql_literal(conn, secret$value)
        )
      ),
      error = function(e) stop("Failed to register parquet decryption key.", call. = FALSE)
    )
    if (!is.null(registered_keys)) {
      registered_keys[[key_label]] <- TRUE
    }
  }

  if (!DBI::dbExistsTable(conn, record)) {

    DBI::dbExecute(
      conn,
      sprintf(
        "CREATE TABLE %s AS\n         SELECT * FROM read_parquet(%s,\n         encryption_config = { footer_key: %s });",
        sql_ident(conn, record),
        sql_literal(conn, pq_file),
        sql_literal(conn, secret$key)
      )
    )

    return(invisible())
  }

  record_temp <- paste0(record, "_temp")

  DBI::dbExecute(
    conn,
    sprintf(
      "CREATE TABLE %s AS\n       SELECT * FROM read_parquet(%s,\n       encryption_config = { footer_key: %s });",
      sql_ident(conn, record_temp),
      sql_literal(conn, pq_file),
      sql_literal(conn, secret$key)
    )
  )

  DBI::dbExecute(
    conn,
    sprintf(
      "INSERT INTO %s\n       SELECT * FROM %s;",
      sql_ident(conn, record),
      sql_ident(conn, record_temp)
    )
  )

  DBI::dbExecute(
    conn,
    sprintf(
      "DROP TABLE IF EXISTS %s;",
      sql_ident(conn, record_temp)
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

# Returns each table as a lazy rcdf_tbl_db (DuckDB connection stays open).
collect_tables_lazy <- function(conn, data_dictionary) {

  pq <- rcdf_list()

  for (record in DBI::dbListTables(conn)) {

    lazy_tbl <- dplyr::tbl(conn, record)

    if (!is.null(data_dictionary) && length(data_dictionary) > 0) {
      lazy_tbl <- add_metadata(lazy_tbl, data_dictionary)
    } else {
      class(lazy_tbl) <- c("rcdf_tbl_db", class(lazy_tbl))
    }

    pq[[record]] <- lazy_tbl
  }

  pq
}
