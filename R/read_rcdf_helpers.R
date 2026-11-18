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

# Returns keys as a list (index with [[i]]) so key objects keep their class.
normalize_credentials <- function(rcdf_files, decryption_key, password) {

  if (inherits(decryption_key, "key")) decryption_key <- list(decryption_key)

  lf <- length(rcdf_files)
  ld <- length(decryption_key)
  lp <- length(password)

  if (!(ld %in% c(1, lf)) || !(lp %in% c(0, 1, lf))) {
    stop("Mismatched number of decryption keys provided.")
  }

  list(
    keys = rep_len(as.list(decryption_key), lf),
    passwords = if (lp == 1) rep(password, lf) else password
  )
}

# Extract one archive, unlock its AES key and register it on `conn` under
# `key_label` (each archive has its own key, so each needs its own label).
# Returns list(meta, files, cleanup); the caller runs cleanup() once the
# parquet files are no longer needed.
open_rcdf_file <- function(conn, rcdf_file, key, password, key_label) {

  ext <- extract_rcdf(rcdf_file)
  ok <- FALSE
  on.exit(if (!ok) ext$cleanup(), add = TRUE)

  secret <- decrypt_key(ext$meta, key, password)

  verify_rcdf_checksum(ext$files, ext$meta$checksum)
  register_parquet_key(conn, list(key = key_label, value = secret$value))

  ok <- TRUE
  list(meta = ext$meta, files = ext$files, cleanup = ext$cleanup)
}

# Primary key to check on read: the caller's, else the keys stored in any of
# the archives (`stored`, collected by combine_rcdf_meta()). NULL if none.
resolve_primary_key <- function(metadata, stored) {

  pk <- metadata$primary_key
  if (is.null(pk)) pk <- metadata$primary_keys
  if (is.null(pk)) pk <- stored
  if (is.null(pk) || nrow(pk) == 0) return(NULL)

  pk

}

# Warns (ignore_duplicates = TRUE) or stops if any loaded table has duplicate
# primary keys. `pk$file` may be a record name or "<record>.parquet" (the form
# stored in metadata checksums).
check_duplicate_keys <- function(conn, pk, ignore_duplicates) {

  for (record in DBI::dbListTables(conn)) {

    cols <- intersect(pk_columns(pk, record), DBI::dbListFields(conn, record))
    if (length(cols) == 0) next

    report_duplicates(record, count_duplicate_keys(conn, record, cols), ignore_duplicates)
  }
}

# Number of distinct key values that occur more than once in `table`.
count_duplicate_keys <- function(conn, table, cols) {
  col_sql <- paste(vapply(cols, sql_ident, character(1), conn = conn), collapse = ", ")
  DBI::dbGetQuery(conn, sprintf(
    "SELECT count(*) AS n FROM (SELECT 1 FROM %s GROUP BY %s HAVING count(*) > 1)",
    sql_ident(conn, table), col_sql
  ))$n
}

report_duplicates <- function(record, dup_n, ignore_duplicates) {
  if (dup_n == 0) return(invisible())
  msg <- sprintf("Detected potential duplicates in `%s` based on provided `primary_keys`: %d row%s",
                 record, dup_n, if (dup_n > 1) "s" else "")
  if (ignore_duplicates) warning(msg, call. = FALSE) else stop(msg, call. = FALSE)
}

collect_tables <- function(conn, data_dictionary) {

  pq <- rcdf_list()

  for (record in DBI::dbListTables(conn)) {

    df <- dplyr::collect(dplyr::tbl(conn, record))

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
    # Class first: add_metadata() only stores the dictionary on rcdf_tbl_db objects.
    class(lazy_tbl) <- c("rcdf_tbl_db", class(lazy_tbl))

    if (!is.null(data_dictionary) && length(data_dictionary) > 0) {
      lazy_tbl <- add_metadata(lazy_tbl, data_dictionary)
    }

    pq[[record]] <- lazy_tbl
  }

  pq
}
