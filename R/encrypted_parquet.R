# Encrypted Parquet module: the only place that talks to DuckDB's Parquet
# encryption. `secret` is the list(key = <label>, value = <AES key>) produced by
# normalize_key_value() or decrypt_key(). Key material never reaches an error message.

register_parquet_key <- function(conn, secret) {

  tryCatch(
    DBI::dbExecute(
      conn,
      sprintf(
        "PRAGMA add_parquet_key(%s, %s)",
        sql_literal(conn, secret$key),
        sql_literal(conn, secret$value)
      )
    ),
    error = function(e) stop("Failed to register parquet encryption key.", call. = FALSE)
  )

  invisible()
}

# Create `table` from one or more encrypted Parquet files. `key_labels` (one per
# file, recycled) are labels registered with register_parquet_key(). Files are
# combined by column name, so their column order may differ. `view = TRUE`
# reads the files on demand instead of copying them into DuckDB; the files must
# then outlive every query on the view.
load_encrypted_parquet <- function(conn, table, files, key_labels, columns = NULL, view = FALSE) {

  cols <- "*"
  if (!is.null(columns)) {
    cols <- paste(vapply(columns, sql_ident, character(1), conn = conn), collapse = ", ")
  }

  selects <- sprintf(
    "SELECT %s FROM read_parquet(%s, encryption_config = { footer_key: %s })",
    cols,
    vapply(files, sql_literal, character(1), conn = conn),
    vapply(rep_len(key_labels, length(files)), sql_literal, character(1), conn = conn)
  )

  DBI::dbExecute(
    conn,
    sprintf(
      "CREATE %s %s AS %s",
      if (view) "VIEW" else "TABLE",
      sql_ident(conn, table),
      paste(selects, collapse = " UNION ALL BY NAME ")
    )
  )

  invisible()
}

write_encrypted_parquet <- function(conn, data, path, secret, compression = "zstd") {

  load_duckdb_crypto(conn)
  register_parquet_key(conn, secret)

  tmp <- "__TEMP_DATA__"
  DBI::dbWriteTable(conn, tmp, data, overwrite = TRUE)
  # Always drop the plaintext temp table, even if COPY fails.
  on.exit(DBI::dbRemoveTable(conn, tmp), add = TRUE)

  DBI::dbExecute(
    conn,
    sprintf(
      "COPY %s TO %s (ENCRYPTION_CONFIG { footer_key: %s }, COMPRESSION %s)",
      sql_ident(conn, tmp),
      sql_literal(conn, path),
      sql_literal(conn, secret$key),
      sql_literal(conn, toupper(compression))
    )
  )

  invisible()
}

# Load the full crypto module needed for AES-encrypted Parquet writes.
# Tries httpfs first (OpenSSL-backed); falls back to force_mbedtls_unsafe
# (DuckDB's built-in mbedtls) so that writes succeed even when httpfs is
# not installed in the current DuckDB distribution.
load_duckdb_crypto <- function(conn) {
  loaded <- tryCatch({
    DBI::dbExecute(conn, "LOAD httpfs")
    TRUE
  }, error = function(e) FALSE)

  if (!loaded) {
    tryCatch(
      DBI::dbExecute(conn, "SET force_mbedtls_unsafe = 'true'"),
      error = function(e) NULL
    )
  }

  invisible(NULL)
}
