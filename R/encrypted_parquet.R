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

# Writes a data frame to Parquet, encrypted when `secret` is given. The frame is
# registered as a view (no copy), so peak memory stays at one copy of the data.
write_df_parquet <- function(conn, data, path, secret, compression = "zstd") {

  tmp <- "__TEMP_DATA__"
  duckdb::duckdb_register(conn, tmp, as.data.frame(data), overwrite = TRUE)
  on.exit(duckdb::duckdb_unregister(conn, tmp), add = TRUE)

  copy_to_parquet(conn, sql_ident(conn, tmp), path, secret, compression)
}

# COPY `source` (a quoted table name or parenthesised query) to a Parquet file.
copy_to_parquet <- function(conn, source, path, secret, compression) {

  opts <- sprintf("FORMAT PARQUET, COMPRESSION %s", sql_literal(conn, toupper(compression)))

  if (!is.null(secret)) {
    load_duckdb_crypto(conn)
    register_parquet_key(conn, secret)
    opts <- sprintf("%s, ENCRYPTION_CONFIG { footer_key: %s }", opts, sql_literal(conn, secret$key))
  }

  DBI::dbExecute(conn, sprintf("COPY %s TO %s (%s)", source, sql_literal(conn, path), opts))

  invisible()
}

# A dbplyr lazy table on a DuckDB connection.
is_duckdb_lazy <- function(x) {
  inherits(x, "tbl_lazy") && inherits(dbplyr::remote_con(x), "duckdb_connection")
}

# Streams a DuckDB lazy table to Parquet with COPY on its own connection, so the
# rows never come into R. When encrypting, the key stays registered on that
# connection until it closes (DuckDB cannot unregister one).
write_lazy_parquet <- function(data, path, secret, compression = "zstd") {

  # Random label so we never replace a key the caller registered themselves.
  if (!is.null(secret)) secret$key <- paste0("rcdf_", generate_uuid())

  copy_to_parquet(dbplyr::remote_con(data), sprintf("(%s)", dbplyr::sql_render(data)), path, secret, compression)
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
