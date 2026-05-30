#' Write Parquet file with optional encryption
#'
#' Writes a data frame to a Parquet file. When \code{encryption_key} is
#' supplied the file is encrypted with AES using DuckDB's native Parquet
#' encryption support. Without a key the file is written by
#' \code{arrow::write_parquet()} and supports any compression codec
#' understood by the Arrow library.
#'
#' @param data A data frame or tibble to write.
#' @param path Character string. Destination file path for the Parquet file.
#' @param ... Additional arguments passed to \code{arrow::write_parquet()} when
#'   \code{encryption_key} is \code{NULL}.
#' @param encryption_key A raw AES key as a hex string (32, 48, or 64 hex
#'   characters) or a base-64–encoded 256-bit key string. When \code{NULL}
#'   (default) no encryption is applied.
#' @param conn An optional existing DuckDB \code{DBIConnection}. When supplied,
#'   the connection is reused instead of opening a new one, which avoids
#'   per-call setup overhead when writing many tables in a batch. The caller
#'   is responsible for disconnecting the connection. Default is \code{NULL}.
#' @param compression Compression codec to use for unencrypted Parquet files.
#'   Any codec supported by \code{arrow::write_parquet()} is valid (e.g.
#'   \code{"zstd"}, \code{"snappy"}, \code{"gzip"}, \code{"lz4"},
#'   \code{"uncompressed"}). Ignored when \code{encryption_key} is set
#'   (DuckDB chooses the codec). Default is \code{"zstd"}.
#'
#' @return \code{NULL} invisibly. The Parquet file is written to \code{path}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(rcdf)
#'
#' data <- mtcars
#' key <- "rppqM5CuEqotys4wQq/g7xh6wpIjRozcAIbI9sagwKE="
#'
#' temp_dir <- tempdir()
#'
#' # Encrypted write
#' write_parquet(
#'   data = data,
#'   path = file.path(temp_dir, "mtcars.parquet"),
#'   encryption_key = key
#' )
#'
#' # Unencrypted write with gzip compression
#' write_parquet(
#'   data = data,
#'   path = file.path(temp_dir, "mtcars-gz.parquet"),
#'   compression = "gzip"
#' )
#' }
#'

write_parquet <- function(data, path, ..., encryption_key = NULL, conn = NULL, compression = "zstd") {

  secret <- normalize_key_value(encryption_key)

  if (is.null(secret)) {
    pq_conn_w <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(pq_conn_w, shutdown = TRUE), add = TRUE)
    DBI::dbWriteTable(pq_conn_w, "__WRITE_DATA__", data, overwrite = TRUE)
    DBI::dbExecute(
      pq_conn_w,
      sprintf(
        "COPY __WRITE_DATA__ TO '%s' (FORMAT PARQUET, COMPRESSION '%s')",
        gsub("'", "''", path, fixed = TRUE),
        toupper(compression)
      )
    )
    DBI::dbRemoveTable(pq_conn_w, "__WRITE_DATA__")
  } else {

    pq_name <- "__TEMP_DATA__"

    # Use caller-supplied connection to amortise setup cost across many tables;
    # fall back to a local connection when called standalone.
    own_conn <- is.null(conn)
    if (own_conn) {
      conn <- DBI::dbConnect(drv = duckdb::duckdb())
      on.exit(DBI::dbDisconnect(conn = conn, shutdown = TRUE), add = TRUE)
    }

    # Ensure the full write-capable crypto module is loaded on this connection,
    # regardless of whether the connection was created here or supplied by the caller.
    load_duckdb_crypto(conn)

    pq_key <- secret$key

    # Escape single quotes in the file path so it embeds safely in a SQL string literal.
    safe_path <- gsub("'", "''", path, fixed = TRUE)
    pq_query  <- sprintf(
      "COPY %s TO '%s' (ENCRYPTION_CONFIG { footer_key: '%s' });",
      pq_name,
      safe_path,
      pq_key
    )

    # Wrap PRAGMA in tryCatch so the raw AES key value is never echoed in an error message.
    tryCatch(
      DBI::dbExecute(
        conn = conn,
        statement = sprintf("PRAGMA add_parquet_key('%s', '%s')", pq_key, secret$value)
      ),
      error = function(e) stop("Failed to register parquet encryption key.", call. = FALSE)
    )

    DBI::dbWriteTable(conn = conn, name = pq_name, value = data, overwrite = TRUE)
    DBI::dbExecute(conn = conn, statement = pq_query)

    # Always clean up the plaintext temp table, regardless of connection ownership.
    DBI::dbRemoveTable(conn, pq_name)
  }

}



#' Write RCDF data to Parquet files
#'
#' This function writes an RCDF object (a list of data frames) to multiple Parquet files. Each data frame in the list is written to its corresponding Parquet file in the specified path.
#'
#' @param data A list where each element is a data frame or tibble that will be written to a Parquet file.
#' @param path The directory path where the Parquet files will be written.
#' @param ... Additional arguments passed to \code{rcdf::write_parquet()} while writing each Parquet file.
#' @param parent_dir An optional parent directory to be included in the path where the files will be written.
#' @param primary_key A \code{data.frame} or \code{tibble} that includes at least two columns: \code{file} and \code{pk_field_name}.
#' @param ignore_duplicates A \code{logical} flag. If \code{TRUE}, a warning is issued when duplicates are found. If \code{FALSE}, the function stops with an error.
#'
#' @return A character vector of file paths to the written Parquet files.
#' @export
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key-pw.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#' temp_dir <- tempdir()
#'
#' write_rcdf_parquet(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_parquet <- function(data, path, ..., parent_dir = NULL, primary_key = NULL, ignore_duplicates = TRUE) {

  data <- check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)
  records <- records[!grepl('__data_dictionary', records)]

  # Open one shared DuckDB connection for all encrypted writes in this batch.
  pq_conn <- DBI::dbConnect(drv = duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn = pq_conn, shutdown = TRUE), add = TRUE)
  load_duckdb_crypto(pq_conn)

  for (i in seq_along(records)) {

    record_i <- records[i]
    # Collect once so we don't materialise the same lazy table twice.
    data_i <- dplyr::collect(data[[record_i]])

    if (!is.null(primary_key)) {
      check_duplicates(
        data = data_i,
        record = record_i,
        primary_key = primary_key,
        ignore_duplicates = ignore_duplicates
      )
    }

    write_parquet(
      data = data_i,
      path = file.path(path, paste0(record_i, ".parquet")),
      conn = pq_conn,
      ...
    )
  }

  list.files(path, pattern = '.parquet', full.names = TRUE)

}


check_duplicates <- function(data, record, primary_key, ignore_duplicates) {

  pk <- primary_key[primary_key$file == record, "pk_field_name", drop = TRUE]
  if(length(pk) == 0) return(NULL)

  pk_cols <- intersect(trimws(strsplit(pk, ",\\s*")[[1]]), names(data))
  if (length(pk_cols) == 0) return(NULL)

  keys <- do.call(paste, c(data[pk_cols], list(sep = "\001")))
  dup_n <- sum(table(keys) > 1L)

  if(dup_n == 0) return(NULL)

  if_plural <- ""
  if(dup_n > 1) { if_plural <- "s" }

  if(ignore_duplicates) {
    warning(
      sprintf("Detected potential duplicates in `%s` based on provided `primary_key`: %d row%s",
              record, dup_n, if_plural),
      call. = FALSE
    )
  } else {
    stop(
      sprintf("Detected potential duplicates in `%s` based on provided `primary_keys`: %d row%s",
              record, dup_n, if_plural),
      call. = FALSE
    )
  }
}


