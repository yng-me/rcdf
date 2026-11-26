#' Write Parquet file with optional encryption
#'
#' Writes a data frame to a Parquet file. When \code{encryption_key} is
#' supplied the file is encrypted with AES using DuckDB's native Parquet
#' encryption support. Without a key the file is written unencrypted, also by
#' DuckDB, using the codec given in \code{compression}.
#'
#' @param data A data frame or tibble to write, or a lazy table on a DuckDB
#'   connection. A lazy table is written with \code{COPY} on its own connection
#'   (\code{conn} is ignored), so its rows never come into R. When encrypting,
#'   this loads DuckDB's crypto support on that connection and registers the
#'   AES key there; the key stays registered until the connection closes.
#' @param path Character string. Destination file path for the Parquet file.
#' @param ... Currently unused.
#' @param encryption_key A raw AES key as a hex string (32, 48, or 64 hex
#'   characters) or a base-64–encoded 256-bit key string. When \code{NULL}
#'   (default) no encryption is applied.
#' @param conn An optional existing DuckDB \code{DBIConnection}. When supplied,
#'   the connection is reused instead of opening a new one, which avoids
#'   per-call setup overhead when writing many tables in a batch. The caller
#'   is responsible for disconnecting the connection. Default is \code{NULL}.
#' @param compression Compression codec for the Parquet file, encrypted or not.
#'   Any codec supported by DuckDB's Parquet writer is valid (e.g.
#'   \code{"zstd"}, \code{"snappy"}, \code{"gzip"}, \code{"lz4"},
#'   \code{"uncompressed"}). Default is \code{"zstd"}.
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

  if (is_duckdb_lazy(data)) {
    return(write_lazy_parquet(data, path, secret, compression = compression))
  }

  # Reuse a caller-supplied connection to amortise setup cost across many tables.
  if (is.null(conn)) {
    conn <- DBI::dbConnect(drv = duckdb::duckdb())
    on.exit(DBI::dbDisconnect(conn = conn, shutdown = TRUE), add = TRUE)
  }
  write_df_parquet(conn, data, path, secret, compression = compression)

}



#' Write RCDF data to Parquet files
#'
#' This function writes an RCDF object (a list of data frames) to multiple Parquet files. Each data frame in the list is written to its corresponding Parquet file in the specified path.
#'
#' @param data A list where each element is a data frame or tibble that will be written to a Parquet file. DuckDB lazy tables are streamed without collecting; see \code{\link{write_parquet}}.
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

  # One shared DuckDB connection for all writes in this batch.
  pq_conn <- DBI::dbConnect(drv = duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn = pq_conn, shutdown = TRUE), add = TRUE)

  written <- character(0)
  write_each_record(data, path, parent_dir, function(df, record, dir) {
    if (!is.null(primary_key)) {
      check_duplicates(df, record, primary_key, ignore_duplicates, conn = pq_conn)
    }
    file <- file.path(dir, paste0(record, ".parquet"))
    write_parquet(data = df, path = file, conn = pq_conn, ...)
    written <<- c(written, file)
  }, keep_lazy = TRUE)

  written
}


check_duplicates <- function(data, record, primary_key, ignore_duplicates, conn) {

  pk_cols <- intersect(pk_columns(primary_key, record), colnames(data))
  if (length(pk_cols) == 0) return(NULL)

  if (is_duckdb_lazy(data)) {
    # Count in the source database; the rows never come into R.
    conn <- dbplyr::remote_con(data)
    from <- sprintf("(%s)", dbplyr::sql_render(data))
  } else {
    # Zero-copy view of only the key columns; GROUP BY in DuckDB is ~6x faster
    # than pasting keys together in R on millions of rows.
    from <- "__PK_CHECK__"
    duckdb::duckdb_register(conn, from, as.data.frame(data[pk_cols]), overwrite = TRUE)
    on.exit(duckdb::duckdb_unregister(conn, from), add = TRUE, after = FALSE)
    from <- sql_ident(conn, from)
  }

  report_duplicates(record, count_duplicate_keys(conn, from, pk_cols), ignore_duplicates)
}
