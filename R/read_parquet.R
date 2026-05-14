#' Read Parquet file with optional decryption
#'
#' This function reads a Parquet file, optionally decrypting it using the provided decryption key. If no decryption key is provided, it reads the file normally without decryption. It supports reading Parquet files as Arrow tables or regular data frames, depending on the \code{as_arrow_table} argument.
#'
#' @param path The file path to the Parquet file.
#' @param ... Additional arguments passed to \code{arrow::open_dataset()} when no decryption key is provided.
#' @param decryption_key A list containing \code{aes_key} and \code{aes_iv}. If provided, the Parquet file will be decrypted using these keys. Default is `NULL`.
#' @param as_arrow_table Logical. If \code{TRUE}, the function will return the result as an Arrow table. If \code{FALSE}, a regular data frame will be returned. Default is \code{FALSE}.
#' @param metadata Optional metadata (e.g., a data dictionary) to be applied to the resulting data.
#'
#' @return An Arrow table or a data frame, depending on the value of \code{as_arrow_table}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Using sample Parquet files from `mtcars` dataset
#' dir <- system.file("extdata", package = "rcdf")
#'
#' # Not encrypted
#' read_parquet(file.path(dir, "mtcars.parquet"))
#'
#' # Encrypted
#' read_parquet(
#'   file.path(dir, "mtcars-encrypted.parquet"),
#'   decryption_key = 'rppqM5CuEqotys4wQq/g7xh6wpIjRozcAIbI9sagwKE='
#' )
#' }

read_parquet <- function(path, ..., decryption_key = NULL, as_arrow_table = FALSE, metadata = NULL) {

  if(is.null(decryption_key)) {
    pq_conn_r <- open_duckdb_connection()
    on.exit(DBI::dbDisconnect(pq_conn_r, shutdown = TRUE), add = TRUE)
    data <- DBI::dbGetQuery(
      pq_conn_r,
      sprintf("SELECT * FROM read_parquet(%s)", sql_literal(pq_conn_r, path))
    )
    if (!is.null(metadata)) data <- add_metadata(data, metadata)
    if (as_arrow_table) {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop('Package "arrow" is required for as_arrow_table = TRUE. Install it with: install.packages("arrow")', call. = FALSE)
      }
      data <- arrow::arrow_table(data)
    }
    return(data)
  }

  pq_conn <- open_duckdb_connection()
  on.exit(DBI::dbDisconnect(conn = pq_conn, shutdown = TRUE), add = TRUE)
  data <- dplyr::collect(read_parquet_tbl(pq_conn, file = path, decryption_key = decryption_key))

  if(!is.null(metadata)) { data <- add_metadata(data, metadata) }

  if(as_arrow_table) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop('Package "arrow" is required for as_arrow_table = TRUE. Install it with: install.packages("arrow")', call. = FALSE)
    }
    data <- arrow::arrow_table(data)
  }

  return(data)

}


#' Read Parquet file as database
#'
#' This function reads a Parquet file, optionally decrypting it using the provided decryption key. If no decryption key is provided, it reads the file normally without decryption. It supports reading Parquet files as Arrow tables or regular data frames, depending on the \code{as_arrow_table} argument.
#'
#' @param conn A DuckDB connection.
#' @param file The file path to the Parquet file.
#' @param decryption_key A list containing \code{aes_key} and \code{aes_iv}. If provided, the Parquet file will be decrypted using these keys. Default is `NULL`.
#' @param table_name Database table name. If \code{NULL}, file name will be used as table name.
#' @param columns A character vector matching the column names available in the Parquet file.
#'
#' @return Lazy table from DuckDB connection
#' @export
#'
#' @examples
#' \dontrun{
#' # Using sample Parquet files from `mtcars` dataset
#' dir <- system.file("extdata", package = "rcdf")

#'
#' # Encrypted
#' read_parquet_tbl(
#'   file.path(dir, "mtcars-encrypted.parquet"),
#'   decryption_key = 'rppqM5CuEqotys4wQq/g7xh6wpIjRozcAIbI9sagwKE='
#' )
#' }

read_parquet_tbl <- function(conn, file, decryption_key, table_name = NULL, columns = NULL) {

  secret <- normalize_key_value(decryption_key)

  if(is.null(table_name)) {
    table_name <- tools::file_path_sans_ext(basename(file))
  }

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

  if(!is.null(columns)) {

    col_idents <- paste(vapply(columns, function(c) sql_ident(conn, c), character(1)), collapse = ", ")
    q <- sprintf(
      "CREATE TABLE %s AS\n      SELECT %s\n      FROM read_parquet(\n        %s,\n        encryption_config = { footer_key: %s }\n      );",
      sql_ident(conn, table_name),
      col_idents,
      sql_literal(conn, file),
      sql_literal(conn, secret$key)
    )

  } else {
    q <- sprintf(
      "CREATE TABLE %s AS\n      SELECT * FROM read_parquet(\n        %s,\n        encryption_config = { footer_key: %s }\n      );",
      sql_ident(conn, table_name),
      sql_literal(conn, file),
      sql_literal(conn, secret$key)
    )
  }

  DBI::dbExecute(conn, q)

  dplyr::tbl(conn, table_name)

}
