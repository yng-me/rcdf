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
    return(arrow::read_parquet(file = path, ...))
  }

  pq_conn <- open_duckdb_connection()
  data <- dplyr::collect(read_parquet_tbl(pq_conn, file = path, decryption_key = decryption_key))
  on.exit(DBI::dbDisconnect(conn = pq_conn, shutdown = TRUE), add = TRUE)

  if(!is.null(metadata)) { data <- add_metadata(data, metadata) }

  if(as_arrow_table) {
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
    table_name <- fs::path_ext_remove(basename(file))
  }

  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "PRAGMA add_parquet_key({secret$key}, {secret$value})",
      .con = conn
    )
  )

  if(!is.null(columns)) {

    q <- glue::glue_sql(
      "CREATE TABLE {`table_name`} AS
      SELECT {`columns`*}
      FROM read_parquet(
        {file},
        encryption_config = {{ footer_key: {secret$key} }}
      );",
      vals = columns,
      .con = conn
    )

  } else {
    q <- glue::glue_sql(
      "CREATE TABLE {`table_name`} AS
      SELECT * FROM read_parquet(
        {file},
        encryption_config = {{ footer_key: {secret$key} }}
      );",
      .con = conn
    )
  }

  DBI::dbExecute(conn, q)

  dplyr::tbl(conn, table_name)

}
