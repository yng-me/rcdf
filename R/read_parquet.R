#' Read Parquet File with Optional Decryption
#'
#' This function reads a Parquet file, optionally decrypting it using the provided decryption key. If no decryption key is provided, it reads the file normally without decryption. It supports reading Parquet files as Arrow tables or regular data frames, depending on the `as_arrow_table` argument.
#'
#' @param path The file path to the Parquet file.
#' @param ... Additional arguments passed to `arrow::open_dataset()` when no decryption key is provided.
#' @param decryption_key A list containing `aes_key` and `aes_iv`. If provided, the Parquet file will be decrypted using these keys. Default is `NULL`.
#' @param as_arrow_table Logical. If `TRUE`, the function will return the result as an Arrow table. If `FALSE`, a regular data frame will be returned. Default is `TRUE`.
#'
#' @return An Arrow table or a data frame, depending on the value of `as_arrow_table`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Without decryption
#' df <- read_parquet("data.parquet")
#'
#' # With decryption
#' decryption_key <- list(aes_key = "your_aes_key", aes_iv = "your_aes_iv")
#' df <- read_parquet("data_encrypted.parquet", decryption_key = decryption_key)
#' }

read_parquet <- function(path, ..., decryption_key = NULL, as_arrow_table = TRUE) {

  aes_key <- decryption_key$aes_key
  aes_iv <- decryption_key$aes_iv

  if(is.null(aes_key) | is.null(aes_iv)) {
    return(arrow::open_dataset(sources = path, ...))
  }

  pq_conn <- DBI::dbConnect(drv = duckdb::duckdb())
  pq_encrypt <- glue::glue("PRAGMA add_parquet_key('{aes_key}', '{aes_iv}')")
  pq_query <- glue::glue("SELECT * FROM read_parquet('{path}', encryption_config = {{ footer_key: '{aes_key}' }});")

  DBI::dbExecute(conn = pq_conn, statement = pq_encrypt)
  df <- DBI::dbGetQuery(conn = pq_conn, statement = pq_query)

  if(as_arrow_table) {
    df <- arrow::arrow_table(df)
  }

  suppressWarnings(DBI::dbDisconnect(conn = pq_conn, shutdown = TRUE))

  return(df)

}
