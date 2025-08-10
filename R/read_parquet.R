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
