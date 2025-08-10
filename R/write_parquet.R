#' Title
#'
#' @param data
#' @param path
#' @param ...
#' @param encryption_key
#'
#' @return
#' @export
#'
#' @examples
#'

write_parquet <- function(data, path, ..., encryption_key = NULL) {

  aes_key <- encryption_key$aes_key
  aes_iv <- encryption_key$aes_iv

  if(is.null(aes_key) | is.null(aes_iv)) {
    arrow::write_parquet(x = data, sink = path, ...)
  } else {

    pq_name <- "__TEMP_DATA__"
    pq_conn <- DBI::dbConnect(drv = duckdb::duckdb())
    pq_encrypt <- glue::glue("PRAGMA add_parquet_key('{aes_key}', '{aes_iv}')")
    pq_query <- glue::glue("COPY {pq_name} TO '{path}' (ENCRYPTION_CONFIG {{ footer_key: '{aes_key}' }});")

    DBI::dbExecute(conn = pq_conn, statement = pq_encrypt)

    DBI::dbWriteTable(
      conn = pq_conn,
      name = pq_name,
      value = dplyr::collect(data),
      overwrite = T
    )

    DBI::dbExecute(conn = pq_conn, statement = pq_query)
    suppressWarnings(DBI::dbDisconnect(conn = pq_conn, shutdown = TRUE))
  }


}


#' Title
#'
#' @param data
#' @param path
#' @param ...
#' @param parent_dir
#'
#' @return
#' @export
#'
#' @examples

write_rcdf_parquet <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    rcdf::write_parquet(
      data = data[[record]],
      path = file.path(path, glue::glue("{record}.parquet")),
      ...
    )
  }

  list.files(path, pattern = '.parquet', full.names = T)

}
