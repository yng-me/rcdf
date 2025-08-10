#' Title
#'
#' @param data
#' @param path
#' @param formats
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

write_rcdf_as <- function(data, path, formats, ...) {

  # TODO:
  # - gheet format
  # - othe db formats

  valid_formats <- c("csv", "tsv", "json", "parquet", "xlsx", "dta", "sav", "sqlite")
  label_formats <- c("CSV", "TSV", "JSON", "Parquet", "Excel", "Stata", "SPSS", "SQLite")

  valid_format_args <- which(formats %in% valid_formats)

  if(length(valid_format_args) < length(formats)) {

    n_invalid <- length(formats) - length(valid_format_args)
    n_invalid_s <- ''
    if(n_invalid > 1) { n_invalid_s <- 's' }

    stop(glue::glue('{n_invalid} invalid format{n_invalid_s} found.'))

  }


  for(i in seq_along(formats)) {

    data_format <- formats[i]
    label_format <- label_formats[which(valid_formats == data_format)]

    cli::cli_alert_info('Writing {data_format}')

    write_rcdf_fn <- eval(parse(text = glue::glue("write_rcdf_{data_format}")))

    write_rcdf_fn(
      data = data,
      path = path,
      ...,
      parent_dir = label_format
    )

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
#'

write_rcdf_csv <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    write.csv(
      x = dplyr::collect(data[[record]]),
      file = file.path(path, glue::glue("{record}.csv"))
    )
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
#'

write_rcdf_tsv <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    write.table(
      x = dplyr::collect(data[[record]]),
      file = file.path(path, glue::glue("{record}.txt")),
      sep = "\t"
    )
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

write_rcdf_json <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    jsonlite::write_json(
      x = dplyr::collect(data[[record]]),
      path = file.path(path, glue::glue("{record}.json")),
      auto_unbox = TRUE,
      pretty = TRUE
    )
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
#'

write_rcdf_xlsx <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    openxlsx::write.xlsx(
      x = dplyr::collect(data[[record]]),
      file = file.path(path, glue::glue("{record}.xlsx")),
      ...
    )
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
#'

write_rcdf_dta <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    foreign::write.dta(
      dataframe = dplyr::collect(data[[record]]),
      file = file.path(path, glue::glue("{record}.dta")),
      ...
    )
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
#'

write_rcdf_sav <- function(data, path, ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    haven::write_sav(
      data = dplyr::collect(data[[record]]),
      path = file.path(path, glue::glue("{record}.sav")),
      ...
    )
  }

}


#' Title
#'
#' @param data
#' @param path
#' @param db_name
#' @param ...
#' @param parent_dir
#'
#' @return
#' @export
#'
#' @examples
#'

write_rcdf_sqlite <- function(data, path, db_name = "cbms_data", ..., parent_dir = NULL) {

  check_if_rcdf(data)
  path <- dir_create_new(path, parent_dir)

  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, glue::glue("{db_name}.db")))

  records <- names(data)

  for(i in seq_along(records)) {

    record <- records[i]

    DBI::dbWriteTable(
      conn = conn,
      name = record,
      value = dplyr::collect(data[[record]]),
      ...
    )

  }

}
