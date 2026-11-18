#' Write RCDF data to multiple formats
#'
#' Exports RCDF-formatted data to one or more supported open data formats. The function automatically dispatches to the appropriate writer function based on the \code{formats} provided.
#'
#' @param data A named list or RCDF object. Each element should be a table or tibble-like object (typically a \code{dbplyr} or \code{dplyr} table).
#' @param path The target directory where output files should be saved.
#' @param formats A character vector of file formats to export to. Supported formats include: \code{"csv"}, \code{"tsv"}, \code{"json"}, \code{"parquet"}, \code{"xlsx"}, \code{"dta"}, \code{"sav"}, and \code{"sqlite"}.
#' @param ... Additional arguments passed to the respective writer functions.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_csv}  \link[rcdf]{write_rcdf_tsv}  \link[rcdf]{write_rcdf_json}  \link[rcdf]{write_rcdf_xlsx}  \link[rcdf]{write_rcdf_dta}  \link[rcdf]{write_rcdf_sav}  \link[rcdf]{write_rcdf_sqlite}
#'
#' @examples
#' \dontrun{
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key-pw.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#' temp_dir <- tempdir()
#'
#' write_rcdf_as(data = rcdf_data, path = temp_dir, formats = c("csv", "xlsx"))
#'
#' unlink(temp_dir, force = TRUE)
#' }


write_rcdf_as <- function(data, path, formats, ...) {

  valid_formats <- c("csv", "tsv", "json", "parquet", "xlsx", "dta", "sav", "sqlite")
  label_formats <- c("CSV", "TSV", "JSON", "Parquet", "Excel", "Stata", "SPSS", "SQLite")

  # Safe dispatch table — avoids eval(parse()) on user-supplied format names.
  writer_fns <- list(
    csv     = write_rcdf_csv,
    tsv     = write_rcdf_tsv,
    json    = write_rcdf_json,
    parquet = write_rcdf_parquet,
    xlsx    = write_rcdf_xlsx,
    dta     = write_rcdf_dta,
    sav     = write_rcdf_sav,
    sqlite  = write_rcdf_sqlite
  )

  valid_format_args <- which(formats %in% valid_formats)

  if(length(valid_format_args) < length(formats)) {

    n_invalid <- length(formats) - length(valid_format_args)
    n_invalid_s <- ''
    if(n_invalid > 1) { n_invalid_s <- 's' }

    stop(paste0(n_invalid, " invalid format", n_invalid_s, " found."))

  }


  for(i in seq_along(formats)) {

    data_format <- formats[i]
    label_format <- label_formats[which(valid_formats == data_format)]

    writer_fns[[data_format]](
      data = data,
      path = path,
      ...,
      parent_dir = label_format
    )

  }

}



#' Write RCDF data to CSV files
#'
#' Writes each table in the RCDF object as a separate \code{.csv} file.
#'
#' @param data A valid RCDF object.
#' @param path The base output directory.
#' @param ... Additional arguments passed to \code{write.csv()}.
#' @param parent_dir Optional subdirectory under \code{path} to group CSV files.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key-pw.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#' temp_dir <- tempdir()
#'
#' write_rcdf_csv(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_csv <- function(data, path, ..., parent_dir = NULL) {
  write_each_record(data, path, parent_dir, function(df, record, dir) {
    utils::write.csv(x = df, file = file.path(dir, paste0(record, ".csv")), row.names = FALSE)
  })
}


#' Write RCDF data to TSV files
#'
#' Writes each table in the RCDF object as a separate tab-separated \code{.txt} file.
#'
#' @param data A valid RCDF object.
#' @param path The base output directory.
#' @param ... Additional arguments passed to \code{write.table()}.
#' @param parent_dir Optional subdirectory under \code{path} to group TSV files.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key-pw.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#' temp_dir <- tempdir()
#'
#' write_rcdf_tsv(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_tsv <- function(data, path, ..., parent_dir = NULL) {
  write_each_record(data, path, parent_dir, function(df, record, dir) {
    utils::write.table(x = df, file = file.path(dir, paste0(record, ".txt")), sep = "\t", row.names = FALSE)
  })
}


#' Write RCDF data to JSON files
#'
#' Writes each table in the RCDF object as a separate \code{.json} file.
#'
#' @param data A valid RCDF object.
#' @param path The output directory for files.
#' @param ... Additional arguments passed to \code{jsonlite::write_json()}.
#' @param parent_dir Optional subdirectory under \code{path} to group JSON files.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key-pw.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#' temp_dir <- tempdir()
#'
#' write_rcdf_json(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)


write_rcdf_json <- function(data, path, ..., parent_dir = NULL) {
  write_each_record(data, path, parent_dir, function(df, record, dir) {
    jsonlite::write_json(x = df, path = file.path(dir, paste0(record, ".json")), auto_unbox = TRUE, pretty = TRUE)
  })
}


#' Write RCDF data to Excel files
#'
#' Writes each table in the RCDF object as a separate \code{.xlsx} file using the \code{openxlsx} package.
#'
#' @param data A valid RCDF object.
#' @param path The output directory.
#' @param ... Additional arguments passed to \code{openxlsx::write.xlsx()}.
#' @param parent_dir Optional subdirectory under \code{path} to group Excel files.
#' @param as_single_file Whether to export all records (items in the RCDF list) in a single file where each item will be written per sheet in the workbook.
#' @param file_name File name to assign when \code{as_single_file} is set to \code{TRUE}.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key-pw.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#' temp_dir <- tempdir()
#'
#' write_rcdf_xlsx(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_xlsx <- function(data, path, ..., parent_dir = NULL, as_single_file = FALSE, file_name = NULL) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop('Package "openxlsx" is required for Excel export. Install it with: install.packages("openxlsx")', call. = FALSE)
  }

  if(!as_single_file) {
    write_each_record(data, path, parent_dir, function(df, record, dir) {
      openxlsx::write.xlsx(x = df, file = file.path(dir, paste0(record, ".xlsx")), ...)
    })
    return(invisible())
  }

  wb <- openxlsx::createWorkbook()
  dir <- dir_create_new(path, parent_dir)
  write_each_record(data, dir, NULL, function(df, record, dir) {
    # Excel caps sheet names at 31 characters; keep truncated names unique.
    sheet <- substr(record, 1, 31)
    i <- 1
    while (tolower(sheet) %in% tolower(names(wb))) {
      sheet <- paste0(substr(record, 1, 28), "_", i)
      i <- i + 1
    }
    openxlsx::addWorksheet(wb, sheetName = sheet)
    openxlsx::writeData(wb, x = df, sheet = sheet)
  })

  file <- "Book 1.xlsx"
  if(!is.null(file_name)) {
    file <- if (grepl('\\.xlsx$', file_name)) file_name else paste0(file_name, ".xlsx")
  }

  openxlsx::saveWorkbook(wb, file = file.path(dir, file), ...)
}


#' Write RCDF data to Stata \code{.dta} files
#'
#' Writes each table in the RCDF object to a \code{.dta} file for use in Stata.
#'
#' @param data A valid RCDF object.
#' @param path Output directory for files.
#' @param ... Additional arguments passed to \code{haven::write.dta()}.
#' @param parent_dir Optional subdirectory under \code{path} to group Stata files.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key-pw.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#' temp_dir <- tempdir()
#'
#' write_rcdf_dta(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_dta <- function(data, path, ..., parent_dir = NULL) {
  write_each_record(data, path, parent_dir, function(df, record, dir) {
    haven::write_dta(data = df, path = file.path(dir, paste0(record, ".dta")), ...)
  })
}


#' Write RCDF data to SPSS \code{.sav} files
#'
#' Writes each table in the RCDF object to a \code{.sav} file using the \code{haven} package for compatibility with SPSS.
#'
#' @param data A valid RCDF object.
#' @param path Output directory for files.
#' @param ... Additional arguments passed to \code{haven::write_sav()}.
#' @param parent_dir Optional subdirectory under \code{path} to group SPSS files.
#'
#' @return Invisibly returns \code{NULL}. Files are written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key-pw.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#' temp_dir <- tempdir()
#'
#' write_rcdf_sav(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)


write_rcdf_sav <- function(data, path, ..., parent_dir = NULL) {
  write_each_record(data, path, parent_dir, function(df, record, dir) {
    haven::write_sav(data = df, path = file.path(dir, paste0(record, ".sav")), ...)
  })
}


#' Write RCDF data to a SQLite database
#'
#' Writes all tables in the RCDF object to a single SQLite database file.
#'
#' @param data A valid RCDF object.
#' @param path Output directory for the database file.
#' @param db_name Name of the SQLite database file (without extension).
#' @param ... Additional arguments passed to \code{DBI::dbWriteTable()}.
#' @param parent_dir Optional subdirectory under `path` to store the SQLite file.
#'
#' @return Invisibly returns \code{NULL}. A \code{.db} file is written to disk.
#' @export
#'
#' @seealso \link[rcdf]{write_rcdf_as}
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key-pw.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#' temp_dir <- tempdir()
#'
#' write_rcdf_sqlite(data = rcdf_data, path = temp_dir)
#'
#' unlink(temp_dir, force = TRUE)

write_rcdf_sqlite <- function(data, path, db_name = "cbms_data", ..., parent_dir = NULL) {

  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop('Package "RSQLite" is required for SQLite export. Install it with: install.packages("RSQLite")', call. = FALSE)
  }

  dir <- dir_create_new(path, parent_dir)
  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, paste0(db_name, ".db")))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  write_each_record(data, dir, NULL, function(df, record, dir) {
    DBI::dbWriteTable(conn = conn, name = record, value = df, ...)
  })
}


# Shared record loop for every export writer: collects each record once and
# hands it to `write_fn(df, record, dir)`. `__data_dictionary` is metadata, not a record.
write_each_record <- function(data, path, parent_dir, write_fn) {
  data <- check_if_rcdf(data)
  dir <- dir_create_new(path, parent_dir)
  for (record in setdiff(names(data), "__data_dictionary")) {
    write_fn(dplyr::collect(data[[record]]), record, dir)
  }
  invisible()
}

