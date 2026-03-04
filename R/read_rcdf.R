#' Read and decrypt RCDF data
#'
#' This function reads an RCDF file, decrypts its contents using the specified decryption key,
#' and loads it into R as an RCDF object.
#'
#' @param path A string specifying the path to the RCDF archive (zip file). If a directory is provided, all \code{.rcdf} files within that directory will be processed.
#' @param decryption_key The key used to decrypt the RCDF. This can be an RSA or AES key, depending on how the RCDF was encrypted.
#' @param ... Additional parameters passed to other functions, if needed (not yet implemented).
#' @param password A password used for RSA decryption (optional).
#' @param metadata An optional list of metadata object containing data dictionaries, value sets, and primary key constraints for data integrity measure (a \code{data.frame} or \code{tibble} that includes at least two columns: \code{file} and \code{pk_field_name}. This metadata is applied to the data if provided.
#' @param ignore_duplicates A \code{logical} flag. If \code{TRUE}, a warning is issued when duplicates are found, based on the primary key/s defined during creation of RCDF file. If \code{FALSE}, the function stops with an error.
#' @param recursive Logical. If \code{TRUE} and \code{path} is a directory, the function will search recursively for \code{.rcdf} files.
#' @param return_meta Logical. If \code{TRUE}, the metadata will be returned as an attribute of the RCDF object.
#'
#' @return An RCDF object, which is a list of Parquet files (one for each record) along with attached metadata.
#' @export
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')

#' private_key <- file.path(dir, 'sample-private-key-pw.pem')
#' pw <- '1234'
#'
#' \dontrun{
#' rcdf_data <- read_rcdf(
#'   path = rcdf_path,
#'   decryption_key = private_key,
#'   password = pw
#' )
#'
#' rcdf_data
#' }

read_rcdf <- function(
  path,
  ...,
  decryption_key,
  password = NULL,
  metadata = list(),
  ignore_duplicates = TRUE,
  recursive = FALSE,
  return_meta = FALSE
) {

  rcdf_files <- resolve_rcdf_files(path, recursive)

  creds <- normalize_credentials(rcdf_files, decryption_key, password)

  conn <- open_duckdb_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  meta_list <- list()
  data_dictionary <- metadata$dictionary

  for (i in seq_along(rcdf_files)) {

    meta <- process_rcdf_file(
      conn = conn,
      rcdf_file = rcdf_files[i],
      key = creds$keys[i],
      password = creds$passwords[i],
      metadata = metadata,
      ignore_duplicates = ignore_duplicates
    )

    print(names(meta))

    # Collect Metadata
    meta_list$log_id     <- c(meta_list$log_id, meta$log_id)
    meta_list$created_at <- c(meta_list$created_at, meta$created_at)
    meta_list$version    <- c(meta_list$version, meta$version)

    if (!is.null(meta$area_names)) {
      meta_list$area_names <- dplyr::bind_rows(
        meta_list$area_names,
        meta$area_name
      )
    }

    # Dictionary merge
    if (!is.null(meta$dictionary) &&
        is.null(metadata$dictionary)) {

      if (is.null(data_dictionary)) {
        data_dictionary <- meta$dictionary
      } else {
        data_dictionary <- dplyr::bind_rows(
          data_dictionary,
          meta$dictionary
        )
      }
    }
  }

  # Collect Results
  pq <- collect_tables(conn, data_dictionary)

  # Attach Metadata
  if (!is.null(data_dictionary) && return_meta) {
    pq[["__data_dictionary"]] <- data_dictionary
  }

  if (return_meta) {
    attr(pq, "metadata") <- meta_list
  }

  pq

}
