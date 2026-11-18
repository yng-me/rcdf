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
#' @param lazy Logical. If \code{TRUE}, each dataset is returned as a lazy \code{rcdf_tbl_db} DuckDB-backed table instead of being collected into memory. The underlying DuckDB connection is kept alive; call \code{collect()} on each element when you are ready to materialise. When you are done, close it with \code{DBI::dbDisconnect(dbplyr::remote_con(x[[1]]), shutdown = TRUE)}; otherwise it stays open (holding the decrypted data in memory) until R garbage-collects it. Default is \code{FALSE}.
#' @param n_threads Integer or \code{NULL}. Number of DuckDB threads to use for reading. \code{NULL} (default) lets DuckDB choose based on available cores.
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
  return_meta = FALSE,
  lazy = FALSE,
  n_threads = NULL
) {

  rcdf_files <- resolve_rcdf_files(path, recursive)

  creds <- normalize_credentials(rcdf_files, decryption_key, password)

  conn <- open_duckdb_connection(n_threads = n_threads)
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  # Extracted parquet files live until read_rcdf() returns.
  cleanups <- list()
  run_cleanups <- function() for (f in cleanups) f()
  on.exit(run_cleanups(), add = TRUE)

  meta_list <- list()
  # One row per parquet file: its record and the key label of its archive.
  sources <- data.frame(record = character(0), file = character(0), key_label = character(0))

  for (i in seq_along(rcdf_files)) {

    key_label <- paste0("rcdf_key_", i)
    src <- open_rcdf_file(
      conn = conn,
      rcdf_file = rcdf_files[i],
      key = creds$keys[[i]],
      password = creds$passwords[i],
      key_label = key_label
    )
    cleanups[[i]] <- src$cleanup
    sources <- rbind(sources, data.frame(
      record = rcdf_record_name(src$files), file = src$files, key_label = rep(key_label, length(src$files))
    ))

    meta_list <- combine_rcdf_meta(meta_list, src$meta)
  }

  # A dictionary passed by the caller replaces the ones stored in the archives.
  data_dictionary <- metadata$dictionary
  if (is.null(data_dictionary)) data_dictionary <- meta_list$dictionary
  stored_pk <- meta_list$primary_key
  meta_list$dictionary <- NULL
  meta_list$primary_key <- NULL

  # One table per record; same-named records across files are combined.
  # Eager reads use views so data goes from parquet straight into R (one copy,
  # not two). Lazy reads copy into DuckDB, since the files are deleted on return.
  for (record in unique(sources$record)) {
    hit <- sources$record == record
    load_encrypted_parquet(conn, record, sources$file[hit], sources$key_label[hit], view = !lazy)
  }

  # Checked after all files load, so duplicates across files are caught too.
  pk <- resolve_primary_key(metadata, stored_pk)
  if (!is.null(pk)) check_duplicate_keys(conn, pk, ignore_duplicates)

  if (lazy) {
    # Return lazy DuckDB-backed tables. The connection is kept open; the
    # caller is responsible for collecting when ready.
    run_cleanups()
    on.exit(NULL, add = FALSE)  # cancel the auto-disconnect
    pq <- collect_tables_lazy(conn, data_dictionary)
  } else {
    pq <- collect_tables(conn, data_dictionary)
  }

  # Attach Metadata
  if (!is.null(data_dictionary) && return_meta) {
    pq[["__data_dictionary"]] <- data_dictionary
  }

  if (return_meta) {
    attr(pq, "metadata") <- meta_list
  }

  pq

}
