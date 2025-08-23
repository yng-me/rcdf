#' Write data to RCDF format
#'
#' This function writes data to an RCDF (Reusable Data Container Format) archive. It encrypts the data using AES, generates metadata,
#' and then creates a zip archive containing both the encrypted Parquet files and metadata. The function supports the inclusion of
#' metadata such as system information and encryption keys.
#'
#' @param data A list of data frames or tables to be written to RCDF format. Each element of the list represents a record.
#' @param path The path where the RCDF file will be written. The file will be saved with a `.rcdf` extension if not already specified.
#' @param pub_key The public RSA key used to encrypt the AES encryption keys.
#' @param ... Additional arguments passed to helper functions if needed.
#' @param metadata A list of metadata to be included in the RCDF file. Can contain system information or other relevant details.
#'
#' @return NULL. The function writes the data to a `.rcdf` file at the specified path.
#' @export
#'
#' @examples
#' # Example usage of writing an RCDF file
#'
#' rcdf_data <- rcdf_list()
#' rcdf_data$mtcars <- mtcars
#'
#' dir <- system.file("extdata", package = "rcdf")
#'
#' temp_dir <- tempdir()
#'
#' write_rcdf(
#'   data = rcdf_data,
#'   path = file.path(temp_dir, "mtcars.rcdf"),
#'   pub_key = file.path(dir, 'sample-public-key.pem')
#' )
#'
#' write_rcdf(
#'   data = rcdf_data,
#'   path = file.path(temp_dir, "mtcars-pw.rcdf"),
#'   pub_key = file.path(dir, 'sample-public-key-pw.pem')
#' )
#'
#' unlink(file.path(temp_dir, "mtcars.rcdf"), force = TRUE)
#' unlink(file.path(temp_dir, "mtcars-pw.rcdf"), force = TRUE)


write_rcdf <- function(data, path, pub_key, ..., metadata = list()) {

  dir_temp <- tempdir()

  key_uuid <- uuid::UUIDgenerate()
  key_pub <-  openssl::read_pubkey(pub_key)
  key <- generate_aes_key(key_uuid)

  pq_files <- write_rcdf_parquet(
    data = data,
    path = dir_temp,
    encryption_key = key,
    parent_dir = 'lineage'
  )

  meta <- list(
    log_id = key_uuid,
    key_app = encrypt_info_rsa(key$aes_key, pub_key = pub_key),
    iv_app = encrypt_info_rsa(key$aes_iv, pub_key = pub_key),
    key_admin = encrypt_info_rsa(key$aes_key, pub_key = pub_key),
    iv_admin = encrypt_info_rsa(key$aes_iv, pub_key = pub_key),
    pc_os = get_pc_metadata('pc_os'),
    pc_os_release_date = get_pc_metadata('pc_os_release_date'),
    pc_os_version = get_pc_metadata('pc_os_version'),
    pc_hardware = get_pc_metadata('pc_hardware'),
    created_at = stringr::str_sub(Sys.time(), 1, 19),
    version = 1,
    checksum = pq_files |>
      tools::md5sum() |>
      dplyr::as_tibble(rownames = 'file') |>
      dplyr::mutate(file = basename(file))
  )


  dir_zip <- fs::dir_create(dir_temp, key_uuid)

  jsonlite::write_json(
    x =  meta,
    path = file.path(dir_zip, 'metadata.json'),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  zip::zip(
    zipfile = file.path(dir_zip, 'lineage.zip'),
    files = file.path(dir_temp, 'lineage'),
    include_directories = FALSE,
    mode = "cherry-pick"
  )

  if(!grepl('.rcdf$', path)) { path <- paste0(path, '.rcdf') }

  zip::zip(
    zipfile = path,
    files = dir_zip,
    include_directories = F,
    mode = "cherry-pick"
  )

  unlink(dir_zip, force = TRUE, recursive = TRUE)
  unlink(file.path(dir_temp, '__rcdf_temp__'), force = TRUE, recursive = TRUE)

}
