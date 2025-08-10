#' Title
#'
#' @param data
#' @param path
#' @param pub_key
#' @param ...
#' @param metadata
#'
#' @return
#' @export
#'
#' @examples
#'

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
    pretty = T,
    auto_unbox = T
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

  unlink(dir_zip, force = T, recursive = T)
  unlink(file.path(dir_temp, '__rcdf_temp__'), force = T, recursive = T)

}
