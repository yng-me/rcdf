#' Title
#'
#' @param path
#' @param decryption_key
#' @param ...
#' @param password
#' @param metadata
#'
#' @return
#' @export
#'
#' @examples
#'

read_rcdf <- function(path, decryption_key, ..., password = NULL, metadata = NULL) {

  meta <- extract_rcdf(path)
  key <- decrypt_key(data = meta, key = decryption_key, password = password)

  pq_files <- list.files(
    path = file.path(meta$dir, 'lineage'),
    pattern = '\\.parquet',
    full.names = T
  )

  pq <- list()

  for(i in seq_along(pq_files)) {

    pq_file <- pq_files[i]
    record <- fs::path_ext_remove(basename(pq_file))

    pq_temp <- rcdf::read_parquet(
      path = pq_file,
      decryption_key = list(
        aes_key = key$aes_key,
        aes_iv = key$aes_iv
      ),
      as_arrow_table = F
    )

    if(!is.null(metadata)) {

      if(!is.null(metadata$data_dictionary) & !is.null(metadata$valueset)) {

        pq_temp <- add_metadata(
          .data = pq_temp,
          .dictionary = metadata$data_dictionary[[meta$input_data]],
          .valueset = metadata$valueset
        )
      }
    }

    pq[[record]] <- arrow::arrow_table(pq_temp)

  }

  attr(pq, 'metadata') <- meta

  as_rcdf(pq)

}



extract_rcdf <- function(path, meta_only = FALSE) {

  temp_dir <-  tempdir()
  temp_dir_extract <- stringr::str_split_1(temp_dir, '\\/')[1:(length(stringr::str_split_1(temp_dir, '\\/')) - 1)]

  temp_dir_rcdf <- paste0(paste0(temp_dir_extract, collapse = '/'), '/')

  temp_dir_rcdf <- list.files(
    path = temp_dir_rcdf,
    pattern = '__rcdf_temp__',
    include.dirs = T,
    recursive = T,
    full.names = T
  )

  if(length(temp_dir_rcdf) > 0) {
    for(i in seq_along(temp_dir_rcdf)) {
      unlink(temp_dir_rcdf[i], recursive = T, force = T)
    }
  }

  temp_dir_to <- file.path(temp_dir, '__rcdf_temp__', fs::path_ext_remove(basename(path)))

  unzip(path, exdir = temp_dir_to, junkpaths = T)
  if(!meta_only) {
    unzip(file.path(temp_dir_to, 'lineage.zip'), exdir = temp_dir_to)
  }

  unlink(file.path(temp_dir_to, 'lineage.zip'), recursive = T, force = T)

  meta <- jsonlite::read_json(file.path(temp_dir_to, 'metadata.json'), simplifyVector = T)

  meta$dir <- temp_dir_to
  meta

}

decrypt_key <- function(data, key, password = NULL) {

  if(inherits(key, 'rsa')) {

    meta <- extract_key(data)

    aes_key <- openssl::base64_decode(meta$key)
      openssl::rsa_decrypt(key, password = password) |>
      unserialize()

    aes_iv <- openssl::base64_decode(meta$iv)
      openssl::rsa_decrypt(key, password = password) |>
      unserialize()

  } else if (inherits(key, 'character')) {

    if(grepl('.pem$', key)) {

      aes_key <- decrypt_info_rsa(data$key_admin, prv_key = key, password = password)
      aes_iv <- decrypt_info_rsa(data$iv_admin, prv_key = key, password = password)

    } else {

      aes_key <- decrypt_info_aes(data$key_app, key = list(aes_key = key))
      aes_iv <- decrypt_info_aes(data$iv_app, key = list(aes_key = key))

    }

  }

  return(
    list(
      aes_key = aes_key,
      aes_iv = aes_iv
    )
  )


}
