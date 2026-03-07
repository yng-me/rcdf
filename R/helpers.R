#' @importFrom dplyr collect
NULL

#' Collect
#'
#' @param data A lazy data frame (e.g. from dbplyr or dtplyr) from database connection.
#' @param ... Optional arguments
#'
#' @returns A data frame
#' @export
#'
collect <- function(data, ...) { UseMethod('collect') }


#' @export
collect.rcdf_tbl_db <- function(data, ...) {

  metadata <- attributes(data)$metadata
  attr(data, 'metadata') <- NULL

  add_metadata(dplyr::collect(data), metadata)
}



#' Encrypt string using RSA
#'
#' @param x A character of length 1
#' @param pub_key A public key object or .pem file
#'
#' @returns Encrypted base64-encoded string
#' @export
#'
#' @examples
encrypt_string <- function(x, pub_key) {
  x |>
    serialize(connection = NULL) |>
    openssl::rsa_encrypt(pubkey = pub_key) |>
    openssl::base64_encode()
}


#' Decrypt string using RSA
#'
#' @param x Encrypted base64-encoded string
#' @param prv_key A private key object or .pem file
#' @param password Passwor of the private key
#'
#' @returns A decrpyted character of length 1
#' @export
#'
#' @examples
decrypt_string <- function(x, prv_key, password = NULL) {

  if(!is.null(password)) {
    key <- openssl::read_key(file = prv_key, password = password)
  } else {
    key <- openssl::read_key(file = prv_key)
  }

  x |>
    openssl::base64_decode() |>
    openssl::rsa_decrypt(key) |>
    unserialize()
}



