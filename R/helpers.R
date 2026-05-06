#' @importFrom dplyr collect
NULL

#' Collect a lazy RCDF table into a data frame
#'
#' Materialises a lazy \code{rcdf_tbl_db} DuckDB-backed table into a regular
#' R data frame, optionally applying the variable labels and value labels stored
#' in the table's metadata dictionary.
#'
#' @param data A lazy \code{rcdf_tbl_db} object (returned by
#'   \code{\link{read_rcdf}} with \code{lazy = TRUE}), or any object supported
#'   by \code{dplyr::collect()}.
#' @param ... Additional arguments passed to \code{dplyr::collect()}.
#'
#' @returns A \code{tibble} with all rows materialised. If the table carries a
#'   metadata dictionary, variable labels and value labels are applied via
#'   \code{\link{add_metadata}} before returning.
#' @export
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path  <- file.path(dir, "mtcars.rcdf")
#' prv_key    <- file.path(dir, "sample-private-key-pw.pem")
#'
#' \dontrun{
#' result <- read_rcdf(path = rcdf_path, decryption_key = prv_key,
#'                     password = "1234", lazy = TRUE)
#' df <- collect(result$mtcars)
#' class(df)  # "tbl_df"
#' }
collect <- function(data, ...) { UseMethod('collect') }


#' @rdname collect
#' @export
collect.rcdf_tbl_db <- function(data, ...) {

  metadata <- attributes(data)$metadata
  attr(data, 'metadata') <- NULL
  class(data) <- setdiff(class(data), "rcdf_tbl_db")

  collected <- dplyr::collect(data)
  if (!is.null(metadata)) add_metadata(collected, metadata) else collected
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
#' dir <- system.file("extdata", package = "rcdf")
#' pub_key <- file.path(dir, 'sample-public-key.pem')
#' encrypt_string('hello', pub_key)

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
#' dir <- system.file("extdata", package = "rcdf")
#' pub_key <- file.path(dir, 'sample-public-key.pem')
#' prv_key <- file.path(dir, 'sample-private-key.pem')
#' x <- encrypt_string('hello', pub_key)
#' decrypt_string(x, prv_key = prv_key, password = '1234')

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



