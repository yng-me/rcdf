#' Create an empty `rcdf` object
#'
#' Initializes and returns an empty `rcdf` object. This is a convenient constructor
#' for creating a new `rcdf`-class list structure.
#'
#' @return A list object of class `"rcdf"`.
#' @export
#'
#' @examples
#' rcdf <- rcdf_list()
#' class(rcdf)

rcdf_list <- function() {
  value <- list()
  as_rcdf(value)
}



#' Convert to `rcdf` class
#'
#' Converts an existing list or compatible object into an object of class `"rcdf"`.
#'
#' @param data A list or object to be converted to class `"rcdf"`.
#'
#' @return The input object with class set to `"rcdf"`.
#' @export
#'
#' @examples
#' my_list <- list(a = 1, b = 2)
#' rcdf_obj <- as_rcdf(my_list)
#' class(rcdf_obj)

as_rcdf <- function(data) {
  set_class(data, class_name = 'rcdf')
}


set_class <- function(data, class_name) {
  class(data) <- c(class(data), class_name)
  return(data)
}


is_rcdf <- function(data) {
  inherits(data, 'rcdf') & inherits(data, 'list')
}



check_if_rcdf <- function(data) {
  if(!is_rcdf(data)) {
    stop('Not a valid RCDF data file')
  }
}


raw_to_hex <- function(x) {
  if (!is.raw(x)) {
    stop("Input must be a raw vector.")
  }

  hex_string <- paste0(sprintf("%02X", as.integer(x)), collapse = "")

  return(hex_string)
}


hex_to_raw <- function(x) {
  digits <- strtoi(strsplit(x, "")[[1]], base = 16L)
  as.raw(bitwShiftL(digits[c(TRUE, FALSE)], 4) + digits[c(FALSE, TRUE)])
}


generate_aes_key <- function(passphrase = "") {

  key <- raw_to_hex(openssl::aes_keygen())
  if(passphrase == '') { passphrase <- as.character(Sys.Date()) }

  salt <- openssl::sha256(passphrase)

  value <- list(
    aes_key = as.character(openssl::sha256(paste0(key, salt))),
    aes_iv = as.character(raw_to_hex(openssl::rand_bytes(16)))
  )

  return(value)

}



dir_create_new <- function(path, parent_dir = NULL) {
  if(!is.null(parent_dir)) {
    path <- file.path(path, parent_dir)
  }
  fs::dir_create(path)
  return(path)
}



decrypt_info_aes <- function(data, key = list()) {

  data <- openssl::base64_decode(data)

  aes_key <- key$aes_key
  aes_iv <- key$aes_iv

  if(is.null(aes_key)) {
    return(unserialize(data))
  }

  if(is.null(aes_iv)) {

    value <- openssl::aes_cbc_decrypt(
      data = data,
      key = openssl::sha256(charToRaw(aes_key)),
      iv = NULL
    )

    return(unserialize(value))

  }

  value <- openssl::aes_cbc_decrypt(
    data = data,
    key = openssl::sha256(charToRaw(aes_key)),
    iv = hex_to_raw(aes_iv)
  )

  unserialize(value)

}



extract_key <- function(meta) {

  key <- meta$key_app
  key_admin <- meta$key_admin

  iv <-  meta$iv_app
  iv_admin <-  meta$iv_admin

  if(!is.null(key_admin)) {
    key <- key_admin
  }

  if(!is.null(iv_admin)) {
    iv <- stringr::str_split_i(iv_admin, pattern = '>>><<<', i = 1)
  }

  return(list(key = key, iv = iv))

}




encrypt_info_rsa <- function(data, pub_key) {
  data |>
    serialize(connection = NULL) |>
    openssl::rsa_encrypt(pubkey = pub_key) |>
    openssl::base64_encode()
}



decrypt_info_rsa <- function(data, prv_key, password = NULL) {

  if(!is.null(password)) {
    key <- openssl::read_key(file = prv_key, password = password)
  } else {
    key <- openssl::read_key(file = prv_key)
  }

  data |>
    openssl::base64_decode() |>
    openssl::rsa_decrypt(key) |>
    unserialize()
}



get_pc_metadata <- function(which) {

  values <- list()

  pc <- Sys.info()

  values$pc_os <- tolower(pc[["sysname"]])
  values$pc_user <- pc[["user"]]
  values$pc_effective_user <- pc[["effective_user"]]
  values$pc_os_release_date <- pc[["version"]] |>
    stringr::str_extract(":\\s\\w*\\s\\w*\\s\\d{2}\\s\\d{2}:\\d{2}:\\d{2}.*;") |>
    stringr::str_remove("^:\\s") |>
    stringr::str_remove(";$")
  values$pc_os_version <- pc[["release"]]
  values$pc_hardware <- pc[["machine"]]
  values$pc_pid <- Sys.getpid()

  values[[which]]

}
