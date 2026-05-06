#' Create an empty \code{rcdf} object
#'
#' Initializes and returns an empty \code{rcdf} object. This is a convenient constructor
#' for creating a new \code{rcdf}-class list structure.
#'
#' @param ... Optional elements to include in the list. These will be passed to
#'   the internal list constructor and included in the resulting \code{rcdf} object.
#'
#' @return A list object of class \code{rcdf}.
#' @export
#'
#' @examples
#' rcdf <- rcdf_list()
#' class(rcdf)

rcdf_list <- function(...) {
  value <- list(...)
  as_rcdf(value)
}


#' Convert to \code{rcdf} class
#'
#' Converts an existing list or compatible object into an object of class \code{rcdf}.
#'
#' @param data A list or object to be converted to class \code{rcdf}.
#'
#' @return The input object with class set to \code{rcdf}.
#' @export
#'
#' @examples
#' my_list <- list(a = 1, b = 2)
#' rcdf_obj <- as_rcdf(my_list)
#' class(rcdf_obj)

as_rcdf <- function(data) {
  set_class(data, class_name = 'rcdf')
}


#' Generate RSA key pair and save to files
#'
#' This function generates an RSA key pair (public and private) and saves them to specified files.
#'
#' @param path A character string specifying the directory path where the key files in \code{.pem} format should be saved.
#' @param ... Additional arguments passed to the \code{openssl::rsa_keygen()} function, such as key size.
#' @param password A character string specifying the password for the private key. If \code{NULL}, the private key will not be encrypted.
#' @param which A character string specifying which key to return. Can be either \code{"public"} or \code{"private"}. Default is \code{"public"}.
#' @param prefix A character string used as a prefix for the key file names. Defaults to \code{NULL}, which will result in no prefix.
#'
#' @return A character string representing the file path of the generated key (either public or private, based on the \code{which} argument).
#'
#' @export
#'
#' @examples
#' # Generate both public and private RSA keys and save them to the temp directory
#' path_to <- tempdir()
#' generate_rsa_keys(path = path_to, password = "securepassword")
#'

generate_rsa_keys <- function(path, ..., password = NULL, which = "public", prefix = NULL) {
  key <- openssl::rsa_keygen(...)
  path_to <- list(
    public = file.path(path, paste0(c(prefix, "public-key.pem"), collapse = "-")),
    private = file.path(path, paste0(c(prefix, "private-key.pem"), collapse = "-"))
  )
  openssl::write_pem(key$pubkey, path = path_to$public)
  openssl::write_pem(key, path = path_to$private, password = password)

  return(path_to[[which]])
}


#' Generate a random password
#'
#' This function generates a random password of a specified length. It includes
#' alphanumeric characters by default and can optionally include special characters.
#'
#' @param length Integer. The length of the password to generate. Default is \code{16}.
#' @param special_chr Logical. Whether to include special characters
#'   (e.g., `!`, `@`, `#`, etc.) in the password. Default is \code{TRUE}.
#'
#' @return A character string representing the generated password.
#' @export
#'
#' @examples
#' generate_pw()
#' generate_pw(32)
#' generate_pw(12, special_chr = FALSE)

generate_pw <- function(length = 16, special_chr = TRUE) {

  pw <- c(0:9, rep(letters, 2), rep(LETTERS, 2))

  if(special_chr) {
    pw <- c(pw, "!", "#", "@", "$", "%", "&", "^", "-", "_", "=", "(", ")", "*", "+", "?", "<", ">")
  }

  # Use a cryptographically secure RNG: draw one random byte per character
  # and use modular reduction to pick an index.
  n_choices <- length(pw)
  rand_bytes <- as.integer(openssl::rand_bytes(length))
  indices <- (rand_bytes %% n_choices) + 1L

  paste0(pw[indices], collapse = "")

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
    data <- rcdf_list(data = data)
  }

  data

}


raw_to_hex <- function(x) {
  if (!is.raw(x)) {
    stop("Input must be a raw vector.")
  }

  hex_string <- paste0(sprintf("%02X", as.integer(x)), collapse = "")

  return(hex_string)
}


hex_to_raw <- function(x) {
  if (!is.character(x) || length(x) != 1L) {
    stop("hex_to_raw: input must be a single character string.", call. = FALSE)
  }
  if (nchar(x) %% 2L != 0L) {
    stop("hex_to_raw: input must have an even number of characters.", call. = FALSE)
  }
  if (!grepl("^[0-9A-Fa-f]*$", x)) {
    stop("hex_to_raw: input contains non-hexadecimal characters.", call. = FALSE)
  }
  digits <- strtoi(strsplit(x, "")[[1]], base = 16L)
  as.raw(bitwShiftL(digits[c(TRUE, FALSE)], 4) + digits[c(FALSE, TRUE)])
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

  if(!is.null(meta$key)) {

    secret <- normalize_key_value(meta$key)

    return(
      list(
        key = secret$key,
        value = secret$value,
        legacy = FALSE
      )
    )

  } else {

    # Legacy

    key <- meta$key_app
    key_admin <- meta$key_admin

    value <-  meta$iv_app
    value_admin <-  meta$iv_admin

    if(!is.null(key_admin)) {
      key <- key_admin
    }

    if(!is.null(value_admin)) {
      value <- stringr::str_split_i(value_admin, pattern = '>>><<<', i = 1)
    }

    return(list(key = key, value = value, legacy = TRUE))
  }

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

normalize_key_value <- function(value) {

  # Legacy support
  if(inherits(value, 'list')) {

    if(!is.null(value$aes_key) & !is.null(value$aes_iv)) {
      return(
        list(
          key = value$aes_key,
          value = value$aes_iv
        )
      )
    } else {
      return(NULL)
    }

  } else if (typeof(value) == 'character') {

    # Check hex before base64: hex chars are a subset of base64-valid characters,
    # so a pure-hex string of length 32/48/64 would otherwise always match base64 first.
    if (grepl("^(?:[A-Fa-f0-9]{32}|[A-Fa-f0-9]{48}|[A-Fa-f0-9]{64})$", value, ignore.case = TRUE)) {

      key_length <- 8 * nchar(value)

      return(
        list(
          key = glue::glue("key{key_length}"),
          value = value
        )
      )

    } else if(grepl('^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)?$', value)) {

      return(
        list(
          key = "key256base64",
          value = value
        )
      )

    } else {
      stop('Invalid `decryption_key` provided.')
    }

  } else {
    return(NULL)
  }

}

decrypt_key <- function(data, prv_key, password = NULL) {

  meta <- extract_key(data)

  if(inherits(prv_key, 'rsa')) {

    key <- meta$key
    value <- openssl::base64_decode(meta$value) |>
      openssl::rsa_decrypt(key, password = password) |>
      unserialize()

    if(meta$legacy) {

      key <- openssl::base64_decode(meta$key) |>
        openssl::rsa_decrypt(key, password = password) |>
        unserialize()

    }

  } else if (inherits(prv_key, 'character')) {

    if(grepl('.pem$', prv_key)) {

      value <- decrypt_string(meta$value, prv_key = prv_key, password = password)
      key <- meta$key

      if(meta$legacy) {
        key <- decrypt_string(meta$key, prv_key = prv_key, password = password)
      }

    } else {

      # Legacy
      key <- decrypt_info_aes(meta$key, key = list(aes_key = prv_key))
      value <- decrypt_info_aes(meta$value, key = list(aes_key = key))

    }

  }

  return(
    list(
      key = key,
      value = value
    )
  )
}

open_duckdb_connection <- function(n_threads = NULL) {

  cfg <- list()
  if (!is.null(n_threads)) {
    cfg$threads <- as.integer(n_threads)
  }

  conn <- DBI::dbConnect(
    duckdb::duckdb(),
    config = cfg
  )

  conn
}


# Load the full crypto module needed for AES-encrypted Parquet writes.
# Tries httpfs first (OpenSSL-backed); falls back to force_mbedtls_unsafe
# (DuckDB's built-in mbedtls) so that writes succeed even when httpfs is
# not installed in the current DuckDB distribution.
load_duckdb_crypto <- function(conn) {
  loaded <- tryCatch({
    DBI::dbExecute(conn, "LOAD httpfs")
    TRUE
  }, error = function(e) FALSE)

  if (!loaded) {
    tryCatch(
      DBI::dbExecute(conn, "SET force_mbedtls_unsafe = 'true'"),
      error = function(e) NULL
    )
  }

  invisible(NULL)
}
