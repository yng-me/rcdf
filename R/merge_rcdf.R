#' Merge multiple RCDF files
#'
#' @param rcdf_files A character vector of RCDF file paths
#' @param decryption_keys Decryption keys associated with each RCDF file. Must match the length of the vector passed in the \code{rcdf_files} argument.
#' @param passwords Password of the associated decryption keys. Must match the length of \code{decryption_keys}.
#' @param merged_file_path File path or name of the merged RCDF file.
#' @param pub_key Public key to encrypt the merged file. If \code{NULL}, a new RSA key pair will be generated.
#'
#' @returns \code{NULL} (void)
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dir <- system.file("extdata", package = "rcdf")
#'
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key-pw.pem')
#' pw <- '1234'
#'
#' temp_dir <- tempdir()
#'
#' merge_rcdf(
#'   rcdf_files = rcdf_path,
#'   decryption_keys = private_key,
#'   passwords = pw,
#'   merged_file_path = file.path(temp_dir, "merged.rcdf"),
#'   pub_key = file.path(dir, 'sample-public-key-pw.pem')
#' )
#'
#' unlink(file.path(temp_dir, "merged.rcdf"), force = TRUE)
#' }


merge_rcdf <- function(rcdf_files, decryption_keys, passwords, merged_file_path, pub_key = NULL) {

  merged <- read_rcdf(
    path = rcdf_files,
    decryption_key = decryption_keys,
    password = passwords,
    return_meta = TRUE
  )

  dcf <- merged[['__data_dictionary']]
  meta <- attributes(merged)$metadata

  if(!grepl('\\.rcdf$', merged_file_path)) {
    merged_file_path <- paste0(merged_file_path, '.rcdf')
  }

  if(is.null(pub_key)) {

    pw <- generate_pw(32)

    path_up <- normalizePath(merged_file_path, winslash = "/", mustWork = FALSE)
    path_up <- sub(paste0("/", basename(path_up), "$"), "", path_up)
    pub_key_name <- tools::file_path_sans_ext(basename(merged_file_path))
    pub_key <- generate_rsa_keys(
      path = path_up,
      prefix = pub_key_name,
      password = pw
    )

    alert <- path_up
    alert_pw <- pw
    message(sprintf("Generated new RSA keys in: %s", alert))
    message(sprintf("Password for decryption key: %s", alert_pw))

  }

  merged[['__data_dictionary']] <- NULL
  merged <- Filter(Negate(is.null), merged)

  write_rcdf(
    data = as_rcdf(merged),
    path = merged_file_path,
    pub_key = pub_key,
    metadata = list(
      dictionary = dcf,
      meta = meta$meta,
      area_names = meta$area_names
    )
  )

  return(invisible(NULL))

}
