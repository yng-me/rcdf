#' Read environment variables from a file
#'
#' Based on https://github.com/gaborcsardi/dotenv
#'
#' Reads a \code{.env} file containing environment variables in the format \code{KEY=VALUE}, and returns them as a named list.
#' Lines starting with \code{#} are considered comments and ignored.
#'
#' @param path A string specifying the path to the \code{.env} file. If not provided, defaults to \code{.env} in the current working directory.
#'
#' @return A named list of environment variables. Each element is a key-value pair extracted from the file. If no variables are found, \code{NULL} is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming an `.env` file with the following content:
#' # DB_HOST=localhost
#' # DB_USER=root
#' # DB_PASS="secret"
#'
#' env_vars <- read_dot_env(".env")
#' print(env_vars)
#' # Should output something like:
#' # $DB_HOST
#' # [1] "localhost"
#'
#' # If no path is given, it defaults to `.env` in the current directory.
#' env_vars <- read_dot_env()
#' }


read_dot_env <- function(path = ".env") {

  if (!file.exists(path)) stop("dot-env file does not exist", call. = TRUE)

  tmp <- readLines(path)
  tmp <- ignore_comments(tmp)
  tmp <- ignore_empty_lines(tmp)

  # If there's no env vars, return nothing
  if (length(tmp) == 0) return(invisible())

  env <- lapply(tmp, parse_dot_line)
  set_env(env)

}


ignore_comments <- function(lines) {
  grep("^#", lines, invert = TRUE, value = TRUE)
}

ignore_empty_lines <- function(lines) {
  grep("^\\s*$", lines, invert = TRUE, value = TRUE)
}


parse_dot_line <- function(line) {

  line_regex <- paste0(
    "^\\s*",
    "(?<export>export\\s+)?", # export, if given
    "(?<key>[^=]+)",          # variable name
    "=",                      # equals sign
    "(?<q>['\"]?)",           # quote if present
    "(?<value>.*)",           # value
    "\\g{q}",                 # the same quote again
    "\\s*",                   # trailing whitespace
    "$"                       # end of line
  )

  match <- regexpr(line_regex, line, perl = TRUE)
  if (match == -1) stop("Cannot parse dot-env line: ", substr(line, 1, 40),
                        call. = FALSE)
  as.list(extract_match(line, match)[c("key", "value")])
}

extract_match <- function(line, match) {
  tmp <- mapply(
    attr(match, "capture.start"),
    attr(match, "capture.length"),
    FUN = function(start, length) {
      tmp <- substr(line, start, start + length - 1)
    }
  )
  names(tmp) <- attr(match, "capture.names")
  tmp
}

set_env <- function(pairs) {
  structure(
    .Data  = lapply(pairs, "[[", "value"),
    .Names = sapply(pairs, "[[", "key")
  )
}


#' Read environment variables from a file
#'
#' `r lifecycle::badge('deprecated')`
#'
#' Reads a \code{.env} file containing environment variables in the format \code{KEY=VALUE}, and returns them as a named list.
#' Lines starting with \code{#} are considered comments and ignored.
#'
#' @param path A string specifying the path to the \code{.env} file. If not provided, defaults to \code{.env} in the current working directory.
#'
#' @return A named list of environment variables. Each element is a key-value pair extracted from the file. If no variables are found, \code{NULL} is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming an `.env` file with the following content:
#' # DB_HOST=localhost
#' # DB_USER=root
#' # DB_PASS="secret"
#'
#' env_vars <- read_env(".env")
#' print(env_vars)
#' # Should output something like:
#' # $DB_HOST
#' # [1] "localhost"
#'
#' # If no path is given, it defaults to `.env` in the current directory.
#' env_vars <- read_env()
#' }

read_env <- function(path = '.env') {

  warning("This function is currently deprecated. Use `read_dot_env()` instead.")
  read_dot_env(path)
}
