#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#'

read_env <- function(path) {

  if(missingArg(path)) {  path <- '.env' }

  if(!file.exists(path)) {
    stop('Path to environment file is required.')
  }

  env_file <- readLines(path)
  env_values <- unlist(stringr::str_split(env_file, '\n')[[1]])
  env_values <- env_values[env_values != '']
  env_values <- env_values[!grepl('^#', env_values)]

  if (length(env_values) == 0) {
    return(NULL)
  }

  env <- list()
  for (i in seq_along(env_values)) {
    env_value <- unlist(stringr::str_split(env_values[i], "=")[[1]])
    key <- stringr::str_trim(env_value[1])
    value <- stringr::str_trim(env_value[2])

    env[[key]] <- stringr::str_remove_all(value, '\\"')
  }

  return(env)
}
