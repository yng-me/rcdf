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
