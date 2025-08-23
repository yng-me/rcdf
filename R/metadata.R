#' Add metadata attributes to a data frame
#'
#' Adds variable labels and value labels to a data frame based on a metadata
#' dictionary. This is particularly useful for preparing datasets for use with
#' packages like `haven` or for exporting to formats like SPSS or Stata.
#'
#' @param data A data frame containing the raw dataset.
#' @param metadata A data frame that serves as a metadata dictionary. It must contain
#'   at least the columns: `"variable_name"`, `"label"`, and `"type"`. Optionally,
#'   it may include a `"valueset"` column for categorical variables, which should be
#'   a list column with data frames containing `"value"` and `"label"` columns.
#' @param ... Additional arguments (currently unused).
#' @param set_data_types Logical; if `TRUE`, attempts to coerce column data types
#'   to match those implied by the metadata. (Note: currently not fully implemented.)
#'
#' @return A `tibble` with the same data as `data`, but with added attributes:
#'   - Variable labels (via the `"label"` attribute)
#'   - Value labels (as a `haven::labelled` class, if applicable)
#'
#' @details
#' The function first checks the structure of the `metadata` using an internal helper.
#' Then, for each variable listed in `metadata`, it:
#' - Adds a label using the `"label"` attribute
#' - Converts values to labelled vectors using `haven::labelled()` if a `valueset` is provided
#'
#' If value labels are present, the function tries to align data types between the data
#' and the valueset (e.g., converting character codes to integers if necessary).
#'
#' @export
#'
#'
#' @examples
#' data <- data.frame(
#'   sex = c(1, 2, 1),
#'   age = c(23, 45, 34)
#' )
#'
#' metadata <- data.frame(
#'   variable_name = c("sex", "age"),
#'   label = c("Gender", "Age in years"),
#'   type = c("categorical", "numeric"),
#'   valueset = I(list(
#'     data.frame(value = c(1, 2), label = c("Male", "Female")),
#'     NULL
#'   ))
#' )
#'
#' labelled_data <- add_metadata(data, metadata)
#' str(labelled_data)
#'

add_metadata <- function(data, metadata, ..., set_data_types = FALSE) {
  column_names <- names(data)
  dictionary <- check_metadata_structure(metadata)

  with_valueset_col <- "valueset" %in% names(dictionary)

  variable_names <- dictionary$variable_name

  for(i in seq_along(variable_names)) {

    variable_name_i <- variable_names[i]

    if(!(variable_name_i %in% column_names)) next

    attr(data[[variable_name_i]], "label") <- dictionary$label[i]

    if(!with_valueset_col) next
    valueset <- dictionary$valueset[i][[1]]

    if(is.null(valueset)) next

    labels <- valueset$value

    is_int <- rlang::is_integer(data[[variable_name_i]])
    if(is_int & !rlang::is_integer(labels) & grepl("^\\d{1,}$", labels[1])) {
      labels <- as.integer(labels)
    }

    if(!is_int & grepl("^\\d{1,}$", data[[variable_name_i]][1]) & rlang::is_integer(labels)) {
      data[[variable_name_i]] <- as.integer(data[[variable_name_i]])
    }

    names(labels) <- valueset$label

    data[[variable_name_i]] <- haven::labelled(
      x = data[[variable_name_i]],
      labels = labels,
      label = dictionary$label[i]
    )
  }

  return(dplyr::tibble(data))
}


check_metadata_structure <- function(data) {

  required_cols <- c("variable_name", "label", "type")
  required_cols_which <- which(required_cols %in% names(data))

  if (length(required_cols_which) < length(required_cols)) {
    stop("Invalid column names specified.")
  }

  data |>
    dplyr::filter(!is.na(variable_name)) |>
    dplyr::distinct(variable_name, .keep_all = TRUE) |>
    convert_to_na() |>
    dplyr::select(
      variable_name,
      label,
      type,
      dplyr::any_of(c("input_data", "valueset", "labels"))
    )
}


convert_to_na <- function(data) {
  data |>
    dplyr::mutate_if(is.character, stringr::str_trim) |>
    dplyr::mutate_if(
      is.character,
      ~ dplyr::if_else(. == '', NA_character_, stringr::str_squish(.))
    )
}
