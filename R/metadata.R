#' Add metadata attributes to a data frame
#'
#' Adds variable labels and value labels to a data frame based on a metadata
#' dictionary. This is particularly useful for preparing datasets for use with
#' packages like \code{haven} or for exporting to formats like SPSS or Stata.
#'
#' @param data A data frame containing the raw dataset.
#' @param metadata A data frame that serves as a metadata dictionary. It must contain
#'   at least the columns: \code{variable_name}, \code{label}, and \code{type}. Optionally,
#'   it may include a \code{valueset} column for categorical variables, which should be
#'   a list column with data frames containing \code{value} and \code{label} columns.
#' @param ... Additional arguments (currently unused).
#' @param set_data_types Logical; if \code{TRUE}, attempts to coerce column data types
#'   to match those implied by the metadata. (Note: currently not fully implemented.)
#'
#' @return A `tibble` with the same data as \code{data}, but with added attributes:
#'   - Variable labels (via the \code{label} attribute)
#'   - Value labels (as a \code{haven::labelled} class, if applicable)
#'
#' @details
#' The function first checks the structure of the \code{metadata} using an internal helper.
#' Then, for each variable listed in \code{metadata}, it:
#' - Adds a label using the \code{label} attribute
#' - Converts values to labelled vectors using \code{haven::labelled()} if a \code{valueset} is provided
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

  if(is.character(metadata)) { metadata <- read_metadata(metadata) }

  column_names <- colnames(data)
  dictionary <- check_metadata_structure(metadata, column_names)

  if(inherits(data, 'rcdf_tbl_db')) {

    attr(data, "metadata") <- metadata

  } else {

    with_valueset_col <- "valueset" %in% names(dictionary)
    variable_names <- dictionary$variable_name

    # Compute updated columns in one lapply pass (avoids repeated `[[<-` overhead).
    updated <- lapply(seq_along(variable_names), function(i) {
      vn <- variable_names[i]
      if (!(vn %in% column_names)) return(NULL)

      col <- data[[vn]]
      attr(col, "label") <- dictionary$label[i]

      if (with_valueset_col) {
        valueset <- dictionary$valueset[i][[1]]
        if (length(valueset) > 0) {
          labels <- valueset$value
          is_int <- rlang::is_integer(col)
          if (is_int && !rlang::is_integer(labels) && grepl("^\\d{1,}$", labels[1])) {
            labels <- as.integer(labels)
          }
          if (!is_int && grepl("^\\d{1,}$", col[1]) && rlang::is_integer(labels)) {
            col <- as.integer(col)
          }
          names(labels) <- valueset$label
          col <- haven::labelled(x = col, labels = labels, label = dictionary$label[i])
        }
      }
      col
    })
    names(updated) <- variable_names

    # Batch-assign all non-NULL updated columns back to the data frame.
    valid <- !vapply(updated, is.null, logical(1L))
    data[variable_names[valid]] <- updated[valid]
  }

  return(data)

}

check_metadata_structure <- function(data, cols) {

  required_cols <- c("variable_name", "label", "type")
  required_cols_which <- which(required_cols %in% names(data))

  if (length(required_cols_which) != length(required_cols)) {
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
    ) |>
    dplyr::filter(variable_name %in% cols)
}

convert_to_na <- function(data) {
  data |>
    dplyr::mutate_if(is.character, stringr::str_trim) |>
    dplyr::mutate_if(
      is.character,
      ~ dplyr::if_else(. == '', NA_character_, stringr::str_squish(.))
    )
}

read_metadata <- function(path) {

  data <- NULL

  if(grepl("\\.json$", path)) {
    data <- jsonlite::read_json(path, simplifyVector = TRUE)
  } else if (grepl("\\.csv$", path)) {
    data <- utils::read.csv(path)
  } else if (grepl("\\.xlsx$", path)) {
    data <- openxlsx::read.xlsx(path)
  } else if (grepl("\\.parquet$", path)) {
    data <- arrow::open_dataset(path)
  }

  data

}


#' Extract metadata from an RCDF file
#'
#' Retrieves a specific metadata value from a \code{.rcdf} file.
#'
#' @param path Character string. The file path to the \code{.rcdf} file.
#' @param name Character string. The metadata key to extract from the file.
#' @param key `r lifecycle::badge('deprecated')` Character string. The metadata key to extract from the file.
#'
#' @return The value associated with the specified metadata key, or \code{NULL} if the key does not exist.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming "example.rcdf" is a valid RCDF file in the working directory:
#' get_rcdf_metadata("example.rcdf", "log_id")
#' }

get_rcdf_metadata <- function(path, name = NULL, key) {

  if(!fs::file_exists(path)) {
    stop(glue::glue("Specified RCDF file does not exist: {path}"))
  }

  if(!grepl("\\.rcdf$", path)) {
    stop(glue::glue("Not a valid RCDF file: {path}"))
  }

  ext <- extract_rcdf(path, meta_only = TRUE)
  ext$cleanup()

  if(!is.null(name)) {
    ext$meta[[name]]
  } else {
    warning("The `key` argument is now deprecated. Use `name` instead.")
    ext$meta[[key]]
  }

}

#' Get metadata attribute from RCDF data
#'
#' @param rcdf RCDF data
#' @param attr Valid metadata key.
#'
#' @returns RCDF attribute/s or NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming `df` is a valid RCDF object
#'
#' get_attr(df, "area_names")
#'
#' # To get nested attributes
#' get_attr(df, "meta.source_note")
#' }


get_attr <- function(rcdf, attr) {

  attr_key <- stringr::str_split_1(attr, "\\.")

  if(length(attr_key) > 1) {
    attributes(rcdf)$metadata[[attr_key[1]]][[attr_key[2]]]
  } else {
    attributes(rcdf)$metadata[[attr]]
  }

}

#' Read all top-level metadata from an RCDF file
#'
#' Extracts and returns the complete metadata JSON stored inside an
#' \code{.rcdf} archive without decrypting or loading the underlying data.
#' Useful for inspecting provenance, checksums, encryption parameters, and
#' embedded data dictionaries without a decryption key.
#'
#' @param path Character string. Path to a valid \code{.rcdf} file.
#'
#' @returns A named list corresponding to the \code{metadata.json} stored
#'   inside the archive. Common keys include \code{log_id}, \code{created_at},
#'   \code{version}, \code{checksum}, \code{dictionary}, and \code{key}.
#' @export
#'
#' @seealso \code{\link{get_rcdf_metadata}} for retrieving a single metadata key,
#'   \code{\link{get_attr}} for reading metadata attributes attached to an
#'   in-memory RCDF object.
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, "mtcars.rcdf")
#'
#' meta <- get_attrs(rcdf_path)
#' meta$version
#' meta$created_at
get_attrs <- function(path) {

  if(!fs::file_exists(path)) {
    stop(glue::glue("Specified RCDF file does not exist: {path}"))
  }

  if(!grepl("\\.rcdf$", path)) {
    stop(glue::glue("Not a valid RCDF file: {path}"))
  }

  extracted_dir <- fs::file_temp()
  fs::dir_create(extracted_dir)

  zip::unzip(path, exdir = extracted_dir, junkpaths = TRUE)
  meta_file <- file.path(extracted_dir, "metadata.json")

  if (!fs::file_exists(meta_file)) {
    unlink(extracted_dir, recursive = TRUE, force = TRUE)
    stop("metadata.json not found inside RCDF archive.")
  }

  meta <- jsonlite::read_json(meta_file, simplifyVector = TRUE)
  unlink(extracted_dir, recursive = TRUE, force = TRUE)
  return(meta)
}

