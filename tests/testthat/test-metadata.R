sample_dcf <- dplyr::tibble(
  variable_name = c("name", "sex", "gender", "height", "mass", "hair_color", "skin_color", "eye_color", "homeworld"),
  label = c("Character name", "Sex", "Gender", "Height", "Mass", "Hair color", "Skin color", "Eye color", "Homeworld"),
  type = c("c", "c", "c", "d", "d", "c", "c", "c", "c"),
  valueset = c(
    list(NULL),
    list(
      dplyr::tibble(
        value = c("male", "female", "none", "hermaphroditic"),
        label = c("Male", "Female", "None", "Hermaphroditic")
      )
    ),
    list(NULL),
    list(NULL),
    list(NULL),
    list(NULL),
    list(NULL),
    list(NULL),
    list(NULL)
  )
)



test_that("add_metadata works correctly", {

  starwars <- dplyr::select(
    dplyr::starwars,
    name,
    height,
    mass,
    dplyr::ends_with('color'),
    sex,
    species,
    gender
  )

  starwars_with_meta <- add_metadata(starwars, metadata = sample_dcf)

  expect_true(attributes(starwars_with_meta$name)$label == "Character name")
  expect_true(is.null(attributes(starwars_with_meta$species)$label))
  expect_true(!is.null(attributes(starwars_with_meta$sex)$labels))
  expect_identical(
    as.character(attributes(starwars_with_meta$sex)$labels),
    c("male", "female", "none", "hermaphroditic")
  )
  expect_identical(
    names(attributes(starwars_with_meta$sex)$labels),
    c("Male", "Female", "None", "Hermaphroditic")
  )

})


test_that("check_metadata_structure throws when required columns are missing", {
  bad_meta <- data.frame(variable_name = "x", label = "X")  # missing 'type'
  expect_error(check_metadata_structure(bad_meta, cols = "x"), "Invalid column names")
})


test_that("add_metadata handles integer coercion for labelled valuesets", {
  df <- data.frame(code = c(1L, 2L, 1L))
  meta <- data.frame(
    variable_name = "code",
    label = "Status",
    type = "categorical",
    valueset = I(list(
      data.frame(value = c(1L, 2L), label = c("Active", "Inactive"))
    ))
  )
  result <- add_metadata(df, meta)
  expect_true(inherits(result$code, "haven_labelled"))
  expect_equal(attr(result$code, "label"), "Status")
})


test_that("add_metadata on rcdf_tbl_db attaches metadata attribute without labelling columns", {
  df <- tibble::tibble(a = 1:3, b = letters[1:3])
  class(df) <- c("rcdf_tbl_db", class(df))
  meta <- data.frame(
    variable_name = "a", label = "Column A", type = "numeric",
    stringsAsFactors = FALSE
  )
  result <- add_metadata(df, meta)
  expect_identical(attributes(result)$metadata, meta)
  expect_null(attr(result$a, "label"))
})


test_that("read_metadata reads a CSV file", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(variable_name = "x", label = "X", type = "numeric"), tmp, row.names = FALSE)
  result <- read_metadata(tmp)
  expect_true(is.data.frame(result))
  expect_true("variable_name" %in% names(result))
  unlink(tmp)
})


test_that("read_metadata reads a JSON file", {
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(
    data.frame(variable_name = "x", label = "X", type = "numeric"),
    tmp, auto_unbox = TRUE
  )
  result <- read_metadata(tmp)
  expect_true(is.data.frame(result))
  unlink(tmp)
})


test_that("read_metadata reads an XLSX file", {
  tmp <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    data.frame(variable_name = "x", label = "X", type = "numeric"), tmp
  )
  result <- read_metadata(tmp)
  expect_true(is.data.frame(result))
  unlink(tmp)
})


test_that("read_metadata reads a Parquet file", {
  tmp <- tempfile(fileext = ".parquet")
  write_parquet(
    data.frame(variable_name = "x", label = "X", type = "numeric"), tmp
  )
  result <- read_metadata(tmp)
  expect_true(!is.null(result))
  unlink(tmp)
})


test_that("get_rcdf_metadata returns the correct metadata value", {
  dir <- system.file("extdata", package = "rcdf")
  path <- file.path(dir, "mtcars.rcdf")
  version <- get_rcdf_metadata(path, name = "version")
  expect_true(is.character(version))
})


test_that("get_rcdf_metadata errors on non-existent file", {
  expect_error(get_rcdf_metadata(tempfile(fileext = ".rcdf"), "version"))
})


test_that("get_rcdf_metadata errors on non-.rcdf file", {
  tmp <- tempfile(fileext = ".zip")
  writeLines("", tmp)
  expect_error(get_rcdf_metadata(tmp, "version"), "valid RCDF file")
  unlink(tmp)
})


test_that("get_attrs returns the full metadata list from an RCDF file", {
  dir <- system.file("extdata", package = "rcdf")
  path <- file.path(dir, "mtcars.rcdf")
  meta <- get_attrs(path)
  expect_true(is.list(meta))
  expect_true("version" %in% names(meta))
})


test_that("get_attrs errors on non-existent file", {
  expect_error(get_attrs(tempfile(fileext = ".rcdf")))
})


test_that("get_attrs errors on non-.rcdf file", {
  tmp <- tempfile(fileext = ".txt")
  writeLines("", tmp)
  expect_error(get_attrs(tmp), "valid RCDF file")
  unlink(tmp)
})


test_that("get_attr retrieves a top-level metadata attribute after read_rcdf", {
  dir <- system.file("extdata", package = "rcdf")
  rcdf_data <- read_rcdf(
    path = file.path(dir, "mtcars.rcdf"),
    decryption_key = file.path(dir, "sample-private-key-pw.pem"),
    password = "1234",
    return_meta = TRUE
  )
  version <- get_attr(rcdf_data, "version")
  expect_true(is.character(version))
})
