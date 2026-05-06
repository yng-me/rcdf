# Helper function to create a mock RSA public key
create_mock_pub_key <- function(temp_dir, pw = NULL) {
  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(temp_dir, 'pub.pem'))
  openssl::write_pem(key, file.path(temp_dir, 'prv.pem'), password = pw)

  return(file.path(temp_dir, 'pub.pem'))
}


test_that("write_rcdf creates a valid RCDF file", {

  # Create a mock dataset
  mock_data <- list(
    dataset1 = data.frame(a = 1:5, b = letters[1:5]),
    dataset2 = data.frame(x = 6:10, y = letters[6:10])
  )

  mock_data <- as_rcdf(mock_data)

  # Create a mock public key
  dir_temp <- tempdir()
  pub_key <- create_mock_pub_key(dir_temp)

  # Create a temporary path for the RCDF file
  rcdf_path <- tempfile(fileext = ".rcdf")

  # Write the data to RCDF format
  write_rcdf(data = mock_data, path = rcdf_path, pub_key = pub_key)

  meta <- extract_rcdf(rcdf_path)

  expect_true(file.exists(file.path(meta$dir, 'metadata.json')))
  expect_true(file.exists(file.path(meta$dir, 'lineage')))

  # Clean up the mock public key
  unlink(pub_key, force = TRUE)
  unlink(file.path(dir_temp, 'prv.pem'), force = TRUE)

})



test_that("write_rcdf creates RCDF file with correct encryption", {

  # Create a mock dataset
  mock_data <- list(
    dataset1 = data.frame(a = 1:5, b = letters[1:5]),
    dataset2 = data.frame(x = 6:10, y = letters[6:10])
  )

  mock_data <- as_rcdf(mock_data)

  # Create a mock public key
  dir_temp <- tempdir()
  pub_key <- create_mock_pub_key(dir_temp)

  # Create a temporary path for the RCDF file
  rcdf_path <- tempfile(fileext = ".rcdf")

  # Write the data to RCDF format
  write_rcdf(data = mock_data, path = rcdf_path, pub_key = pub_key)

  meta <- extract_rcdf(rcdf_path)

  metadata_file <- jsonlite::fromJSON(file.path(meta$dir, 'metadata.json'), simplifyVector = TRUE)

  expect_true("key" %in% names(metadata_file))

  unlink(dir_temp, recursive = TRUE)

})


test_that("write_rcdf auto-appends .rcdf extension when missing", {
  dir_temp <- withr::local_tempdir()
  pub_key <- create_mock_pub_key(dir_temp)

  data <- as_rcdf(list(df = data.frame(a = 1:3)))
  path_no_ext <- file.path(dir_temp, "output")

  write_rcdf(data = data, path = path_no_ext, pub_key = pub_key)
  expect_true(file.exists(paste0(path_no_ext, ".rcdf")))
})


test_that("write_rcdf returns the data invisibly", {
  dir_temp <- withr::local_tempdir()
  pub_key <- create_mock_pub_key(dir_temp)

  data <- as_rcdf(list(df = data.frame(a = 1:3)))
  result <- write_rcdf(data = data, path = tempfile(fileext = ".rcdf"), pub_key = pub_key)

  expect_identical(result, data)
})


test_that("write_rcdf embeds data dictionary and it is readable on read_rcdf", {
  dir_temp <- withr::local_tempdir()
  pub_key <- create_mock_pub_key(dir_temp)
  rcdf_path <- file.path(dir_temp, "dict_test.rcdf")

  data <- as_rcdf(list(people = data.frame(age = c(25L, 30L))))
  dict <- data.frame(
    variable_name = "age", label = "Age (years)", type = "numeric",
    stringsAsFactors = FALSE
  )

  write_rcdf(data = data, path = rcdf_path, pub_key = pub_key,
             metadata = list(dictionary = dict))

  ext <- extract_rcdf(rcdf_path, meta_only = TRUE)
  on.exit(ext$cleanup(), add = TRUE)

  expect_false(is.null(ext$meta$dictionary))
  expect_equal(ext$meta$dictionary$variable_name[1], "age")
})


test_that("write_rcdf with ignore_duplicates = FALSE and duplicate rows throws an error", {
  dir_temp <- withr::local_tempdir()
  pub_key <- create_mock_pub_key(dir_temp)
  rcdf_path <- file.path(dir_temp, "dupe_test.rcdf")

  data <- as_rcdf(list(df = data.frame(id = c(1L, 1L, 2L), val = c("a", "b", "c"))))
  pk <- data.frame(file = "df", pk_field_name = "id")

  expect_error(
    write_rcdf(
      data = data,
      path = rcdf_path,
      pub_key = pub_key,
      metadata = list(primary_key = pk),
      ignore_duplicates = FALSE
    ),
    "duplicates"
  )
})
