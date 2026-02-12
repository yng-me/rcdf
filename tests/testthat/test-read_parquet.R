# Test `read_parquet()` without decryption
test_that("read_parquet reads Parquet file without decryption", {
  data <- tibble::tibble(a = 1:5, b = letters[1:5])
  temp_file <- tempfile(fileext = ".parquet")

  # Write Parquet file
  arrow::write_parquet(data, temp_file)

  # Read without decryption
  result <- read_parquet(temp_file)

  # Check if result is an Arrow table
  expect_true(inherits(result, "data.frame"))

  # Check the content of the data
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)

  unlink(temp_file, recursive = TRUE, force = TRUE)

})



# Test `read_parquet()` with decryption
test_that("read_parquet reads encrypted Parquet file with decryption (legacy)", {
  data <- tibble::tibble(a = 1:5, b = letters[1:5])
  temp_file <- tempfile(fileext = ".parquet")

  mock_key_legacy <- list(
    aes_key = "3e5bdf7031653ddcffca50831f6d9822f73cce44948c5a0861540cfb5620a633",
    aes_iv = "9A5D8517510F8E26C564C6C8DD39EA68"
  )

  # Write Parquet file with mock encryption
  write_parquet(data, temp_file, encryption_key = mock_key_legacy)

  # Read encrypted file with decryption
  result <- read_parquet(temp_file, decryption_key = mock_key_legacy)

  # Check if result is an Arrow table
  expect_true(inherits(result, "data.frame"))

  # Check the content of the data
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)

  unlink(temp_file, recursive = TRUE, force = TRUE)

})


# Test `read_parquet()` with decryption
test_that("read_parquet reads encrypted Parquet file with decryption", {
  data <- tibble::tibble(a = 1:5, b = letters[1:5])
  temp_file <- tempfile(fileext = ".parquet")

  mock_key <- 'rppqM5CuEqotys4wQq/g7xh6wpIjRozcAIbI9sagwKE='

  # Write Parquet file with mock encryption
  write_parquet(data, temp_file, encryption_key = mock_key)

  # Read encrypted file with decryption
  result <- read_parquet(temp_file, decryption_key = mock_key)

  # Check if result is an Arrow table
  expect_true(inherits(result, "data.frame"))

  # Check the content of the data
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)

  unlink(temp_file, recursive = TRUE, force = TRUE)

})


