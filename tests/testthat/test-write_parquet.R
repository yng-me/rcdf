# Test `write_parquet()` with encryption
test_that("write_parquet writes encrypted Parquet files (legacy)", {
  data <- tibble::tibble(a = 1:5, b = letters[1:5])
  temp_file <- tempfile(fileext = ".parquet")

  # Writing without encryption
  mock_key_legacy <- list(
    aes_key = "3e5bdf7031653ddcffca50831f6d9822f73cce44948c5a0861540cfb5620a633",
    aes_iv = "9A5D8517510F8E26C564C6C8DD39EA68"
  )
  write_parquet(data, temp_file, encryption_key = mock_key_legacy)

  # Check if file is created
  expect_true(file.exists(temp_file))
  unlink(temp_file, force = TRUE)
})


test_that("write_parquet writes encrypted Parquet files", {
  data <- tibble::tibble(a = 1:5, b = letters[1:5])
  temp_file <- tempfile(fileext = ".parquet")

  # Writing without encryption
  mock_key <- 'rppqM5CuEqotys4wQq/g7xh6wpIjRozcAIbI9sagwKE='
  write_parquet(data, temp_file, encryption_key = mock_key)

  # Check if file is created
  expect_true(file.exists(temp_file))
  unlink(temp_file, force = TRUE)
})



# Test `write_parquet()` without encryption
test_that("write_parquet writes regular Parquet files", {
  data <- tibble::tibble(a = 1:5, b = letters[1:5])
  temp_file <- tempfile(fileext = ".parquet")

  # Writing without encryption
  write_parquet(data, temp_file)

  # Check if file is created
  expect_true(file.exists(temp_file))
  unlink(temp_file, force = TRUE)
})



# Test `write_rcdf_parquet()` with RCDF data
test_that("write_rcdf_parquet writes RCDF data to Parquet files", {

  data_list <- list(
    df1 = tibble::tibble(a = 1:5),
    df2 = tibble::tibble(b = 6:10)
  )

  data_list <- as_rcdf(data_list)

  temp_dir <- tempfile()

  # Writing the RCDF data
  written_files <- write_rcdf_parquet(data_list, temp_dir)

  # Check if the correct files were written
  expect_true(all(grepl("\\.parquet$", written_files)))

  # Check if the files are located in the temp_dir
  expect_true(all(file.exists(written_files)))
  unlink(written_files, force = TRUE)

})


test_that("write_rcdf_parquet with primary_key passes silently when no duplicates", {
  data_list <- as_rcdf(list(
    df1 = tibble::tibble(id = 1:5, val = letters[1:5])
  ))
  pk <- data.frame(file = "df1", pk_field_name = "id")
  temp_dir <- tempfile()

  expect_no_warning(write_rcdf_parquet(data_list, temp_dir, primary_key = pk))

  unlink(temp_dir, recursive = TRUE, force = TRUE)
})


test_that("check_duplicates warns when ignore_duplicates = TRUE", {
  data_with_dupes <- tibble::tibble(id = c(1L, 1L, 2L), val = c("a", "b", "c"))
  pk <- data.frame(file = "df1", pk_field_name = "id")

  expect_warning(
    check_duplicates(data_with_dupes, record = "df1", primary_key = pk, ignore_duplicates = TRUE),
    "duplicates"
  )
})


test_that("check_duplicates aborts when ignore_duplicates = FALSE", {
  data_with_dupes <- tibble::tibble(id = c(1L, 1L, 2L), val = c("a", "b", "c"))
  pk <- data.frame(file = "df1", pk_field_name = "id")

  expect_error(
    check_duplicates(data_with_dupes, record = "df1", primary_key = pk, ignore_duplicates = FALSE),
    "duplicates"
  )
})



test_that("write_parquet uses zstd compression by default for unencrypted files", {
  data <- tibble::tibble(x = 1:1000L)
  f_zstd   <- tempfile(fileext = ".parquet")
  f_snappy <- tempfile(fileext = ".parquet")
  on.exit({ unlink(f_zstd, force = TRUE); unlink(f_snappy, force = TRUE) }, add = TRUE)

  write_parquet(data, f_zstd)
  write_parquet(data, f_snappy, compression = "snappy")

  expect_true(file.exists(f_zstd))
  expect_true(file.exists(f_snappy))
  # zstd-compressed file should be no larger than snappy for repetitive integer data
  expect_lte(file.size(f_zstd), file.size(f_snappy))
})


test_that("write_parquet encryption works via shared caller-supplied connection", {
  data <- tibble::tibble(a = 1:10)
  key  <- "rppqM5CuEqotys4wQq/g7xh6wpIjRozcAIbI9sagwKE="
  f    <- tempfile(fileext = ".parquet")
  on.exit(unlink(f, force = TRUE), add = TRUE)

  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  write_parquet(data, f, encryption_key = key, conn = conn)
  expect_true(file.exists(f))
  # The shared conn must still be usable after the call
  expect_true(DBI::dbIsValid(conn))
})
