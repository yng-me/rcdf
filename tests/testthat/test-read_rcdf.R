# Helper function to simulate an RCDF file (this is just a mock)
create_mock_rcdf <- function(temp_dir, pw = NULL) {
  # Create a mock RCDF zip file structure

  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(temp_dir, 'pub.pem'))
  openssl::write_pem(key, file.path(temp_dir, 'prv.pem'), password = pw)

  data <- list(
    dataset1 = data.frame(a = 1:5, b = letters[1:5]),
    dataset2 = data.frame(a = 6:10, b = letters[6:10])
  )

  write_rcdf(
    as_rcdf(data),
    path = file.path(temp_dir, 'mock.rcdf'),
    pub_key = file.path(temp_dir, 'pub.pem')
  )

  return(file.path(temp_dir, 'mock.rcdf'))

}


test_that("read_rcdf can read and decrypt RCDF files", {

  # Create a temporary directory for testing
  temp_dir <- tempdir()
  mock_rcdf <- create_mock_rcdf(temp_dir)

  # Test reading the RCDF
  rcdf_data <- read_rcdf(
    path = mock_rcdf,
    decryption_key = file.path(temp_dir, 'prv.pem')
  )

  # Check if the RCDF object is returned correctly
  expect_s3_class(rcdf_data, "rcdf")
  expect_true("dataset1" %in% names(rcdf_data))

  # Clean up mock RCDF file
  unlink(rcdf_data)
  unlink(file.path(temp_dir, 'prv.pem'))
  unlink(file.path(temp_dir, 'pub.pem'))

})


test_that("read_rcdf can read and decrypt RCDF files with RSA password protected key", {

  # Create a temporary directory for testing
  temp_dir <- tempdir()
  mock_rcdf <- create_mock_rcdf(temp_dir, pw = 'xxx')

  # Test reading the RCDF
  rcdf_data <- read_rcdf(
    path = mock_rcdf,
    decryption_key = file.path(temp_dir, 'prv.pem'),
    password = 'xxx'
  )

  # Check if the RCDF object is returned correctly
  expect_s3_class(rcdf_data, "rcdf")
  expect_true("dataset1" %in% names(rcdf_data))

  # Clean up mock RCDF file
  unlink(rcdf_data)
  unlink(file.path(temp_dir, 'prv.pem'))
  unlink(file.path(temp_dir, 'pub.pem'))

})


test_that("read_rcdf with return_meta = TRUE attaches metadata attribute", {
  temp_dir <- withr::local_tempdir()
  mock_rcdf <- create_mock_rcdf(temp_dir)

  rcdf_data <- read_rcdf(
    path = mock_rcdf,
    decryption_key = file.path(temp_dir, 'prv.pem'),
    return_meta = TRUE
  )

  meta <- attributes(rcdf_data)$metadata
  expect_true(is.list(meta))
  expect_true("version" %in% names(meta))
})


test_that("read_rcdf applies embedded data dictionary labels", {
  temp_dir <- withr::local_tempdir()

  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(temp_dir, 'pub.pem'))
  openssl::write_pem(key, file.path(temp_dir, 'prv.pem'))

  data <- as_rcdf(list(ds = data.frame(sex = c(1L, 2L))))
  dict <- data.frame(
    variable_name = "sex", label = "Gender", type = "categorical",
    valueset = I(list(data.frame(value = c(1L, 2L), label = c("Male", "Female"))))
  )

  write_rcdf(
    data = data,
    path = file.path(temp_dir, "labeled.rcdf"),
    pub_key = file.path(temp_dir, "pub.pem"),
    metadata = list(dictionary = dict)
  )

  rcdf_data <- read_rcdf(
    path = file.path(temp_dir, "labeled.rcdf"),
    decryption_key = file.path(temp_dir, "prv.pem")
  )

  expect_equal(attr(rcdf_data$ds$sex, "label"), "Gender")
})


test_that("read_rcdf reads all .rcdf files from a directory", {
  temp_dir <- withr::local_tempdir()
  rcdf_dir <- file.path(temp_dir, "rcdf_files")
  dir.create(rcdf_dir)

  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(temp_dir, 'pub.pem'))
  openssl::write_pem(key, file.path(temp_dir, 'prv.pem'))

  write_rcdf(as_rcdf(list(a = data.frame(x = 1:3))), file.path(rcdf_dir, "f1.rcdf"), file.path(temp_dir, "pub.pem"))
  write_rcdf(as_rcdf(list(b = data.frame(y = 4:6))), file.path(rcdf_dir, "f2.rcdf"), file.path(temp_dir, "pub.pem"))

  result <- read_rcdf(path = rcdf_dir, decryption_key = file.path(temp_dir, "prv.pem"))
  expect_s3_class(result, "rcdf")
  expect_true("a" %in% names(result))
  expect_true("b" %in% names(result))
})


test_that("read_rcdf with vector of file paths reads and merges them", {
  temp_dir <- withr::local_tempdir()

  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(temp_dir, 'pub.pem'))
  openssl::write_pem(key, file.path(temp_dir, 'prv.pem'))

  path1 <- file.path(temp_dir, "f1.rcdf")
  path2 <- file.path(temp_dir, "f2.rcdf")
  write_rcdf(as_rcdf(list(t1 = data.frame(x = 1:3))), path1, file.path(temp_dir, "pub.pem"))
  write_rcdf(as_rcdf(list(t2 = data.frame(y = 4:6))), path2, file.path(temp_dir, "pub.pem"))

  result <- read_rcdf(path = c(path1, path2), decryption_key = file.path(temp_dir, "prv.pem"))
  expect_true("t1" %in% names(result))
  expect_true("t2" %in% names(result))
})


test_that("read_rcdf errors when path does not exist", {
  expect_error(
    read_rcdf(path = "/nonexistent/path.rcdf", decryption_key = "key"),
    "does not exist"
  )
})


test_that("read_rcdf errors when file lacks .rcdf extension", {
  tmp <- tempfile(fileext = ".zip")
  writeLines("", tmp)
  on.exit(unlink(tmp))
  expect_error(
    read_rcdf(path = tmp, decryption_key = "key"),
    "valid RCDF file"
  )
})


test_that("normalize_credentials errors when key and password counts mismatch", {
  expect_error(
    normalize_credentials(
      rcdf_files     = c("a.rcdf", "b.rcdf", "c.rcdf"),
      decryption_key = c("k1", "k2"),
      password       = c("p1", "p2")
    ),
    "Mismatched"
  )
})


test_that("resolve_rcdf_files errors when directory has no .rcdf files", {
  empty_dir <- withr::local_tempdir()
  expect_error(resolve_rcdf_files(empty_dir, recursive = FALSE), "No valid RCDF")
})


test_that("read_rcdf with lazy = TRUE returns rcdf_tbl_db objects", {
  temp_dir <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(temp_dir, "pub.pem"))
  openssl::write_pem(key, file.path(temp_dir, "prv.pem"), password = NULL)

  data <- as_rcdf(list(ds = data.frame(x = 1:5)))
  write_rcdf(data, file.path(temp_dir, "test.rcdf"), pub_key = file.path(temp_dir, "pub.pem"))

  result <- read_rcdf(
    path = file.path(temp_dir, "test.rcdf"),
    decryption_key = file.path(temp_dir, "prv.pem"),
    lazy = TRUE
  )

  expect_true(inherits(result$ds, "rcdf_tbl_db"))
  collected <- collect(result$ds)
  expect_equal(nrow(collected), 5L)
})


test_that("read_rcdf with n_threads = 1 reads correctly", {
  temp_dir <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(temp_dir, "pub.pem"))
  openssl::write_pem(key, file.path(temp_dir, "prv.pem"), password = NULL)

  data <- as_rcdf(list(ds = data.frame(val = 11:15)))
  write_rcdf(data, file.path(temp_dir, "thr.rcdf"), pub_key = file.path(temp_dir, "pub.pem"))

  result <- read_rcdf(
    path = file.path(temp_dir, "thr.rcdf"),
    decryption_key = file.path(temp_dir, "prv.pem"),
    n_threads = 1L
  )

  expect_true(is.data.frame(result$ds))
  expect_equal(nrow(result$ds), 5L)
})
