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

test_that("read_rcdf accepts an RSA private key object", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(dir_temp, "pub.pem"))
  path <- file.path(dir_temp, "x.rcdf")
  write_rcdf(rcdf_list(d = data.frame(a = 1:3)), path, file.path(dir_temp, "pub.pem"))

  expect_equal(read_rcdf(path, decryption_key = key)$d$a, 1:3)
})

test_that("read_rcdf gives a clear error for an unsupported key type", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(dir_temp, "pub.pem"))
  path <- file.path(dir_temp, "x.rcdf")
  write_rcdf(rcdf_list(d = data.frame(a = 1:3)), path, file.path(dir_temp, "pub.pem"))

  expect_error(read_rcdf(path, decryption_key = 123), "must be an RSA private key")
})

test_that("read_rcdf with ignore_duplicates = FALSE catches duplicate keys across files", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir_temp, "pub.pem")
  openssl::write_pem(key$pubkey, pub)
  p1 <- file.path(dir_temp, "a.rcdf")
  p2 <- file.path(dir_temp, "b.rcdf")
  write_rcdf(rcdf_list(d = data.frame(id = 1:3)), p1, pub)
  write_rcdf(rcdf_list(d = data.frame(id = 3:5)), p2, pub)
  pk <- data.frame(file = "d", pk_field_name = "id")

  expect_error(
    read_rcdf(c(p1, p2), decryption_key = key, metadata = list(primary_key = pk), ignore_duplicates = FALSE),
    "duplicates in `d`"
  )
  expect_equal(nrow(read_rcdf(c(p1, p2), decryption_key = key)$d), 6)
})

test_that("normalize_credentials rejects a key count that matches neither 1 nor the file count", {
  expect_error(normalize_credentials(c("a", "b", "c"), c("k1", "k2"), NULL), "Mismatched")
})

test_that("read_rcdf warns on duplicate keys when ignore_duplicates = TRUE", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir_temp, "pub.pem")
  openssl::write_pem(key$pubkey, pub)
  p1 <- file.path(dir_temp, "a.rcdf")
  p2 <- file.path(dir_temp, "b.rcdf")
  write_rcdf(rcdf_list(d = data.frame(id = 1:3)), p1, pub)
  write_rcdf(rcdf_list(d = data.frame(id = 3:5)), p2, pub)
  pk <- data.frame(file = "d", pk_field_name = "id")

  expect_warning(
    read_rcdf(c(p1, p2), decryption_key = key, metadata = list(primary_key = pk)),
    "duplicates in `d`"
  )
})

test_that("verify_rcdf_checksum stops on a modified parquet file and skips unknown files", {
  dir_temp <- withr::local_tempdir()
  f <- file.path(dir_temp, "d.parquet")
  writeLines("original", f)
  checksum <- data.frame(file = "d.parquet", value = unname(tools::md5sum(f)))

  expect_silent(verify_rcdf_checksum(f, checksum))
  writeLines("tampered", f)
  expect_error(verify_rcdf_checksum(f, checksum), "checksum mismatch")
  expect_silent(verify_rcdf_checksum(f, data.frame(file = "other.parquet", value = "x")))
  expect_silent(verify_rcdf_checksum(f, NULL))
})

test_that("a primary key given as a record name is checked at write and stored for read", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir_temp, "pub.pem")
  openssl::write_pem(key$pubkey, pub)
  path <- file.path(dir_temp, "x.rcdf")
  dup <- rcdf_list(d = data.frame(id = c(1, 1, 2)))

  expect_warning(
    write_rcdf(dup, path, pub, metadata = list(primary_key = data.frame(file = "d", pk_field_name = "id"))),
    "duplicates in `d`"
  )
  expect_equal(get_attrs(path)$checksum$pk_field_name, "id")
  expect_error(read_rcdf(path, decryption_key = key, ignore_duplicates = FALSE), "duplicates in `d`")
})

test_that("a primary key given as a parquet file name is checked at write", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir_temp, "pub.pem")
  openssl::write_pem(key$pubkey, pub)

  expect_error(
    write_rcdf(rcdf_list(d = data.frame(id = c(1, 1))), file.path(dir_temp, "x.rcdf"), pub,
               metadata = list(primary_key = data.frame(file = "d.parquet", pk_field_name = "id")),
               ignore_duplicates = FALSE),
    "duplicates in `d`"
  )
})

test_that("merge_by_name stacks data frames with different columns and keeps unnamed values", {
  merged <- merge_by_name(data.frame(a = 1), data.frame(a = 2, b = "x"))
  expect_equal(merged$a, c(1, 2))
  expect_equal(merged$b, c(NA, "x"))
  expect_equal(merge_by_name(c(1, 2), c(3, 4)), c(1, 2, 3, 4))
  expect_equal(merge_by_name(list(n = data.frame(v = 1)), list(n = data.frame(v = 2)))$n$v, c(1, 2))
})

test_that("read_rcdf keeps records with zero rows", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir_temp, "pub.pem")
  openssl::write_pem(key$pubkey, pub)
  path <- file.path(dir_temp, "x.rcdf")
  write_rcdf(rcdf_list(full = data.frame(a = 1:2), empty = data.frame(a = integer(0))), path, pub)

  out <- read_rcdf(path, decryption_key = key)
  expect_setequal(names(out), c("full", "empty"))
  expect_equal(nrow(out$empty), 0L)
})

test_that("read_rcdf combines area_names whose columns differ between files", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir_temp, "pub.pem")
  openssl::write_pem(key$pubkey, pub)
  p1 <- file.path(dir_temp, "a.rcdf")
  p2 <- file.path(dir_temp, "b.rcdf")
  write_rcdf(rcdf_list(d = data.frame(x = 1)), p1, pub, metadata = list(area_names = data.frame(code = "01")))
  write_rcdf(rcdf_list(d = data.frame(x = 2)), p2, pub, metadata = list(area_names = data.frame(code = "02", name = "B")))

  areas <- attr(read_rcdf(c(p1, p2), decryption_key = key, return_meta = TRUE), "metadata")$area_names
  expect_equal(areas$code, c("01", "02"))
  expect_equal(areas$name, c(NA, "B"))
})

test_that("the documented call closes a lazy read's connection", {
  skip_if_not_installed("dbplyr")
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir_temp, "pub.pem")
  openssl::write_pem(key$pubkey, pub)
  path <- file.path(dir_temp, "x.rcdf")
  write_rcdf(rcdf_list(d = data.frame(x = 1:3)), path, pub)

  x <- read_rcdf(path, decryption_key = key, lazy = TRUE)
  con <- dbplyr::remote_con(x[[1]])
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_false(DBI::dbIsValid(con))
})

test_that("lazy read works on an archive that stores a dictionary", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir_temp, "pub.pem")
  openssl::write_pem(key$pubkey, pub)
  path <- file.path(dir_temp, "x.rcdf")
  dict <- data.frame(variable_name = "a", label = "Label A", type = "numeric")
  write_rcdf(rcdf_list(d = data.frame(a = 1:3)), path, pub, metadata = list(dictionary = dict))

  x <- read_rcdf(path, decryption_key = key, lazy = TRUE)
  withr::defer(DBI::dbDisconnect(dbplyr::remote_con(x$d), shutdown = TRUE))
  out <- collect(x$d)
  expect_equal(as.vector(out$a), 1:3)
  expect_equal(attr(out$a, "label"), "Label A")
})

test_that("records from several files are combined by column name, not position", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir_temp, "pub.pem")
  openssl::write_pem(key$pubkey, pub)
  p1 <- file.path(dir_temp, "a.rcdf")
  p2 <- file.path(dir_temp, "b.rcdf")
  write_rcdf(rcdf_list(d = data.frame(x = 1:2, y = c(10L, 20L))), p1, pub)
  write_rcdf(rcdf_list(d = data.frame(y = c(30L, 40L), x = 3:4)), p2, pub)

  for (lazy in c(FALSE, TRUE)) {
    out <- read_rcdf(c(p1, p2), decryption_key = key, lazy = lazy)$d
    if (lazy) {
      con <- dbplyr::remote_con(out)
      out <- collect(out)
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
    out <- out[order(out$x), ]
    expect_equal(out$x, 1:4)
    expect_equal(out$y, c(10L, 20L, 30L, 40L))
  }
})

test_that("merge_by_name never drops a side", {
  expect_equal(merge_by_name(list(note = "a"), list(note = "a")), list(note = "a"))
  expect_equal(merge_by_name(list(note = "a"), list(note = "b")), list(note = c("a", "b")))
  expect_equal(merge_by_name(list(x = list(y = 1)), list(x = list(z = 2))), list(x = list(y = 1, z = 2)))
  expect_equal(merge_by_name(list(n = 1), c(5, 6)), list(list(n = 1), c(5, 6)))
})

test_that("read_rcdf checks a primary key stored in any archive, not only the last", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir_temp, "pub.pem")
  openssl::write_pem(key$pubkey, pub)
  p1 <- file.path(dir_temp, "a.rcdf")
  p2 <- file.path(dir_temp, "b.rcdf")
  write_rcdf(rcdf_list(d = data.frame(id = 1:3)), p1, pub,
             metadata = list(primary_key = data.frame(file = "d", pk_field_name = "id")))
  write_rcdf(rcdf_list(d = data.frame(id = 3:5)), p2, pub)

  expect_error(read_rcdf(c(p1, p2), decryption_key = key, ignore_duplicates = FALSE), "duplicates in `d`")
})
