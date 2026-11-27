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

test_that("write_rcdf does not pack leftover parquet files from tempdir()/lineage", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(dir_temp, "pub.pem"))

  stale_dir <- file.path(tempdir(), "lineage")
  dir.create(stale_dir, showWarnings = FALSE)
  stale <- file.path(stale_dir, "ghost.parquet")
  writeLines("stale", stale)
  withr::defer(unlink(stale))

  path <- file.path(dir_temp, "x.rcdf")
  write_rcdf(rcdf_list(real = data.frame(a = 1:2)), path, file.path(dir_temp, "pub.pem"))

  expect_equal(get_attrs(path)$checksum$file, "real.parquet")
  expect_equal(names(read_rcdf(path, decryption_key = key)), "real")
})

test_that("write_rcdf keeps a dictionary given as a file path", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(dir_temp, "pub.pem"))

  dict_path <- file.path(dir_temp, "dict.csv")
  utils::write.csv(
    data.frame(variable_name = "a", label = "Label A", type = "numeric"),
    dict_path, row.names = FALSE
  )

  path <- file.path(dir_temp, "x.rcdf")
  write_rcdf(rcdf_list(d = data.frame(a = 1:2)), path, file.path(dir_temp, "pub.pem"),
             metadata = list(dictionary = dict_path))

  expect_equal(get_attrs(path)$dictionary$label, "Label A")
})

test_that("write_rcdf stores archive entries without zip compression", {
  dir_temp <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir_temp, "pub.pem")
  openssl::write_pem(key$pubkey, pub)
  path <- file.path(dir_temp, "x.rcdf")
  write_rcdf(rcdf_list(d = data.frame(a = rep(1:10, 1000))), path, pub)

  entries <- zip::zip_list(path)
  expect_equal(entries$compressed_size, entries$uncompressed_size)
})


test_that("write_rcdf stops on a dictionary that lacks required columns", {
  dir <- withr::local_tempdir()
  pub <- create_mock_pub_key(dir)
  dict <- data.frame(variable = "a", label = "A", type = "numeric")
  expect_error(
    write_rcdf(rcdf_list(d = data.frame(a = 1)), file.path(dir, "x.rcdf"), pub, metadata = list(dictionary = dict)),
    "missing required column\\(s\\): variable_name"
  )
  expect_error(
    write_rcdf(rcdf_list(d = data.frame(a = 1)), file.path(dir, "y.rcdf"), pub,
               metadata = list(dictionary = file.path(dir, "dict.tsv"))),
    "Unsupported dictionary file type"
  )
})

test_that("write_rcdf keeps factor and labelled columns through the encrypted write", {
  dir <- withr::local_tempdir()
  key <- openssl::rsa_keygen()
  pub <- file.path(dir, "pub.pem")
  openssl::write_pem(key$pubkey, pub)
  df <- data.frame(f = factor(c("x", "y", "x")), n = c(1.5, NA, 3))
  df$l <- haven::labelled(c(1L, 2L, 1L), labels = c(Yes = 1L, No = 2L))
  path <- file.path(dir, "t.rcdf")
  write_rcdf(rcdf_list(d = df), path, pub)

  out <- read_rcdf(path, decryption_key = key)$d
  expect_equal(as.character(out$f), c("x", "y", "x"))
  expect_equal(out$n, c(1.5, NA, 3))
  expect_equal(as.vector(out$l), c(1, 2, 1))
})

test_that("write_rcdf stores created_at in UTC", {
  dir <- withr::local_tempdir()
  pub <- create_mock_pub_key(dir)
  path <- file.path(dir, "x.rcdf")
  write_rcdf(rcdf_list(d = data.frame(a = 1)), path, pub)
  expect_match(get_attrs(path)$created_at, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} UTC$")
})

test_that("pack_rcdf removes the staged parquet files once they are zipped", {
  staging <- withr::local_tempdir()
  dir.create(rcdf_lineage_dir(staging))
  writeLines("x", file.path(rcdf_lineage_dir(staging), "d.parquet"))
  path <- file.path(withr::local_tempdir(), "x.rcdf")

  pack_rcdf(staging, list(log_id = "abc"), path)

  expect_false(dir.exists(rcdf_lineage_dir(staging)))
  ext <- extract_rcdf(path)
  on.exit(ext$cleanup(), add = TRUE)
  expect_equal(basename(ext$files), "d.parquet")
})
