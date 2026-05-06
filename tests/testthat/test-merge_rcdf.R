create_rcdf_for_merge <- function(dir, name, data_list) {
  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(dir, paste0(name, "-pub.pem")))
  openssl::write_pem(key, file.path(dir, paste0(name, "-prv.pem")))
  path <- file.path(dir, paste0(name, ".rcdf"))
  write_rcdf(as_rcdf(data_list), path, file.path(dir, paste0(name, "-pub.pem")))
  list(
    path = path,
    pub  = file.path(dir, paste0(name, "-pub.pem")),
    prv  = file.path(dir, paste0(name, "-prv.pem"))
  )
}


test_that("merge_rcdf with explicit pub_key merges files and round-trips data", {
  dir <- withr::local_tempdir()

  f1 <- create_rcdf_for_merge(dir, "f1", list(t1 = data.frame(a = 1:3)))
  f2 <- create_rcdf_for_merge(dir, "f2", list(t2 = data.frame(b = 4:6)))

  merge_pub_key <- openssl::rsa_keygen()
  openssl::write_pem(merge_pub_key$pubkey, file.path(dir, "merge-pub.pem"))
  openssl::write_pem(merge_pub_key, file.path(dir, "merge-prv.pem"))

  merged_path <- file.path(dir, "merged.rcdf")
  merge_rcdf(
    rcdf_files    = c(f1$path, f2$path),
    decryption_keys = c(f1$prv, f2$prv),
    passwords     = c(NULL, NULL),
    merged_file_path = merged_path,
    pub_key       = file.path(dir, "merge-pub.pem")
  )

  expect_true(file.exists(merged_path))

  result <- read_rcdf(merged_path, decryption_key = file.path(dir, "merge-prv.pem"))
  expect_s3_class(result, "rcdf")
  expect_true("t1" %in% names(result))
  expect_true("t2" %in% names(result))
})


test_that("merge_rcdf without pub_key auto-generates a key pair and writes a valid file", {
  dir <- withr::local_tempdir()

  f1 <- create_rcdf_for_merge(dir, "g1", list(ds = data.frame(x = 1:5)))

  merged_path <- file.path(dir, "auto_merged.rcdf")

  suppressMessages(
    merge_rcdf(
      rcdf_files      = f1$path,
      decryption_keys = f1$prv,
      passwords       = NULL,
      merged_file_path = merged_path
    )
  )

  expect_true(file.exists(merged_path))
  # generated private key should sit next to merged file
  expect_true(file.exists(file.path(dir, "auto_merged-private-key.pem")))
})


test_that("merge_rcdf auto-appends .rcdf extension when missing", {
  dir <- withr::local_tempdir()

  f1 <- create_rcdf_for_merge(dir, "h1", list(ds = data.frame(x = 1:3)))

  merge_pub_key <- openssl::rsa_keygen()
  openssl::write_pem(merge_pub_key$pubkey, file.path(dir, "m-pub.pem"))
  openssl::write_pem(merge_pub_key, file.path(dir, "m-prv.pem"))

  merged_no_ext <- file.path(dir, "merged_no_ext")
  merge_rcdf(
    rcdf_files      = f1$path,
    decryption_keys = f1$prv,
    passwords       = NULL,
    merged_file_path = merged_no_ext,
    pub_key         = file.path(dir, "m-pub.pem")
  )

  expect_true(file.exists(paste0(merged_no_ext, ".rcdf")))
})
