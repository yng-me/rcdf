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



test_that("merge_rcdf ratains the correct metadata of merged .rcdf files", {

  rcdf_random <- function() {
    rcdf_list(
      d1 = data.frame(
        a = sample(1:100, size = 5),
        b = sample(LETTERS, size = 5)
      ),
      d2 = data.frame(
        a = sample(1:100, size = 5),
        b = sample(LETTERS, size = 5)
      )
    )
  }

  temp_dir <- tempdir()
  pub_key <- file.path(temp_dir, 'pub.pem')
  prv_key <- file.path(temp_dir, 'prv.pem')
  pw <- '1234'

  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, pub_key)
  openssl::write_pem(key, prv_key, password = pw)

  rcdf1 <- rcdf_random()
  rcdf2 <- rcdf_random()

  rcdf_path1 <- tempfile(fileext = '.rcdf')
  rcdf_path2 <- tempfile(fileext = '.rcdf')

  write_rcdf(
    rcdf1,
    path = rcdf_path1,
    metadata = list(
      meta = list(
        id = data.frame(
          code = c(1, 2),
          label = c("A", "B")
        )
      ),
      area_names = data.frame(
        a = c(1, 2),
        b = c('a', 'b'),
        c = c('aa', 'bb')
      )
    ),
    pub_key = pub_key
  )

  get_rcdf_metadata(rcdf_path1, 'id')

  write_rcdf(
    rcdf2,
    path = rcdf_path2,
    metadata = list(
      meta = list(
        id = data.frame(
          code = c(3, 4),
          label = c("C", "D")
        )
      ),
      area_names = data.frame(
        a = c(3, 4),
        b = c('c', 'd'),
        c = c('cc', 'dd')
      )
    ),
    pub_key = pub_key
  )

  merged_rcdf_file <- tempfile(fileext = '.rcdf')

  merge_rcdf(
    rcdf_files = c(rcdf_path1, rcdf_path2),
    decryption_keys = prv_key,
    passwords = pw,
    merged_file_path = merged_rcdf_file,
    pub_key = pub_key
  )

  final_rcdf <- read_rcdf(
    path = merged_rcdf_file,
    decryption_key = prv_key,
    password = pw
  )

  expect_true(class(final_rcdf) %in% "rcdf")

  meta_area_names <- get_rcdf_metadata(
    merged_rcdf_file,
    name = 'area_names'
  )

  expect_identical(
    meta_area_names,
    data.frame(
      a = 1:4,
      b = letters[1:4],
      c = c("aa", "bb", "cc", "dd")
    )
  )

})










