# Test `set_class()`
test_that("set_class adds a class to an object", {
  obj <- data.frame(a = 1:5)
  obj <- set_class(obj, "custom_class")
  expect_true("custom_class" %in% class(obj))
})

# Test `as_rcdf()`
test_that("as_rcdf adds the 'rcdf' class", {
  obj <- data.frame(a = 1:5)
  obj_rcdf <- as_rcdf(obj)
  expect_true("rcdf" %in% class(obj_rcdf))
})


# Test `raw_to_hex()`
test_that("raw_to_hex converts raw to hex", {
  raw_val <- charToRaw("hello")
  hex_val <- raw_to_hex(raw_val)
  expect_equal(hex_val, "68656C6C6F")
  expect_error(raw_to_hex("not raw"), "Input must be a raw vector.")
})

# Test `dir_create_new()`
test_that("dir_create_new creates a new directory", {
  new_dir <- tempfile()
  created_dir <- dir_create_new(new_dir)
  expect_true(dir.exists(created_dir))
})


# Test `encrypt_string()` and `decrypt_string()`
test_that("RSA encryption and decryption work", {
  rsa_key <- openssl::rsa_keygen()
  encrypted_data <- encrypt_string("test data", rsa_key$pubkey)
  decrypted_data <- decrypt_string(encrypted_data, rsa_key)
  expect_equal(decrypted_data, "test data")
})

# Test `get_pc_metadata()`
test_that("get_pc_metadata returns valid system information", {
  pc_os <- get_pc_metadata("pc_os")
  expect_true(is.character(pc_os))
  expect_false(is.null(pc_os))
})


# Test `rcdf_list()`
test_that("rcdf_list returns an empty rcdf object", {
  obj <- rcdf_list()
  expect_s3_class(obj, "rcdf")
  expect_true(is.list(obj))
  expect_equal(length(obj), 0L)
})

test_that("rcdf_list with elements wraps them as an rcdf", {
  obj <- rcdf_list(a = 1:3, b = letters[1:3])
  expect_s3_class(obj, "rcdf")
  expect_equal(obj$a, 1:3)
})


# Test `is_rcdf()`
test_that("is_rcdf returns TRUE for rcdf objects", {
  expect_true(is_rcdf(rcdf_list()))
})

test_that("is_rcdf returns FALSE for plain lists", {
  expect_false(is_rcdf(list(a = 1)))
})

test_that("is_rcdf returns FALSE for non-list objects", {
  expect_false(is_rcdf(data.frame(a = 1)))
})


# Test `check_if_rcdf()`
test_that("check_if_rcdf wraps non-rcdf input in an rcdf list", {
  df <- data.frame(a = 1:3)
  result <- check_if_rcdf(df)
  expect_s3_class(result, "rcdf")
})

test_that("check_if_rcdf passes through an existing rcdf unchanged", {
  obj <- rcdf_list(x = 1:3)
  result <- check_if_rcdf(obj)
  expect_identical(result, obj)
})


# Test `hex_to_raw()` round-trip with `raw_to_hex()`
test_that("hex_to_raw round-trips correctly with raw_to_hex", {
  original <- charToRaw("hello")
  hex <- raw_to_hex(original)
  restored <- hex_to_raw(hex)
  expect_equal(restored, original)
})


# Test `generate_rsa_keys()`
test_that("generate_rsa_keys creates public and private key files", {
  dir <- withr::local_tempdir()
  pub_path <- generate_rsa_keys(dir)
  prv_path <- file.path(dir, "private-key.pem")
  expect_true(file.exists(pub_path))
  expect_true(file.exists(prv_path))
})

test_that("generate_rsa_keys with which='private' returns private key path", {
  dir <- withr::local_tempdir()
  prv_path <- generate_rsa_keys(dir, which = "private")
  expect_true(grepl("private", prv_path))
  expect_true(file.exists(prv_path))
})

test_that("generate_rsa_keys with password creates password-protected private key", {
  dir <- withr::local_tempdir()
  pub_path <- generate_rsa_keys(dir, password = "secret")
  prv_path <- file.path(dir, "private-key.pem")
  key <- openssl::read_key(prv_path, password = "secret")
  expect_true(inherits(key, "rsa"))
})

test_that("generate_rsa_keys with prefix names files correctly", {
  dir <- withr::local_tempdir()
  pub_path <- generate_rsa_keys(dir, prefix = "myapp")
  expect_true(grepl("myapp", basename(pub_path)))
})


# Test `generate_pw()`
test_that("generate_pw returns a string of the correct default length", {
  pw <- generate_pw()
  expect_equal(nchar(pw), 16L)
})

test_that("generate_pw respects custom length", {
  pw <- generate_pw(32)
  expect_equal(nchar(pw), 32L)
})

test_that("generate_pw without special characters produces only alphanumeric", {
  pw <- generate_pw(100, special_chr = FALSE)
  expect_true(grepl("^[A-Za-z0-9]+$", pw))
})


# Test `normalize_key_value()`
test_that("normalize_key_value handles legacy list with aes_key and aes_iv", {
  legacy <- list(aes_key = "mykey", aes_iv = "myiv")
  result <- normalize_key_value(legacy)
  expect_equal(result$key, "mykey")
  expect_equal(result$value, "myiv")
})

test_that("normalize_key_value handles base64-encoded key string", {
  b64 <- openssl::base64_encode(openssl::rand_bytes(32))
  result <- normalize_key_value(b64)
  expect_equal(result$key, "key256base64")
  expect_equal(result$value, b64)
})

test_that("normalize_key_value handles 256-bit hex key string", {
  hex_key <- paste0(rep("a", 64), collapse = "")  # 64 hex chars = 256 bits
  result <- normalize_key_value(hex_key)
  expect_equal(result$key, "key512")  # 8 * nchar(64) = 512
  expect_true(nchar(result$value) == 64L)
})

test_that("normalize_key_value returns NULL for empty list without aes_key", {
  result <- normalize_key_value(list(other = "x"))
  expect_null(result)
})

test_that("normalize_key_value throws error on invalid key string", {
  expect_error(normalize_key_value("not-a-valid-key!!"), "Invalid")
})


# Test `decrypt_info_aes()`
test_that("decrypt_info_aes decrypts without a key (plain serialised base64)", {
  original <- "plain text"
  encoded <- openssl::base64_encode(serialize(original, connection = NULL))
  result <- decrypt_info_aes(encoded, key = list())
  expect_equal(result, original)
})

test_that("decrypt_info_aes decrypts with AES-CBC key and IV", {
  original <- "secret"
  aes_key <- "3e5bdf7031653ddcffca50831f6d9822f73cce44948c5a0861540cfb5620a633"
  aes_iv  <- "9A5D8517510F8E26C564C6C8DD39EA68"
  key_raw <- openssl::sha256(charToRaw(aes_key))
  iv_raw  <- hex_to_raw(aes_iv)
  encrypted <- openssl::base64_encode(
    openssl::aes_cbc_encrypt(serialize(original, NULL), key_raw, iv = iv_raw)
  )
  result <- decrypt_info_aes(encrypted, key = list(aes_key = aes_key, aes_iv = aes_iv))
  expect_equal(result, original)
})

test_that("decrypt_info_aes decrypts with AES-CBC key and no IV", {
  original <- "no iv secret"
  aes_key <- "3e5bdf7031653ddcffca50831f6d9822f73cce44948c5a0861540cfb5620a633"
  key_raw <- openssl::sha256(charToRaw(aes_key))
  encrypted <- openssl::base64_encode(
    openssl::aes_cbc_encrypt(serialize(original, NULL), key_raw, iv = NULL)
  )
  result <- decrypt_info_aes(encrypted, key = list(aes_key = aes_key))
  expect_equal(result, original)
})
