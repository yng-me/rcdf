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


# Test `encrypt_info_rsa()` and `decrypt_info_rsa()`
test_that("RSA encryption and decryption work", {
  rsa_key <- openssl::rsa_keygen()
  encrypted_data <- encrypt_info_rsa("test data", rsa_key$pubkey)
  decrypted_data <- decrypt_info_rsa(encrypted_data, rsa_key)
  expect_equal(decrypted_data, "test data")
})

# Test `get_pc_metadata()`
test_that("get_pc_metadata returns valid system information", {
  pc_os <- get_pc_metadata("pc_os")
  expect_true(is.character(pc_os))
  expect_false(is.null(pc_os))
})
