lazy_pub_key <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(dir, "pub.pem"))
  openssl::write_pem(key, file.path(dir, "prv.pem"))
  list(pub = file.path(dir, "pub.pem"), prv = file.path(dir, "prv.pem"))
}

duckdb_tbl <- function(df, name) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = withr::local_tempfile(fileext = ".duckdb", .local_envir = parent.frame()))
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = parent.frame())
  DBI::dbWriteTable(con, name, df)
  dplyr::tbl(con, name)
}

sorted <- function(df) {
  df <- as.data.frame(df)
  df[do.call(order, df), , drop = FALSE] |> `rownames<-`(NULL)
}

test_that("write_rcdf streams mixed DuckDB lazy tables and data frames, round-tripping", {
  skip_if_not_installed("dbplyr")
  keys <- lazy_pub_key()

  a <- data.frame(id = 1:5, v = letters[1:5])
  b <- data.frame(id = 6:8, w = c(1.5, 2.5, 3.5))
  c <- data.frame(id = 9:10, z = c(TRUE, FALSE))

  data <- rcdf_list(
    a = duckdb_tbl(a, "a"),
    b = dplyr::filter(duckdb_tbl(b, "b"), id > 6),  # second connection, with a query
    c = c
  )

  path <- withr::local_tempfile(fileext = ".rcdf")
  write_rcdf(data, path, pub_key = keys$pub)

  out <- read_rcdf(path, decryption_key = keys$prv)
  expect_equal(sorted(out$a), sorted(a))
  expect_equal(sorted(out$b), sorted(b[b$id > 6, ]))
  expect_equal(sorted(out$c), sorted(c))
})

test_that("DuckDB lazy tables are not collected into R", {
  skip_if_not_installed("dbplyr")
  keys <- lazy_pub_key()
  data <- rcdf_list(a = duckdb_tbl(data.frame(id = 1:3), "a"))

  pk <- data.frame(file = "a", pk_field_name = "id")  # also exercises the in-database duplicate check

  local_mocked_bindings(collect = function(...) stop("collected"), .package = "dplyr")
  expect_no_error(write_rcdf(data, withr::local_tempfile(fileext = ".rcdf"), pub_key = keys$pub,
                             metadata = list(primary_key = pk)))
})

test_that("duplicate keys in a lazy record warn or stop", {
  skip_if_not_installed("dbplyr")
  keys <- lazy_pub_key()
  data <- rcdf_list(a = duckdb_tbl(data.frame(id = c(1L, 1L, 2L)), "a"))
  pk <- data.frame(file = "a", pk_field_name = "id")

  expect_warning(
    write_rcdf(data, withr::local_tempfile(fileext = ".rcdf"), pub_key = keys$pub, metadata = list(primary_key = pk)),
    "duplicates in `a`"
  )
  expect_error(
    write_rcdf(data, withr::local_tempfile(fileext = ".rcdf"), pub_key = keys$pub,
               metadata = list(primary_key = pk), ignore_duplicates = FALSE),
    "duplicates in `a`"
  )
})

test_that("lazy tables on non-DuckDB databases are collected and written", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("RSQLite")
  keys <- lazy_pub_key()

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  df <- data.frame(id = 1:4, v = letters[1:4])
  DBI::dbWriteTable(con, "s", df)

  path <- withr::local_tempfile(fileext = ".rcdf")
  write_rcdf(rcdf_list(s = dplyr::tbl(con, "s")), path, pub_key = keys$pub)
  expect_equal(sorted(read_rcdf(path, decryption_key = keys$prv)$s), sorted(df))
})

test_that("write_parquet writes an unencrypted DuckDB lazy table", {
  skip_if_not_installed("dbplyr")
  df <- data.frame(id = 1:3)
  path <- withr::local_tempfile(fileext = ".parquet")
  write_parquet(duckdb_tbl(df, "a"), path)
  expect_equal(sorted(read_parquet(path)), sorted(df))
})
