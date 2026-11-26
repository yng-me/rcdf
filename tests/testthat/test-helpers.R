test_that("collect.rcdf_tbl_db applies metadata labels and returns a plain data frame", {
  # Build a real DuckDB-backed lazy table
  conn <- DBI::dbConnect(duckdb::duckdb(), config = list(threads = 1))
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  df <- data.frame(sex = c(1L, 2L, 1L), age = c(23L, 30L, 25L))
  DBI::dbWriteTable(conn, "people", df)

  lazy <- dplyr::tbl(conn, "people")
  class(lazy) <- c("rcdf_tbl_db", class(lazy))

  meta <- data.frame(
    variable_name = c("sex", "age"),
    label = c("Gender", "Age"),
    type = c("categorical", "numeric"),
    valueset = I(list(
      data.frame(value = c(1L, 2L), label = c("Male", "Female")),
      NULL
    ))
  )
  attr(lazy, "metadata") <- meta

  result <- collect(lazy)

  expect_true(is.data.frame(result))
  expect_false(inherits(result, "rcdf_tbl_db"))
  expect_equal(attr(result$sex, "label"), "Gender")
  expect_equal(attr(result$age, "label"), "Age")
  expect_true(inherits(result$sex, "haven_labelled"))
})

test_that("collect() passes plain lazy tables and data frames through to dplyr", {
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(conn, "t", data.frame(a = 1:3))

  expect_equal(collect(dplyr::tbl(conn, "t"))$a, 1:3)
  expect_equal(collect(data.frame(a = 1:3))$a, 1:3)
})

test_that("collect() on a lazy table whose connection is closed explains why", {
  conn <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbWriteTable(conn, "t", data.frame(a = 1:3))
  lazy <- dplyr::tbl(conn, "t")
  class(lazy) <- c("rcdf_tbl_db", class(lazy))
  DBI::dbDisconnect(conn, shutdown = TRUE)

  expect_error(collect(lazy), "connection behind this lazy table is closed")
})
