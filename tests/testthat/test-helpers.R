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
    label         = c("Gender", "Age"),
    type          = c("categorical", "numeric"),
    valueset      = I(list(
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
