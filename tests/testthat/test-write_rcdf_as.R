# test-write_rcdf.R
# Minimal mock RCDF object: named list of lazy tibbles
mock_rcdf <- as_rcdf(
  list(
    dataset1 = tibble::tibble(a = 1:3, b = letters[1:3]),
    dataset2 = tibble::tibble(x = rnorm(3), y = LETTERS[1:3])
  )
)

test_that("write_rcdf_csv writes .csv files", {
  dir <- withr::local_tempdir()
  write_rcdf_csv(mock_rcdf, dir)

  expect_true(file.exists(file.path(dir, "dataset1.csv")))
  expect_true(file.exists(file.path(dir, "dataset2.csv")))

  unlink(file.path(dir, "dataset1.csv"), force = TRUE)
  unlink(file.path(dir, "dataset2.csv"), force = TRUE)
})


test_that("write_rcdf_tsv writes .txt files", {
  dir <- withr::local_tempdir()
  write_rcdf_tsv(mock_rcdf, dir)

  expect_true(file.exists(file.path(dir, "dataset1.txt")))
  expect_true(file.exists(file.path(dir, "dataset2.txt")))

  unlink(file.path(dir, "dataset1.txt"), force = TRUE)
  unlink(file.path(dir, "dataset2.txt"), force = TRUE)
})

test_that("write_rcdf_json writes .json files", {
  dir <- withr::local_tempdir()
  write_rcdf_json(mock_rcdf, dir)

  expect_true(file.exists(file.path(dir, "dataset1.json")))
  expect_true(file.exists(file.path(dir, "dataset2.json")))

  unlink(file.path(dir, "dataset1.json"), force = TRUE)
  unlink(file.path(dir, "dataset2.json"), force = TRUE)

})

test_that("write_rcdf_xlsx writes .xlsx files", {
  dir <- withr::local_tempdir()
  write_rcdf_xlsx(mock_rcdf, dir)

  expect_true(file.exists(file.path(dir, "dataset1.xlsx")))
  expect_true(file.exists(file.path(dir, "dataset2.xlsx")))

  unlink(file.path(dir, "dataset1.xlsx"), force = TRUE)
  unlink(file.path(dir, "dataset2.xlsx"), force = TRUE)

})

test_that("write_rcdf_dta writes .dta files", {
  dir <- withr::local_tempdir()
  write_rcdf_dta(mock_rcdf, dir)

  expect_true(file.exists(file.path(dir, "dataset1.dta")))
  expect_true(file.exists(file.path(dir, "dataset2.dta")))

  unlink(file.path(dir, "dataset1.dta"), force = TRUE)
  unlink(file.path(dir, "dataset2.dta"), force = TRUE)

})

test_that("write_rcdf_sav writes .sav files", {
  dir <- withr::local_tempdir()
  write_rcdf_sav(mock_rcdf, dir)

  expect_true(file.exists(file.path(dir, "dataset1.sav")))
  expect_true(file.exists(file.path(dir, "dataset2.sav")))

  unlink(file.path(dir, "dataset1.sav"), force = TRUE)
  unlink(file.path(dir, "dataset2.sav"), force = TRUE)

})

test_that("write_rcdf_sqlite writes .db file with tables", {
  dir <- withr::local_tempdir()
  db_path <- file.path(dir, "my_db_test.db")
  write_rcdf_sqlite(mock_rcdf, dir, db_name = "my_db_test")

  expect_true(file.exists(db_path))

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  tables <- DBI::dbListTables(conn)
  expect_true(all(c("dataset1", "dataset2") %in% tables))

  unlink(db_path, force = TRUE)

})

test_that("write_rcdf_as writes to multiple formats", {

  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  write_rcdf_as(mock_rcdf, dir, formats = c("csv", "json"))

  csv_path_1 <- file.path(dir, "CSV", "dataset1.csv")
  csv_path_2 <- file.path(dir, "CSV", "dataset2.csv")
  json_path_1 <- file.path(dir, "JSON", "dataset1.json")
  json_path_2 <- file.path(dir, "JSON", "dataset2.json")

  expect_true(file.exists(csv_path_1))
  expect_true(file.exists(csv_path_2))
  expect_true(file.exists(json_path_1))
  expect_true(file.exists(json_path_2))

  unlink(csv_path_1, force = TRUE)
  unlink(csv_path_2, force = TRUE)
  unlink(json_path_1, force = TRUE)
  unlink(json_path_2, force = TRUE)

})

test_that("write_rcdf_as throws error on invalid formats", {
  expect_error(
    write_rcdf_as(mock_rcdf, tempdir(), formats = c("invalidformat")),
    "invalid format"
  )
})
