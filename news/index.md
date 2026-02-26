# Changelog

## rcdf 0.1.4

- Added
  [`get_attr()`](https://yng-me.github.io/rcdf/reference/get_attr.md) to
  retrieve attribute/s from RCDF object
- Added
  [`read_parquet_as_db()`](https://yng-me.github.io/rcdf/reference/read_parquet_as_db.md)
  to read encrypted Parquet file and return as lazy data from DuckDB
  connection.

## rcdf 0.1.3

CRAN release: 2026-02-12

- Added
  [`merge_rcdf()`](https://yng-me.github.io/rcdf/reference/merge_rcdf.md)
  function to combine multiple RCDF files with different encryption keys
  into one.
- Updated
  [`read_rcdf()`](https://yng-me.github.io/rcdf/reference/read_rcdf.md)
  to allow reading of multiple RCDF file with different encryption keys.
- Added `pre_collect` argument to
  [`read_rcdf()`](https://yng-me.github.io/rcdf/reference/read_rcdf.md)
  to allow lazy loading the RCDF object.

## rcdf 0.1.2

CRAN release: 2025-12-03

- Fixed [\#14](https://github.com/yng-me/rcdf/issues/14)

## rcdf 0.1.1

CRAN release: 2025-10-12

- Added a `NEWS.md` file to track changes to the package.
