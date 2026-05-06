# Changelog

## rcdf 0.1.6

#### Security fixes

- [`generate_pw()`](https://yng-me.github.io/rcdf/reference/generate_pw.md):
  switched from Mersenne Twister to a cryptographically secure RNG
  ([`openssl::rand_bytes`](https://jeroen.r-universe.dev/openssl/reference/rand_bytes.html)).
- [`write_parquet()`](https://yng-me.github.io/rcdf/reference/write_parquet.md):
  DuckDB errors no longer leak raw encryption key material in messages.
- [`write_rcdf_parquet()`](https://yng-me.github.io/rcdf/reference/write_rcdf_parquet.md):
  plaintext temp table is now always removed after a shared-connection
  write.
- [`write_rcdf_as()`](https://yng-me.github.io/rcdf/reference/write_rcdf_as.md):
  replaced `eval(parse())` dispatch with a safe named-list of function
  references.
- `hex_to_raw()`: added strict input validation (type, length, character
  set).

#### Performance enhancements

- [`write_parquet()`](https://yng-me.github.io/rcdf/reference/write_parquet.md)
  /
  [`write_rcdf_parquet()`](https://yng-me.github.io/rcdf/reference/write_rcdf_parquet.md):
  removed `httpfs` overhead; shared DuckDB connection across writes;
  default compression changed to `zstd`.
- [`read_rcdf()`](https://yng-me.github.io/rcdf/reference/read_rcdf.md):
  new `lazy = TRUE` option for DuckDB-backed lazy reads; `n_threads` for
  parallel I/O; duplicate `PRAGMA` calls deduplicated per connection.
- [`add_metadata()`](https://yng-me.github.io/rcdf/reference/add_metadata.md):
  label assignment vectorised with batch update.
- `open_duckdb_connection()`: no longer forces single-threaded mode.

#### Bug fixes

- [`collect.rcdf_tbl_db()`](https://yng-me.github.io/rcdf/reference/collect.md):
  fixed infinite-dispatch recursion; `NULL` metadata no longer causes an
  error.
- [`read_parquet()`](https://yng-me.github.io/rcdf/reference/read_parquet.md):
  unencrypted early-return path now correctly applies `metadata` and
  `as_arrow_table`.
- `normalize_key_value()`: hex keys no longer misidentified as base-64.

#### Documentation

- Main vignette rewritten for general/non-technical users with a
  step-by-step workflow.
- Merging vignette completed and fixed.

## rcdf 0.1.5

CRAN release: 2026-03-07

#### Feature and update

- Added `encrypt_value()` and `decrypt_value()` functions, respectively,
  to encrypt and decrypt string using RSA.
- Changed “dot env” (`.env`) reader from
  [`read_env()`](https://yng-me.github.io/rcdf/reference/read_env.md) to
  [`read_dot_env()`](https://yng-me.github.io/rcdf/reference/read_dot_env.md).

#### Bug fix

- Fixed bugs not returning metadata attributes using either
  [`read_rcdf()`](https://yng-me.github.io/rcdf/reference/read_rcdf.md)
  and
  [`get_rcdf_metadata()`](https://yng-me.github.io/rcdf/reference/get_rcdf_metadata.md).

## rcdf 0.1.4

CRAN release: 2026-03-03

#### Features

- Added
  [`get_attr()`](https://yng-me.github.io/rcdf/reference/get_attr.md) to
  retrieve attribute/s from RCDF object.
- Added
  [`read_parquet_tbl()`](https://yng-me.github.io/rcdf/reference/read_parquet_tbl.md)
  to read encrypted Parquet file and return as lazy data from DuckDB
  connection.

#### Breaking change

- Removed `pre_collect` argument in
  [`read_rcdf()`](https://yng-me.github.io/rcdf/reference/read_rcdf.md)

#### Enhancements

- Improved SQL query string interpolation for all DuckDB related codes.

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
