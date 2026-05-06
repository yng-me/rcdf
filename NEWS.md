# rcdf 0.1.6

### Security fixes

* `generate_pw()`: switched from Mersenne Twister to a cryptographically secure RNG (`openssl::rand_bytes`).
* `write_parquet()`: DuckDB errors no longer leak raw encryption key material in messages.
* `write_rcdf_parquet()`: plaintext temp table is now always removed after a shared-connection write.
* `write_rcdf_as()`: replaced `eval(parse())` dispatch with a safe named-list of function references.
* `hex_to_raw()`: added strict input validation (type, length, character set).

### Performance enhancements

* `write_parquet()` / `write_rcdf_parquet()`: removed `httpfs` overhead; shared DuckDB connection across writes; default compression changed to `zstd`.
* `read_rcdf()`: new `lazy = TRUE` option for DuckDB-backed lazy reads; `n_threads` for parallel I/O; duplicate `PRAGMA` calls deduplicated per connection.
* `add_metadata()`: label assignment vectorised with batch update.
* `open_duckdb_connection()`: no longer forces single-threaded mode.

### Bug fixes

* `collect.rcdf_tbl_db()`: fixed infinite-dispatch recursion; `NULL` metadata no longer causes an error.
* `read_parquet()`: unencrypted early-return path now correctly applies `metadata` and `as_arrow_table`.
* `normalize_key_value()`: hex keys no longer misidentified as base-64.

### Documentation

* Main vignette rewritten for general/non-technical users with a step-by-step workflow.
* Merging vignette completed and fixed.


# rcdf 0.1.5

### Feature and update

* Added `encrypt_value()` and `decrypt_value()` functions, respectively, to encrypt and decrypt string using RSA.
* Changed "dot env" (`.env`) reader from `read_env()` to `read_dot_env()`.

### Bug fix

* Fixed bugs not returning metadata attributes using either `read_rcdf()` and `get_rcdf_metadata()`.


# rcdf 0.1.4

### Features

* Added `get_attr()` to retrieve attribute/s from RCDF object.
* Added `read_parquet_tbl()` to read encrypted Parquet file and return as lazy data from DuckDB connection.

### Breaking change
* Removed `pre_collect` argument in `read_rcdf()`

### Enhancements

* Improved SQL query string interpolation for all DuckDB related codes.

# rcdf 0.1.3

* Added `merge_rcdf()` function to combine multiple RCDF files with different encryption keys into one.
* Updated `read_rcdf()` to allow reading of multiple RCDF file with different encryption keys.
* Added `pre_collect` argument to `read_rcdf()` to allow lazy loading the RCDF object.

# rcdf 0.1.2

* Fixed #14 

# rcdf 0.1.1

* Added a `NEWS.md` file to track changes to the package.

