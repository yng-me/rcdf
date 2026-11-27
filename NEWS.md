# rcdf 0.2.2

### New features

* Larger-than-memory data. `write_rcdf()` accepts lazy DuckDB tables (e.g. `dplyr::tbl(con, "x")`) and writes them without loading them into R. `read_rcdf(lazy = TRUE)` no longer loads the data either: nothing is read until you `collect()`, so filter or summarise first. Lazy tables from other databases still work but are loaded into R first.
* `read_rcdf()` accepts an RSA private key file with any name, not only `.pem`.

### Breaking changes

* `write_rcdf()` now stops with an error when `metadata$dictionary` is missing `variable_name`, `label` or `type`, or is an unsupported file type. Before, the dictionary was silently left out.
* `created_at` and `updated_at` are now recorded in UTC, e.g. `2026-11-26 02:00:00 UTC`.

### Bug fixes

* `collect()` works on ordinary lazy tables again when rcdf is loaded after dplyr.
* `collect()` on a lazy table whose connection was closed now explains why. All tables from one `read_rcdf(lazy = TRUE)` call share one connection, so closing it closes them all.
* A failed `read_rcdf()` (e.g. disk full) no longer leaves large temporary files behind.

### Security

* `read_rcdf()` rejects archives that try to write files outside its temporary folder.

### Performance

* `write_rcdf()` no longer keeps an extra copy of your data in memory, and uses less temporary disk space.
* `get_attrs()` and `get_rcdf_metadata()` are fast on large archives, since they no longer unpack the data.

### Notes

* Lazy reads and all writes use `tempdir()`. For very large data, point `TMPDIR` to a drive with plenty of free space: a lazy read needs about twice the archive size while opening and keeps about one copy until R exits.

# rcdf 0.2.1

### New behaviour

* `read_rcdf()` checks each Parquet file against the md5 checksum stored in the archive and stops if it differs. This detects corruption, not deliberate tampering. Files the checksum doesn't list are skipped, so older archives still read.
* `read_rcdf(ignore_duplicates = TRUE)` now warns about duplicate primary keys, as documented (it previously did nothing).
* `read_rcdf()` checks primary keys stored in every archive it reads, not only the last one.
* `merge_rcdf()` keeps `summary_statistics` and the earliest source `created_at`; the merge time is recorded as `updated_at`.
* `write_rcdf()` accepts `metadata$created_at` to keep an original creation time; the write time is then stored as `updated_at`.

### Breaking changes

* `merge_rcdf()` now returns `invisible(list(path, password))` instead of `NULL`. When it generates a new key pair, the private-key password is printed only in interactive sessions, so it no longer lands in job logs; scripts should read it from the return value.
* `write_rcdf_csv()` and `write_rcdf_tsv()` no longer write a row-name column. TSV headers now line up with the data in non-R tools.
* `merge_rcdf()` / `read_rcdf(return_meta = TRUE)`: metadata from several archives is combined without dropping values. Data frames are stacked, named lists merge key by key, and plain values are combined (repeats removed), so a value that differs between files now becomes a vector instead of keeping only the last file's value.
* The unused `rcdf.options` option set at package load has been removed.

### Security

* Decrypted key fields from an archive are only unserialized if they are a plain character vector. Before, anyone holding the public key could craft an archive that runs code when read on R < 4.4 (CVE-2024-27322).

### Performance

Measured on 2.5M rows across two records (2M-row person table), with a dictionary and primary keys:

* `write_rcdf()`: about 4x faster (3.4s -> 0.8-0.95s) and archives about 45% smaller (39.8 MB -> 21.9 MB). Archives are now stored without zip compression (the encrypted data does not compress, and deflate made zipping about 10x slower), encrypted Parquet now uses zstd, and the write-time duplicate key check runs in DuckDB instead of pasting keys together in R.
* `write_parquet()`: `compression` now applies to encrypted files too (default `"zstd"`).
* `read_rcdf()`: normal (non-lazy) reads go straight from Parquet into R through DuckDB views instead of copying into a DuckDB table first, lowering peak memory by about 10%. Read time is unchanged; this was measured with primary keys stored, so it includes the read-time duplicate check, which reads only the key columns.
* New archives remain readable by rcdf 0.1.6, and old and new archives can be read together.

### Bug fixes

* `read_rcdf(lazy = TRUE)` no longer fails on archives that store a data dictionary.
* `read_rcdf()` combines a record from several files by column name. Before, columns were matched by position, so files with a different column order put values under the wrong columns.
* `read_rcdf()` / `merge_rcdf()`: area names, data dictionaries and metadata data frames whose columns differ between files are combined instead of failing.
* `add_metadata()` (and so `read_rcdf()` with a dictionary): code columns are converted to integer only when every value is a plain integer. Before, codes like `"012"` lost their leading zero, values like `"9X"` became `NA`, and decimals were truncated.
* `write_rcdf_xlsx(as_single_file = TRUE)`: record names longer than 31 characters are shortened to unique sheet names instead of failing.
* `write_parquet()` / `read_parquet()`: 48- and 64-character hex keys now work as documented.
* `read_rcdf()`: records with zero rows are kept (they were dropped unless `lazy = TRUE`), so `merge_rcdf()` no longer loses them.
* `read_dot_env()`: spaces around `=` and trailing spaces are ignored, so `PASSWORD = 1234` reads as `PASSWORD` = `"1234"`.
* `read_rcdf()`: legacy archives whose metadata has `key_app` but no `key_admin` are read correctly again (R's partial `$` matching picked up `key_app` as the current-format key).
* `primary_key` can name a record as `"d"` or `"d.parquet"`. Both forms are now checked at write time and saved into the archive for read-time checks; before, each form silently skipped one of the two.
* `write_rcdf()`: stages files in a private temp folder, so leftover Parquet files from an earlier failed write are no longer packed into a new archive.
* `write_rcdf()`: a data dictionary passed as a file path (`metadata$dictionary`) is no longer silently dropped.
* `read_rcdf()`: an RSA private key object (from `openssl::read_key()`) now works as `decryption_key`; unsupported key types give a clear error.
* `read_rcdf(ignore_duplicates = FALSE)`: now actually checks primary keys, including duplicates across several files.
* `read_rcdf()`: errors when the number of keys or passwords matches neither 1 nor the number of files.
* `read_rcdf()`: no longer uses `%||%`, which needs R >= 4.4.
* `write_parquet()`: the encryption key and path are now quoted safely in SQL.
* `write_rcdf_sqlite()`: the connection is closed even if a write fails.
* Export writers (`write_rcdf_csv()` etc.) skip the `__data_dictionary` element, as `write_rcdf_parquet()` already did.
* `write_rcdf_parquet()` returns only the files it wrote.
* `get_attr()`: dotted paths work at any depth (`"meta.source.note"`); before, anything past the second level was ignored.
* `generate_pw()`: removed modulo bias, so every character in the pool is equally likely per draw.

### Documentation

* `read_rcdf(lazy = TRUE)`: docs now show how to close the DuckDB connection, which holds the decrypted data until closed.
* `write_parquet()`: docs no longer claim arrow writes unencrypted files or that `...` is passed to it.
* `write_rcdf()`: documents the recognised `metadata` fields.

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

