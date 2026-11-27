# Write RCDF data to Parquet files

This function writes an RCDF object (a list of data frames) to multiple
Parquet files. Each data frame in the list is written to its
corresponding Parquet file in the specified path.

## Usage

``` r
write_rcdf_parquet(
  data,
  path,
  ...,
  parent_dir = NULL,
  primary_key = NULL,
  ignore_duplicates = TRUE
)
```

## Arguments

- data:

  A list where each element is a data frame or tibble that will be
  written to a Parquet file. DuckDB lazy tables are streamed without
  collecting; see
  [`write_parquet`](https://yng-me.github.io/rcdf/reference/write_parquet.md).

- path:

  The directory path where the Parquet files will be written.

- ...:

  Additional arguments passed to
  [`rcdf::write_parquet()`](https://yng-me.github.io/rcdf/reference/write_parquet.md)
  while writing each Parquet file.

- parent_dir:

  An optional parent directory to be included in the path where the
  files will be written.

- primary_key:

  A `data.frame` or `tibble` that includes at least two columns: `file`
  and `pk_field_name`.

- ignore_duplicates:

  A `logical` flag. If `TRUE`, a warning is issued when duplicates are
  found. If `FALSE`, the function stops with an error.

## Value

A character vector of file paths to the written Parquet files.

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
rcdf_path <- file.path(dir, 'mtcars.rcdf')
private_key <- file.path(dir, 'sample-private-key-pw.pem')

rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpQ9o2F9/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
temp_dir <- tempdir()

write_rcdf_parquet(data = rcdf_data, path = temp_dir)
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpQ9o2F9/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> [1] "/tmp/RtmpQ9o2F9/data.parquet"

unlink(temp_dir, force = TRUE)
```
