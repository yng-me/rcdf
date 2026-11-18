# Write RCDF data to CSV files

Writes each table in the RCDF object as a separate `.csv` file.

## Usage

``` r
write_rcdf_csv(data, path, ..., parent_dir = NULL)
```

## Arguments

- data:

  A valid RCDF object.

- path:

  The base output directory.

- ...:

  Additional arguments passed to
  [`write.csv()`](https://rdrr.io/r/utils/write.table.html).

- parent_dir:

  Optional subdirectory under `path` to group CSV files.

## Value

Invisibly returns `NULL`. Files are written to disk.

## See also

[write_rcdf_as](https://yng-me.github.io/rcdf/reference/write_rcdf_as.md)

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
rcdf_path <- file.path(dir, 'mtcars.rcdf')
private_key <- file.path(dir, 'sample-private-key-pw.pem')

rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpcwKcmS/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
temp_dir <- tempdir()

write_rcdf_csv(data = rcdf_data, path = temp_dir)

unlink(temp_dir, force = TRUE)
```
