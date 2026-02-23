# Write RCDF data to a SQLite database

Writes all tables in the RCDF object to a single SQLite database file.

## Usage

``` r
write_rcdf_sqlite(data, path, db_name = "cbms_data", ..., parent_dir = NULL)
```

## Arguments

- data:

  A valid RCDF object.

- path:

  Output directory for the database file.

- db_name:

  Name of the SQLite database file (without extension).

- ...:

  Additional arguments passed to
  [`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html).

- parent_dir:

  Optional subdirectory under \`path\` to store the SQLite file.

## Value

Invisibly returns `NULL`. A `.db` file is written to disk.

## See also

[write_rcdf_as](https://yng-me.github.io/rcdf/reference/write_rcdf_as.md)

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
rcdf_path <- file.path(dir, 'mtcars.rcdf')
private_key <- file.path(dir, 'sample-private-key.pem')

rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
temp_dir <- tempdir()

write_rcdf_sqlite(data = rcdf_data, path = temp_dir)

unlink(temp_dir, force = TRUE)
```
