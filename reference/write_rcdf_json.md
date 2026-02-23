# Write RCDF data to JSON files

Writes each table in the RCDF object as a separate `.json` file.

## Usage

``` r
write_rcdf_json(data, path, ..., parent_dir = NULL)
```

## Arguments

- data:

  A valid RCDF object.

- path:

  The output directory for files.

- ...:

  Additional arguments passed to
  [`jsonlite::write_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html).

- parent_dir:

  Optional subdirectory under `path` to group JSON files.

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
temp_dir <- tempdir()

write_rcdf_json(data = rcdf_data, path = temp_dir)

unlink(temp_dir, force = TRUE)
```
