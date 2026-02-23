# Write RCDF data to Excel files

Writes each table in the RCDF object as a separate `.xlsx` file using
the `openxlsx` package.

## Usage

``` r
write_rcdf_xlsx(data, path, ..., parent_dir = NULL)
```

## Arguments

- data:

  A valid RCDF object.

- path:

  The output directory.

- ...:

  Additional arguments passed to
  [`openxlsx::write.xlsx()`](https://rdrr.io/pkg/openxlsx/man/write.xlsx.html).

- parent_dir:

  Optional subdirectory under `path` to group Excel files.

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

write_rcdf_xlsx(data = rcdf_data, path = temp_dir)

unlink(temp_dir, force = TRUE)
```
