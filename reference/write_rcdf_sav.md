# Write RCDF data to SPSS `.sav` files

Writes each table in the RCDF object to a `.sav` file using the `haven`
package for compatibility with SPSS.

## Usage

``` r
write_rcdf_sav(data, path, ..., parent_dir = NULL)
```

## Arguments

- data:

  A valid RCDF object.

- path:

  Output directory for files.

- ...:

  Additional arguments passed to
  [`haven::write_sav()`](https://haven.tidyverse.org/reference/read_spss.html).

- parent_dir:

  Optional subdirectory under `path` to group SPSS files.

## Value

Invisibly returns `NULL`. Files are written to disk.

## See also

[write_rcdf_as](https://yng-me.github.io/rcdf/reference/write_rcdf_as.md)

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
rcdf_path <- file.path(dir, 'mtcars.rcdf')
private_key <- file.path(dir, 'sample-private-key.pem')

rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
temp_dir <- tempdir()

write_rcdf_sav(data = rcdf_data, path = temp_dir)

unlink(temp_dir, force = TRUE)
```
