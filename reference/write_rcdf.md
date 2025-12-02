# Write data to RCDF format

This function writes data to an RCDF (Reusable Data Container Format)
archive. It encrypts the data using AES, generates metadata, and then
creates a zip archive containing both the encrypted Parquet files and
metadata. The function supports the inclusion of metadata such as system
information and encryption keys.

## Usage

``` r
write_rcdf(
  data,
  path,
  pub_key,
  ...,
  metadata = list(),
  ignore_duplicates = TRUE
)
```

## Arguments

- data:

  A list of data frames or tables to be written to RCDF format. Each
  element of the list represents a record.

- path:

  The path where the RCDF file will be written. The file will be saved
  with a `.rcdf` extension if not already specified.

- pub_key:

  The public RSA key used to encrypt the AES encryption keys.

- ...:

  Additional arguments passed to helper functions if needed.

- metadata:

  A list of metadata to be included in the RCDF file.

- ignore_duplicates:

  A `logical` flag. If `TRUE`, a warning is issued when duplicates are
  found. If `FALSE`, the function stops with an error.

## Value

NULL. The function writes the data to a `.rcdf` file at the specified
path.

## Examples

``` r
# Example usage of writing an RCDF file

rcdf_data <- rcdf_list()
rcdf_data$mtcars <- mtcars

dir <- system.file("extdata", package = "rcdf")

temp_dir <- tempdir()

write_rcdf(
  data = rcdf_data,
  path = file.path(temp_dir, "mtcars.rcdf"),
  pub_key = file.path(dir, 'sample-public-key.pem')
)

write_rcdf(
  data = rcdf_data,
  path = file.path(temp_dir, "mtcars-pw.rcdf"),
  pub_key = file.path(dir, 'sample-public-key-pw.pem')
)

unlink(file.path(temp_dir, "mtcars.rcdf"), force = TRUE)
unlink(file.path(temp_dir, "mtcars-pw.rcdf"), force = TRUE)
```
