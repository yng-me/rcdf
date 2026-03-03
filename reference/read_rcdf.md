# Read and decrypt RCDF data

This function reads an RCDF file, decrypts its contents using the
specified decryption key, and loads it into R as an RCDF object.

## Usage

``` r
read_rcdf(
  path,
  ...,
  decryption_key,
  password = NULL,
  metadata = list(),
  ignore_duplicates = TRUE,
  recursive = FALSE,
  return_meta = FALSE
)
```

## Arguments

- path:

  A string specifying the path to the RCDF archive (zip file). If a
  directory is provided, all `.rcdf` files within that directory will be
  processed.

- ...:

  Additional parameters passed to other functions, if needed (not yet
  implemented).

- decryption_key:

  The key used to decrypt the RCDF. This can be an RSA or AES key,
  depending on how the RCDF was encrypted.

- password:

  A password used for RSA decryption (optional).

- metadata:

  An optional list of metadata object containing data dictionaries,
  value sets, and primary key constraints for data integrity measure (a
  `data.frame` or `tibble` that includes at least two columns: `file`
  and `pk_field_name`. This metadata is applied to the data if provided.

- ignore_duplicates:

  A `logical` flag. If `TRUE`, a warning is issued when duplicates are
  found, based on the primary key/s defined during creation of RCDF
  file. If `FALSE`, the function stops with an error.

- recursive:

  Logical. If `TRUE` and `path` is a directory, the function will search
  recursively for `.rcdf` files.

- return_meta:

  Logical. If `TRUE`, the metadata will be returned as an attribute of
  the RCDF object.

## Value

An RCDF object, which is a list of Parquet files (one for each record)
along with attached metadata.

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
rcdf_path <- file.path(dir, 'mtcars.rcdf')
private_key <- file.path(dir, 'sample-private-key-pw.pem')
pw <- '1234'

if (FALSE) { # \dontrun{
rcdf_data <- read_rcdf(
  path = rcdf_path,
  decryption_key = private_key,
  password = pw
)

rcdf_data
} # }
```
