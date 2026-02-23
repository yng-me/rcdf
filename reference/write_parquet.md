# Write Parquet file with optional encryption

This function writes a dataset to a Parquet file. If an encryption key
is provided, the data will be encrypted before writing. Otherwise, the
function writes the data as a regular Parquet file without encryption.

## Usage

``` r
write_parquet(data, path, ..., encryption_key = NULL)
```

## Arguments

- data:

  A data frame or tibble to write to a Parquet file.

- path:

  The file path where the Parquet file will be written.

- ...:

  Additional arguments passed to
  [`arrow::write_parquet()`](https://arrow.apache.org/docs/r/reference/write_parquet.html)
  if no encryption key is provided.

- encryption_key:

  A list containing `aes_key` and `aes_iv`. If provided, the data will
  be encrypted using AES before writing to Parquet.

## Value

None. The function writes the data to a Parquet file at the specified
`path`.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- mtcars
key <- "rppqM5CuEqotys4wQq/g7xh6wpIjRozcAIbI9sagwKE="

temp_dir <- tempdir()

rcdf::write_parquet(
  data = data,
  path = file.path(temp_dir, "mtcars.parquet"),
  encryption_key = list(aes_key = key, aes_iv = iv)
)
} # }
```
