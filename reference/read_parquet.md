# Read Parquet file with optional decryption

This function reads a Parquet file, optionally decrypting it using the
provided decryption key. If no decryption key is provided, it reads the
file normally without decryption. It supports reading Parquet files as
Arrow tables or regular data frames, depending on the `as_arrow_table`
argument.

## Usage

``` r
read_parquet(
  path,
  ...,
  decryption_key = NULL,
  as_arrow_table = FALSE,
  metadata = NULL
)
```

## Arguments

- path:

  The file path to the Parquet file.

- ...:

  Additional arguments passed to
  [`arrow::open_dataset()`](https://arrow.apache.org/docs/r/reference/open_dataset.html)
  when no decryption key is provided.

- decryption_key:

  A list containing `aes_key` and `aes_iv`. If provided, the Parquet
  file will be decrypted using these keys. Default is \`NULL\`.

- as_arrow_table:

  Logical. If `TRUE`, the function will return the result as an Arrow
  table. If `FALSE`, a regular data frame will be returned. Default is
  `FALSE`.

- metadata:

  Optional metadata (e.g., a data dictionary) to be applied to the
  resulting data.

## Value

An Arrow table or a data frame, depending on the value of
`as_arrow_table`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Using sample Parquet files from `mtcars` dataset
dir <- system.file("extdata", package = "rcdf")

# Not encrypted
read_parquet(file.path(dir, "mtcars.parquet"))

# Encrypted
read_parquet(
  file.path(dir, "mtcars-encrypted.parquet"),
  decryption_key = 'rppqM5CuEqotys4wQq/g7xh6wpIjRozcAIbI9sagwKE='
)
} # }
```
