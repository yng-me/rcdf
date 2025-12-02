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
data <- mtcars
key <- "5bddd0ea4ab48ed5e33b1406180d68158aa255cf3f368bdd4744abc1a7909ead"
iv <- "7D3EF463F4CCD81B11B6EC3230327B2D"

temp_dir <- tempdir()

rcdf::write_parquet(
  data = data,
  path = file.path(temp_dir, "mtcars.parquet"),
  encryption_key = list(aes_key = key, aes_iv = iv)
)

unlink(file.path(temp_dir, "mtcars.parquet"), force = TRUE)
```
