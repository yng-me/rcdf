# Write Parquet file with optional encryption

Writes a data frame to a Parquet file. When `encryption_key` is supplied
the file is encrypted with AES using DuckDB's native Parquet encryption
support. Without a key the file is written by
[`arrow::write_parquet()`](https://arrow.apache.org/docs/r/reference/write_parquet.html)
and supports any compression codec understood by the Arrow library.

## Usage

``` r
write_parquet(
  data,
  path,
  ...,
  encryption_key = NULL,
  conn = NULL,
  compression = "zstd"
)
```

## Arguments

- data:

  A data frame or tibble to write.

- path:

  Character string. Destination file path for the Parquet file.

- ...:

  Additional arguments passed to
  [`arrow::write_parquet()`](https://arrow.apache.org/docs/r/reference/write_parquet.html)
  when `encryption_key` is `NULL`.

- encryption_key:

  A raw AES key as a hex string (32, 48, or 64 hex characters) or a
  base-64–encoded 256-bit key string. When `NULL` (default) no
  encryption is applied.

- conn:

  An optional existing DuckDB `DBIConnection`. When supplied, the
  connection is reused instead of opening a new one, which avoids
  per-call setup overhead when writing many tables in a batch. The
  caller is responsible for disconnecting the connection. Default is
  `NULL`.

- compression:

  Compression codec to use for unencrypted Parquet files. Any codec
  supported by
  [`arrow::write_parquet()`](https://arrow.apache.org/docs/r/reference/write_parquet.html)
  is valid (e.g. `"zstd"`, `"snappy"`, `"gzip"`, `"lz4"`,
  `"uncompressed"`). Ignored when `encryption_key` is set (DuckDB
  chooses the codec). Default is `"zstd"`.

## Value

`NULL` invisibly. The Parquet file is written to `path`.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- mtcars
key <- "rppqM5CuEqotys4wQq/g7xh6wpIjRozcAIbI9sagwKE="

temp_dir <- tempdir()

# Encrypted write
rcdf::write_parquet(
  data = data,
  path = file.path(temp_dir, "mtcars.parquet"),
  encryption_key = key
)

# Unencrypted write with gzip compression
rcdf::write_parquet(
  data = data,
  path = file.path(temp_dir, "mtcars-gz.parquet"),
  compression = "gzip"
)
} # }
```
