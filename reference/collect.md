# Collect a lazy RCDF table into a data frame

Materialises a lazy `rcdf_tbl_db` DuckDB-backed table into a regular R
data frame, optionally applying the variable labels and value labels
stored in the table's metadata dictionary.

## Usage

``` r
collect(data, ...)

# S3 method for class 'rcdf_tbl_db'
collect(data, ...)
```

## Arguments

- data:

  A lazy `rcdf_tbl_db` object (returned by
  [`read_rcdf`](https://yng-me.github.io/rcdf/reference/read_rcdf.md)
  with `lazy = TRUE`), or any object supported by
  [`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).

- ...:

  Additional arguments passed to
  [`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).

## Value

A `tibble` with all rows materialised. If the table carries a metadata
dictionary, variable labels and value labels are applied via
[`add_metadata`](https://yng-me.github.io/rcdf/reference/add_metadata.md)
before returning.

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
rcdf_path  <- file.path(dir, "mtcars.rcdf")
prv_key    <- file.path(dir, "sample-private-key-pw.pem")

if (FALSE) { # \dontrun{
result <- read_rcdf(path = rcdf_path, decryption_key = prv_key,
                    password = "1234", lazy = TRUE)
df <- collect(result$mtcars)
class(df)  # "tbl_df"
} # }
```
