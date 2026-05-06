# Read all top-level metadata from an RCDF file

Extracts and returns the complete metadata JSON stored inside an `.rcdf`
archive without decrypting or loading the underlying data. Useful for
inspecting provenance, checksums, encryption parameters, and embedded
data dictionaries without a decryption key.

## Usage

``` r
get_attrs(path)
```

## Arguments

- path:

  Character string. Path to a valid `.rcdf` file.

## Value

A named list corresponding to the `metadata.json` stored inside the
archive. Common keys include `log_id`, `created_at`, `version`,
`checksum`, `dictionary`, and `key`.

## See also

[`get_rcdf_metadata`](https://yng-me.github.io/rcdf/reference/get_rcdf_metadata.md)
for retrieving a single metadata key,
[`get_attr`](https://yng-me.github.io/rcdf/reference/get_attr.md) for
reading metadata attributes attached to an in-memory RCDF object.

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
rcdf_path <- file.path(dir, "mtcars.rcdf")

meta <- get_attrs(rcdf_path)
meta$version
#> [1] "0.1.3"
meta$created_at
#> [1] "2026-02-13 01:34:23"
```
