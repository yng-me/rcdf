# Write RCDF data to multiple formats

Exports RCDF-formatted data to one or more supported open data formats.
The function automatically dispatches to the appropriate writer function
based on the `formats` provided.

## Usage

``` r
write_rcdf_as(data, path, formats, ...)
```

## Arguments

- data:

  A named list or RCDF object. Each element should be a table or
  tibble-like object (typically a `dbplyr` or `dplyr` table).

- path:

  The target directory where output files should be saved.

- formats:

  A character vector of file formats to export to. Supported formats
  include: `"csv"`, `"tsv"`, `"json"`, `"parquet"`, `"xlsx"`, `"dta"`,
  `"sav"`, and `"sqlite"`.

- ...:

  Additional arguments passed to the respective writer functions.

## Value

Invisibly returns `NULL`. Files are written to disk.

## See also

[write_rcdf_csv](https://yng-me.github.io/rcdf/reference/write_rcdf_csv.md)
[write_rcdf_tsv](https://yng-me.github.io/rcdf/reference/write_rcdf_tsv.md)
[write_rcdf_json](https://yng-me.github.io/rcdf/reference/write_rcdf_json.md)
[write_rcdf_xlsx](https://yng-me.github.io/rcdf/reference/write_rcdf_xlsx.md)
[write_rcdf_dta](https://yng-me.github.io/rcdf/reference/write_rcdf_dta.md)
[write_rcdf_sav](https://yng-me.github.io/rcdf/reference/write_rcdf_sav.md)
[write_rcdf_sqlite](https://yng-me.github.io/rcdf/reference/write_rcdf_sqlite.md)

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
rcdf_path <- file.path(dir, 'mtcars.rcdf')
private_key <- file.path(dir, 'sample-private-key.pem')

rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
temp_dir <- tempdir()

write_rcdf_as(data = rcdf_data, path = temp_dir, formats = c("csv", "xlsx"))

unlink(temp_dir, force = TRUE)
```
