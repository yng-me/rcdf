# Write RCDF data to Parquet files

This function writes an RCDF object (a list of data frames) to multiple
Parquet files. Each data frame in the list is written to its
corresponding Parquet file in the specified path.

## Usage

``` r
write_rcdf_parquet(
  data,
  path,
  ...,
  parent_dir = NULL,
  primary_key = NULL,
  ignore_duplicates = TRUE
)
```

## Arguments

- data:

  A list where each element is a data frame or tibble that will be
  written to a Parquet file.

- path:

  The directory path where the Parquet files will be written.

- ...:

  Additional arguments passed to
  [`rcdf::write_parquet()`](https://yng-me.github.io/rcdf/reference/write_parquet.md)
  while writing each Parquet file.

- parent_dir:

  An optional parent directory to be included in the path where the
  files will be written.

- primary_key:

  A `data.frame` or `tibble` that includes at least two columns: `file`
  and `pk_field_name`.

- ignore_duplicates:

  A `logical` flag. If `TRUE`, a warning is issued when duplicates are
  found. If `FALSE`, the function stops with an error.

## Value

A character vector of file paths to the written Parquet files.

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
rcdf_path <- file.path(dir, 'mtcars.rcdf')
private_key <- file.path(dir, 'sample-private-key-pw.pem')

rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key, password = '1234')
#>  [1] "log_id"             "created_at"         "meta"              
#>  [4] "area_names"         "summary_statistics" "dictionary"        
#>  [7] "ignore_duplicates"  "key_app"            "iv_app"            
#> [10] "key_admin"          "iv_admin"           "pc_os"             
#> [13] "pc_os_release_date" "pc_os_version"      "pc_hardware"       
#> [16] "version"            "checksum"          
temp_dir <- tempdir()

write_rcdf_parquet(data = rcdf_data, path = temp_dir)
#> [1] "/tmp/RtmphuBmfl/data.parquet"

unlink(temp_dir, force = TRUE)
```
