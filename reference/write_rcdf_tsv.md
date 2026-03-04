# Write RCDF data to TSV files

Writes each table in the RCDF object as a separate tab-separated `.txt`
file.

## Usage

``` r
write_rcdf_tsv(data, path, ..., parent_dir = NULL)
```

## Arguments

- data:

  A valid RCDF object.

- path:

  The base output directory.

- ...:

  Additional arguments passed to
  [`write.table()`](https://rdrr.io/r/utils/write.table.html).

- parent_dir:

  Optional subdirectory under `path` to group TSV files.

## Value

Invisibly returns `NULL`. Files are written to disk.

## See also

[write_rcdf_as](https://yng-me.github.io/rcdf/reference/write_rcdf_as.md)

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

write_rcdf_tsv(data = rcdf_data, path = temp_dir)

unlink(temp_dir, force = TRUE)
```
