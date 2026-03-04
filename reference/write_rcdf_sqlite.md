# Write RCDF data to a SQLite database

Writes all tables in the RCDF object to a single SQLite database file.

## Usage

``` r
write_rcdf_sqlite(data, path, db_name = "cbms_data", ..., parent_dir = NULL)
```

## Arguments

- data:

  A valid RCDF object.

- path:

  Output directory for the database file.

- db_name:

  Name of the SQLite database file (without extension).

- ...:

  Additional arguments passed to
  [`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html).

- parent_dir:

  Optional subdirectory under `path` to store the SQLite file.

## Value

Invisibly returns `NULL`. A `.db` file is written to disk.

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

write_rcdf_sqlite(data = rcdf_data, path = temp_dir)

unlink(temp_dir, force = TRUE)
```
