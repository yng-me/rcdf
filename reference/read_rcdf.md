# Read and decrypt RCDF data

This function reads an RCDF file, decrypts its contents using the
specified decryption key, and loads it into R as an RCDF object.

## Usage

``` r
read_rcdf(
  path,
  decryption_key,
  ...,
  password = NULL,
  metadata = list(),
  ignore_duplicates = TRUE,
  recursive = FALSE,
  return_meta = FALSE
)
```

## Arguments

- path:

  A string specifying the path to the RCDF archive (zip file). If a
  directory is provided, all `.rcdf` files within that directory will be
  processed.

- decryption_key:

  The key used to decrypt the RCDF contents. This can be an RSA or AES
  key, depending on how the RCDF was encrypted.

- ...:

  Additional parameters passed to other functions, if needed.

- password:

  A password used for RSA decryption (optional).

- metadata:

  An optional list of metadata object containing data dictionaries,
  value sets, and primary key constraints for data integrity measure (a
  `data.frame` or `tibble` that includes at least two columns: `file`
  and `pk_field_name`. This metadata is applied to the data if provided.

- ignore_duplicates:

  A `logical` flag. If `TRUE`, a warning is issued when duplicates are
  found, based on the primary key/s defined during creation of RCDF
  file. If `FALSE`, the function stops with an error.

- recursive:

  Logical. If `TRUE` and `path` is a directory, the function will search
  recursively for `.rcdf` files.

- return_meta:

  Logical. If `TRUE`, the metadata will be returned as an attribute of
  the RCDF object.

## Value

An RCDF object, which is a list of Parquet files (one for each record)
along with attached metadata.

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
rcdf_path <- file.path(dir, 'mtcars.rcdf')
private_key <- file.path(dir, 'sample-private-key.pem')

rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
rcdf_data
#> $mtcars
#> # A tibble: 32 × 11
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
#> 
#> attr(,"class")
#> [1] "list" "rcdf"

# Using encrypted/password protected private key
rcdf_path_pw <- file.path(dir, 'mtcars-pw.rcdf')
private_key_pw <- file.path(dir, 'sample-private-key-pw.pem')
pw <- '1234'

rcdf_data_with_pw <- read_rcdf(
  path = rcdf_path_pw,
  decryption_key = private_key_pw,
  password = pw
)

rcdf_data_with_pw
#> $mtcars
#> # A tibble: 32 × 11
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
#> 
#> attr(,"class")
#> [1] "list" "rcdf"
```
