# Extract data dictionary from RCDF object

This function retrieves the data dictionary embedded in the RCDF object

## Usage

``` r
get_data_dictionary(data)
```

## Arguments

- data:

  Object of class `rcdf`.

## Value

A data frame that serves as a metadata dictionary. It must contain at
least the columns: `variable_name`, `label`, and `type`. Optionally, it
may include a `valueset` column for categorical variables, which should
be a list column with data frames containing `value` and `label`
columns.

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
rcdf_path <- file.path(dir, 'mtcars.rcdf')
private_key <- file.path(dir, 'sample-private-key.pem')

rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
data_dictionary <- get_data_dictionary(rcdf_data)
names(data_dictionary)
#> NULL
```
