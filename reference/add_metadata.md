# Add metadata attributes to a data frame

Adds variable labels and value labels to a data frame based on a
metadata dictionary. This is particularly useful for preparing datasets
for use with packages like `haven` or for exporting to formats like SPSS
or Stata.

## Usage

``` r
add_metadata(data, metadata, ..., set_data_types = FALSE)
```

## Arguments

- data:

  A data frame containing the raw dataset.

- metadata:

  A data frame that serves as a metadata dictionary. It must contain at
  least the columns: `variable_name`, `label`, and `type`. Optionally,
  it may include a `valueset` column for categorical variables, which
  should be a list column with data frames containing `value` and
  `label` columns.

- ...:

  Additional arguments (currently unused).

- set_data_types:

  Logical; if `TRUE`, attempts to coerce column data types to match
  those implied by the metadata. (Note: currently not fully
  implemented.)

## Value

A `tibble` with the same data as `data`, but with added attributes:

- Variable labels (via the `label` attribute)

- Value labels (as a
  [`haven::labelled`](https://haven.tidyverse.org/reference/labelled.html)
  class, if applicable)

## Details

The function first checks the structure of the `metadata` using an
internal helper. Then, for each variable listed in `metadata`, it:

- Adds a label using the `label` attribute

- Converts values to labelled vectors using
  [`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
  if a `valueset` is provided

If value labels are present, the function tries to align data types
between the data and the valueset (e.g., converting character codes to
integers if necessary).

## Examples

``` r
data <- data.frame(
  sex = c(1, 2, 1),
  age = c(23, 45, 34)
)

metadata <- data.frame(
  variable_name = c("sex", "age"),
  label = c("Gender", "Age in years"),
  type = c("categorical", "numeric"),
  valueset = I(list(
    data.frame(value = c(1, 2), label = c("Male", "Female")),
    NULL
  ))
)

labelled_data <- add_metadata(data, metadata)
str(labelled_data)
#> 'data.frame':    3 obs. of  2 variables:
#>  $ sex: dbl+lbl [1:3] 1, 2, 1
#>    ..@ labels: Named num  1 2
#>    .. ..- attr(*, "names")= chr [1:2] "Male" "Female"
#>    ..@ label : chr "Gender"
#>  $ age: num  23 45 34
#>   ..- attr(*, "label")= chr "Age in years"
```
