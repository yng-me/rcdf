# Convert to `rcdf` class

Converts an existing list or compatible object into an object of class
`rcdf`.

## Usage

``` r
as_rcdf(data)
```

## Arguments

- data:

  A list or object to be converted to class `rcdf`.

## Value

The input object with class set to `rcdf`.

## Examples

``` r
my_list <- list(a = 1, b = 2)
rcdf_obj <- as_rcdf(my_list)
class(rcdf_obj)
#> [1] "list" "rcdf"
```
