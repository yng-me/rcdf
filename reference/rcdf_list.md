# Create an empty `rcdf` object

Initializes and returns an empty `rcdf` object. This is a convenient
constructor for creating a new `rcdf`-class list structure.

## Usage

``` r
rcdf_list(...)
```

## Arguments

- ...:

  Optional elements to include in the list. These will be passed to the
  internal list constructor and included in the resulting `rcdf` object.

## Value

A list object of class `rcdf`.

## Examples

``` r
rcdf <- rcdf_list()
class(rcdf)
#> [1] "list" "rcdf"
```
