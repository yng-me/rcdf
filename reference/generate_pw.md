# Generate a random password

This function generates a random password of a specified length. It
includes alphanumeric characters by default and can optionally include
special characters.

## Usage

``` r
generate_pw(length = 16, special_chr = TRUE)
```

## Arguments

- length:

  Integer. The length of the password to generate. Default is `16`.

- special_chr:

  Logical. Whether to include special characters (e.g., `!`, `@`, `#`,
  etc.) in the password. Default is `TRUE`.

## Value

A character string representing the generated password.

## Examples

``` r
generate_pw()
#> [1] "ANWkuF<KG4nQO1zs"
generate_pw(32)
#> [1] "zg4WhyxHA3X(yAX<6vq=8l9Z&gptVG0^"
generate_pw(12, special_chr = FALSE)
#> [1] "xojCoa7wHUDr"
```
