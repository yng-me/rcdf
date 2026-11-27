# Encrypt string using RSA

Encrypt string using RSA

## Usage

``` r
encrypt_string(x, pub_key)
```

## Arguments

- x:

  A character of length 1

- pub_key:

  A public key object or .pem file

## Value

Encrypted base64-encoded string

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
pub_key <- file.path(dir, 'sample-public-key.pem')
encrypt_string('hello', pub_key)
#> [1] "TNiJkQlhG57DyLfNJN7KM8SZQ2snfIUowrklN4bW+Aq5pj/t+XxIwLNYCEvTepMxaCmubSCENEesH2H3RlxRNQtbKk4re7ot4MST52KzPKIPCReIQPutqrJ/UsA89tEgVejTXJfxJ4FUV+ZvexuQrFh9CnFVD3H21YNCSH1nWU14+e2xuLGSjzjwnjYjVglmPhFEFkr36m1ywE39aTlwyWv95m433xtKBSB4khhbZf6m9aKWbH0/+R+AjbfbQH3rCE576uYwJ91JNAFGi1XFi4FZcbHzlPLx38iIW/KCAyAjazkGKatYyt+0byNLcLU4V6yFdBvF+7MaTlkE8BaJ3Q=="
```
