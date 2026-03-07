# Decrypt string using RSA

Decrypt string using RSA

## Usage

``` r
decrypt_string(x, prv_key, password = NULL)
```

## Arguments

- x:

  Encrypted base64-encoded string

- prv_key:

  A private key object or .pem file

- password:

  Passwor of the private key

## Value

A decrpyted character of length 1

## Examples

``` r
dir <- system.file("extdata", package = "rcdf")
pub_key <- file.path(dir, 'sample-public-key.pem')
prv_key <- file.path(dir, 'sample-private-key.pem')
x <- encrypt_string('hello', pub_key)
decrypt_string(x, prv_key = prv_key, password = '1234')
#> [1] "hello"
```
