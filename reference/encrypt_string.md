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
#> [1] "GzhzNEo3QokD+Gc3rKp/9k+itltNxOLDARNfWJucgr0xYxc98Pu7wJ/8jgotXK/Jos5o21GTy94eFY4fBDNGLHUQxiavuEGKxeOZ/HWpYKi12tXT2/LD5FJqpbEOUZ5gUBXpROX5pJ57yY4SsxKZ5PdoLjEZKYrQBMcLYgivphdzi5z5/JPqgZn962bIQ/olJsAZ6rxK7RWOkeoKOHDxixunR49uUixObmw8jPgYHZVY5ghJ9WTnhKMBwv0ul1PQU6cRm8xhho8TIu+SvB6vulzddM34CdP1sddNf4g1aq6twqRPXyjdra9kuFaUQUKZ/GKUOIT9yWuKKgzOpj/pew=="
```
