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
#> [1] "QHKNzv2xSsxsLVzC65fFfzRp4PMy4DKKjRxIIer2975yRRTghOrcpw8lAEOKPqQXVkumCayOqo+t7f0AmlrGW9v6l4SVzBgQDFORLZRT/FruY8oBcyJnOXVBBnCprHUH8Bt489l7VEvMPL65LxB3wrSEF7dFMSeH5SqYx4KlDSN8iU3mAeso96/uf47SAVkwq2FuUXVIkNnvHp/Mtn9m6L3DwUetey04izWxgBOJkC7sFdWa3oo4vomgy5uo5rvXsa6HADEApfmkDz66OMFZPJqOYNeiQb9ASKfAM+CalIOG9DB40uELr3H6AtTUZK3CQUk11QLckuC12M6gC7GGSg=="
```
