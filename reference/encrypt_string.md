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
#> [1] "eu/ZwL8Sf2RtasYg337f/mVVADY+KpZKcLN7mUuRrqtxYDCgsckwvGcVnUjCryamLejhipmkPZomlJltiq/hZTR4Mn4VNOSWlYJuFU/DMLOrtdakmBTTWISkAumtPXbsDazNJNxLBeiQGZIh1sd6lQQDrfAh4/EUVwN+iqjnyntT+yCOdkq8D/UicFn3/5VtS5yPVC9NQmVP1+smVrP+bwqr6dxpH3mPly/6TXlCiQNklG0yFiXfbVFDFYVy9goE0GnOrNBI1h+crLaMtBqqrVxyjaRpEXsdzg7V80n/iS0ziAoQgIY/ne8TQHz9Yypr2Hy1Ev/8/o+dg7G7fmJfkQ=="
```
