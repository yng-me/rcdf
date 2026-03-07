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
#> [1] "YWTglCbCyzZAYWgPZCT/JIUNMXUt8/nlCQMF9Wy2xlL3EwfbIIB0cqxdDV4s+HlS/fXiUGyhRYPSgtPAxdCE1dVWZ4iX+XyZRLMcjhSMGvJy9goZaQsCEnzcYkuwyft0SFXauF7VXRSQhWoRccVSAD7xalsLoUvIHepEtRKLB8DNzlZTZZBF3eLm9XidAr9glNu+4PQ/wCw7ov4FZPptsX1v1VAKndI/kx3p9+xXfxooJRR9EgPBmBCq5TPoMUobqoQrdFqxgByv7S6Bu8/EqQURwlXM6unUJS2EiOnuNXBfdfY+CWNZPpai+q87qWdeM9koJU30rLzWd440Ttrn0w=="
```
