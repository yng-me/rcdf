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
#> [1] "C90fJzYDNA+l20geZEMDz+kHXyVacaGJgUKp9uYazzybtKo29GN9kXAtNwj1dCbdYVNF9Pi3Z6Td561Z495eNlp0Fzjd/mt/t/hB52JwAmhuKQT3i3/toewmL+JQNP9Iilau2s0szyrLb9majfnnMapMvJA4ZgkA04+aEme34ndAA1Vyes5KruJM30dNVc6B5iIXzVHEIdhCxJtIxP+MWDn8tw+ajNA3R/hI20MtZ7v+kELLq6bLpNCXDw/uZHRt4PPo54azvO5g1NNk/2ehM0Jw/gjAAc9SB+8vQsdTXjFlec+CeIpt+hFn1fHzbQphiVxKOTa1Gz4pVc7mZLt+Lg=="
```
