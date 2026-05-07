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
#> [1] "TLiXOKLPnnUI9yi4MkzAmzoT0o9+/qQeVB5gAmZ58UgrJ0PnVHB99aMqrrX2ua9SnL8dveR+hqmdsek37kuu+DwlOL+tmULrupXqD1gfMgWk1Pm/egk8y6bCI6fCffxzgaEG8n2r8JWhXHPMUjwEYLK76sxUn+I1i+pmNNbu85SPOqk9R2eGCBpfQwXc2QTvMnArlKu7dnuCD+hyOO1uq9uieDil0Lm0gbMbkaR7/GRAqv0YFx+jKzna+uzes2W7cvAGS0x0N7xi/C/DUSokiQcIRtI5Jtxejhdu7t1FnGrS8aW8RtHrdcdwGC7TnQQWPDDZMcdGYI3Mrkqkh8aDVg=="
```
