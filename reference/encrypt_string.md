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
#> [1] "sfhEvZDwn+PNitxCcGMVMTcGrSvZdUPpI23enrWzXf6dGjpHImDa3HSjrdcNdIktNylFPVZZVpsyqb0rmPtF2WGwI958Ytlznwlv5IfJPdCSLY2z/igAM6gu+1L+xceJ3UK/UTjY2DpPt2J0Mnmf6Pm19sCMPgqj1iRp/VrKiQuFTQf4Es1/Kkb0pbQv5mJrJZmfDkhtENG8rfeC9CVng4aBFjZIEoO/HJ8T1jWepFQaYCKM8ZxIs6FKRp0iI27bXX6XpQ6Syy4z792Ygihlnvptro2MYjfzfHG331F/R3a7aEcQKV60Ps3fpABv9M9IHPQ2PpUSXBnJpxfJFgQYsA=="
```
