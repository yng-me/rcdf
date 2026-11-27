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
#> [1] "IUxwI8M98jzAlLlH7YV/oMcNA1TLfeX8e9HK710/xam4neaavc9dDnY5NSiLtgtxyXe2TOq72BvPl1n1pBwkCDRNpmRYOqO4C5iAzKwuV67mI/f70qqb7pxahIYW9ItnCjsENZ4JCopfHqkEDkeHM0rECsYkgNr/LF0FJ/aVSqM4SNoZac1CB3O5roC/CvIhJMS1ctJ1sz2rE8OI4KcXNchKVSOfoMuhCC6xCT7vfhJMX0hcw7egiw4HBcH4Mo+4EINnUb+YnI/kGjTrosAI/gD83/Cy72tBl8b3Yl6lxkIN8buP9w6EhRGlM9aYsQfRdDzldRlGIBhQXwUMB2V1qA=="
```
