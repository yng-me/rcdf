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
#> [1] "IkGV74qke4hndHtOjcPznN0E6J8yoZRMSjOTkGlbO43T42m4LWbMqDG3v9igeL8jspr8qP0Kwj8Tz/YvRLC08ZfnFY1AK4YnSET2HQVskdZucP1IxnAvJCTKJEHu+1VdJN6jtmqn4QsFFNO/6MZ0h4oMVXULbJccJLk89aQLwZuQpTxtXOpHtgDpu9qyBT6/VwD4vksmDGq0E4tYrRobkCxXs/7jcuGYD0R7xRltxpZYmQAsk4Ycd8wv3kaE+22VJCWPUZQsbSNqDjtbELnf9LoePdjI/bfxaeVPABRbDm4YFSJGEtPUYQMi8APU9pQbe9t1E0tUJ7cxkzy4/U8rQQ=="
```
