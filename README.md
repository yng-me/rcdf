
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The `rcdf` Package

A Comprehensive Toolkit for Working with RCDF (Encrypted Parquet) Files
in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/yng-me/rcdf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yng-me/rcdf/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`rcdf` provides functions to easily create, read, and manipulate RCDF
files (encrypted Parquet format) in R. The package supports robust
encryption and decryption workflows, integrates with Arrow for efficient
data access, and provides utilities for managing metadata, generating
AES keys, and more.

Key features include:

- **Secure Data Handling**: Functions for reading and writing encrypted
  Parquet files using AES and RSA encryption.
- **Metadata Management**: Tools for handling and storing metadata in
  RCDF files, including automated key generation and decryption.
- **Parquet Integration**: Full integration with the `arrow` package to
  read and write Parquet files seamlessly.
- **Cross-Platform Support**: Compatible with Linux, macOS, and Windows
  environments.

## Installation

You can install the development version of rcdf from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yng-me/rcdf")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rcdf)
## basic example code
```
