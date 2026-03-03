# rcdf 0.1.4

### Features

* Added `get_attr()` to retrieve attribute/s from RCDF object.
* Added `read_parquet_tbl()` to read encrypted Parquet file and return as lazy data from DuckDB connection.

### Breaking change:
* Removed `pre_collect` argument in `read_rcdf()`

### Enhancements

* Improved SQL query string interpolation for all DuckDB related codes.

# rcdf 0.1.3

* Added `merge_rcdf()` function to combine multiple RCDF files with different encryption keys into one.
* Updated `read_rcdf()` to allow reading of multiple RCDF file with different encryption keys.
* Added `pre_collect` argument to `read_rcdf()` to allow lazy loading the RCDF object.

# rcdf 0.1.2

* Fixed #14 

# rcdf 0.1.1

* Added a `NEWS.md` file to track changes to the package.

