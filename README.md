
<!-- README.md is generated from README.Rmd. Please edit that file -->
relatable
=========

`relatable` provides two easy-to-use, robust functions for mapping from a vector of keys to a vector of values, as well as creating and applying more sophisticated mappings, such as many-to-many, one-to-many, and many-to-one relations. These are primarily designed with two goals in mind:

1.  Making reusable code that is easier to write and read.
2.  Ensuring relations conform to specified restrictions, for example injectivity or surjectivity, and safely handle nonstandard mappings, including unspecified keys, NAs, heterogeneous lists and lists of lists, and mappings between vectors of different lengths.

Installation
------------

You can install relatable from github with:

``` r
# install.packages("devtools")
devtools::install_github("domjarkey/relatable")
```

Applications
------------

### As simple key-value dictionary

`relate` maps a vector of inputs `X` from their position in a vector of keys `A` to the corresponding value in vector `B`. For simple applications, `relate` provides a more elegant one-line alternative to creating and querying a named-vector dictionary. `relation` returns a function that performs the same mapping for reusability.

``` r
library(relatable)
keys <- c(11, 12, 13, 14, 15)
values <- c("Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen")

## One-off mapping of inputs from A to B
relate(c(12, 14), keys, values)
#> [1] "Twelve"   "Fourteen"

## Create a function for repeat usage
num_to_word <- relation(keys, values)
num_to_word(13:15)
#> [1] "Thirteen" "Fourteen" "Fifteen"

## Both of these require fewer lines of code, and additionally
## do not require keys to be coerced to strings
dictionary <- values
names(dictionary) <- keys
dictionary[as.character(c(12, 14))]
#>         12         14 
#>   "Twelve" "Fourteen"

## Both of the above are much simpler to implement and less
## vulnerable to reproduction errors than base R's switch
## function or other alternatives such as nested if/elses
## or complicated subsetting
sapply(
  as.character(c(12, 14)),
  function(x) {
    switch(
      x,
      "11" = "Eleven",
      "12" = "Twelve",
      "13" = "Thirteen",
      "14" = "Fourteen",
      "15" = "Fifteen"
    )
  }
)
#>         12         14 
#>   "Twelve" "Fourteen"
```

`relate` maps a vector of inputs `X` from their position in a vector of keys `A` to the corresponding value in vector `B`. For simple applications it provides a more elegant one-line alternative to creating and querying a named-vector dictionary.
