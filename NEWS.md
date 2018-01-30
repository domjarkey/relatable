NEWS
================

<!-- README.md is generated from NEWS.Rmd. Please edit that file -->

# relatable 1.0.0

## Features

`relatable` provides two easy-to-use, robust functions for mapping from
a vector of keys to a vector of values, as well as creating and applying
more sophisticated mappings, such as many-to-many, one-to-many, and
many-to-one relations. These are primarily designed with two goals in
mind:

1.  Producing reusable code that is easier to write and read.
2.  Ensuring relations conform to specified restrictions, for example
    injectivity or surjectivity, and safely handle nonstandard mappings,
    including unexpected inputs, NAs, heterogeneous lists containing
    multiple variable types including other lists, and mappings between
    vectors of uncertain or unequal lengths.

## Functions

`relate` returns a vector *Y = F(X)* where *F* maps each element of
input vector `X` from its position in vector `A` to its corresponding
position in vector `B`. Can be applied as a vectorised key-value
dictionary with an optional default return value. Additional options
restrict mapping types so relation *F* must be a function, injective,
surjective, etc.

`relation` returns a reusable function *F* that performs the same
operation as `relate`. In addition to providing a reusable function, if
`handle_duplicate_mappings = TRUE`, `relation` checks for and eliminates
duplicate mappings that would be invalid inputs for `relate`. If
`report_properties = TRUE`, `relation` also prints the restrictions the
mapping from `A` to `B` conforms to.
