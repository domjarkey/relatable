
<!-- README.md is generated from README.Rmd. Please edit that file -->
relatable
=========

`relatable` provides two easy-to-use, robust functions for mapping from a vector of keys to a vector of values, as well as creating and applying more sophisticated mappings, such as many-to-many, one-to-many, and many-to-one relations. These are primarily designed with two goals in mind:

1.  Producing reusable code that is easier to write and read.
2.  Ensuring relations conform to specified restrictions, for example injectivity or surjectivity, and safely handle nonstandard mappings, including unexpected inputs, NAs, heterogeneous lists containing multiple variable types including other lists, and mappings between vectors of uncertain or unequal lengths.

Installation
------------

You can install `relatable` from github with:

``` r
# install.packages("devtools")
devtools::install_github("domjarkey/relatable")
```

Applications
------------

### A simple key-value dictionary

For basic use, `relate` maps a vector of inputs `X` from their position in a vector of keys `A` to the corresponding value in vector `B`. `relation` returns a function that performs the same mapping for repeated usage.

``` r
library(relatable)
# Use relate() for a one-off mapping of inputs from one vector to another
relate(c("March", "April", "May"), month.name, month.abb)
#> [1] "Mar" "Apr" "May"

# Create a reusable dictionary function with relation().
chem_symbol <- relation(elements$Name, elements$Symbol)

chem_symbol(c("Iron", "Lithium", "Sodium"))
#> [1] "Fe" "Li" "Na"

# Unexpected inputs return <NA> by default, but this can be changed
chem_symbol(c("Sodium", "Adamantium"))
#> [1] "Na" NA

# relate() and relation() have optional arguments to determine the type of
# output and default return values.
chem_symbol <- relation(elements$Name, elements$Symbol,
  default = "Unknown", named = TRUE)

chem_symbol(c("Sodium", "Adamantium"))
#>     Sodium Adamantium 
#>       "Na"  "Unknown"
```

### Ensure expected inputs while manipulating larger data sets

When working with unfamiliar data it can be easy to forget to account for all possible values a variable might take, or worse, typographical entry errors. Using `allow_default = FALSE`, `relatable` functions can flag unexpected inputs to ensure these problems don't arise.

In the following example we use the [DWNOMINATE](https://voteview.com/about) data set assembled by Poole and Rosenthal et al, which estimates the ideological positions of US politicians. Suppose we want to create a new column for our data frame indicating political party by colour (red for Republicans, blue for Democrats):

``` r
## Obtain data for senators in the 113th Congress, spanning 2013-2015.
US_senate_113 <- subset(
  foreign::read.dta("ftp://k7moa.com/junkord/SL01113D21_BSSE_12.DTA"),
  cong == 113
)

## Setting allow_default = FALSE ensures we will be notified of any funny inputs.
US_senate_113$colour <- relate(
  X = US_senate_113$party,
  A = c(100, 200),
  B = c("blue", "red"),
  allow_default = FALSE
)
#> Warning in default_behaviour(x): 328 does not have a valid mapping to an
#> element in the codomain.

#> Warning in default_behaviour(x): 328 does not have a valid mapping to an
#> element in the codomain.

## Woops! Looks like we forgot to allow for the two Independent Senators in the data set,
## coded as 328. Let's try again:
US_senate_113$colour <- relate(
  X = US_senate_113$party,
  A = c(100, 200, 328),
  B = c("blue", "red", "gray30"),
  allow_default = FALSE
)

## No warnings, no worries!
with(
  US_senate_113,
  plot(
    x = dwnom1,
    y = dwnom2,
    col = colour,
    main = "Ideal Points of US Senators",
    xlab = "Economic Issues",
    ylab = "Social Issues"
  )
)
```

![](README-data_frames-1.png)

### Enforce properties of relations between vectors

By default, the `relation_type` is assumed to be a funtcion ("func"), meaning that each input maps to one and only one output. These restrictions can be tightened or loosened depending on your needs. `relation_type` enforces restrictions for mapping types such as "one\_to\_many", "injection", "surjection", and "bijection".

``` r
## Authors have a many-to-many relation with books:
## a book can have multiple authors and authors can write multiple books
my_library <- tibble::tribble(
  ~author,              ~work,
  "Arendt",             "The Human Condition",
  "Austen-Smith",       "Social Choice and Voting Models",
  "Austen-Smith",       "Positive Political Theory",
  "Banks",              "Positive Political Theory",
  "Camus",              "The Myth of Sisyphus",
  "Camus",              "The Rebel",
  "Arendt",             "The Origins of Totalitarianism",
  "Dryzek",             "Theories of the Democratic State",
  "Dunleavy",           "Theories of the Democratic State"
)

relate(
  X = c("Arendt", "Austen-Smith", "Banks", "Dryzek", "Dunleavy"),
  A = my_library$author,
  B = my_library$work,
  atomic = FALSE, # relations with multiple outputs must return lists
  named = TRUE,
  relation_type = "many_to_many"
)
#> $Arendt
#> [1] "The Human Condition"            "The Origins of Totalitarianism"
#> 
#> $`Austen-Smith`
#> [1] "Social Choice and Voting Models" "Positive Political Theory"      
#> 
#> $Banks
#> [1] "Positive Political Theory"
#> 
#> $Dryzek
#> [1] "Theories of the Democratic State"
#> 
#> $Dunleavy
#> [1] "Theories of the Democratic State"

## Duplicate mappings usually return twice, but this can be changed...
relate(
  X = 1:3,
  A = c(1, 2, 2, 3, 4, 5),
  B = c('a', 'b', 'b', 'c', 'd', 'e'),
  relation_type = "many_to_many",
  atomic = FALSE
)
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b" "b"
#> 
#> [[3]]
#> [1] "c"

## Use relate or relation with handle_duplicate_mappings = TRUE to avoid errors resulting
## from duplicate mappings to and from the same inputs. Bijections ensure that each input
## has exactly one unique output.
relate(
  X = 1:3,
  A = c(1, 2, 2, 3, 4, 5),
  B = c('a', 'b', 'b', 'c', 'd', 'e'),
  relation_type = "bijection",
  handle_duplicate_mappings = TRUE
)
#> [1] "a" "b" "c"
```

### Determine relation properties for safer mappings

To illustrate some of the more advanced ways `relatable` can help ensure safer data manipulation, we will use the [emperors](https://github.com/zonination/emperors/blob/master/LICENSE.md) data set of Ancient Roman Emperors assembled by github user [Zoni Nation](https://github.com/zonination).

``` r
emperors <- read.csv(
  "https://raw.githubusercontent.com/zonination/emperors/master/emperors.csv",
  stringsAsFactors = FALSE
)
colnames(emperors)
#>  [1] "index"       "name"        "name.full"   "birth"       "death"      
#>  [6] "birth.cty"   "birth.prv"   "rise"        "reign.start" "reign.end"  
#> [11] "cause"       "killer"      "dynasty"     "era"         "notes"      
#> [16] "verif.who"

## Suppose we want a function to map each emperor to the time of their reign.
## First, let's see that a unique mapping from either name or name.full is possible by
## using relation's report properties argument:
relation(emperors$name.full, emperors$reign.start,
  relation_type = NULL,
  atomic = FALSE,
  report_properties = TRUE)
#> Relation properties:
#> min_one_y_per_x: TRUE
#> min_one_x_per_y: TRUE
#> max_one_y_per_x: FALSE
#> max_one_x_per_y: FALSE

relation(emperors$name, emperors$reign.start,
  relation_type = NULL,
  atomic = FALSE,
  report_properties = TRUE)
#> Relation properties:
#> min_one_y_per_x: TRUE
#> min_one_x_per_y: TRUE
#> max_one_y_per_x: TRUE
#> max_one_x_per_y: FALSE

## Neither mapping fulfils the criterion of max_one_y_per_x, but this is not a problem: in
## the later years of the Roman Empire, some emperors were co-rulers whose reigns began at
## the same time.
relate(c("0305-05-01", "0337-05-22"), emperors$reign.start, emperors$name,
  named = TRUE, relation_type = NULL, atomic = FALSE)
#> $`0305-05-01`
#> [1] "Constantius I" "Galerius"      "Severus II"   
#> 
#> $`0337-05-22`
#> [1] "Constantine II" "Consantius II"  "Constans"

## However, we can infer from max_one_y_per_x = FALSE that some elements of name.full are
## non-unique. This is because both Vespasian and his eldest son and successor Titus took
## the same imperial title.
relate(c("Vespasian", "Titus"), emperors$name, emperors$name.full,
  named = TRUE)
#>                                   Vespasian 
#> "TITVS FLAVIVS CAESAR VESPASIANVS AVGVSTVS" 
#>                                       Titus 
#> "TITVS FLAVIVS CAESAR VESPASIANVS AVGVSTVS"

## Hence we can determine that name and not name.full is a better choice for our mapping
## function.
reign_start <- relation(emperors$name, emperors$reign.start)
reign_start("Constantine the Great")
#> [1] "0306-07-25"

## Repeating the vector A can let us return multiple variables at once to return an n-tuple
nice_date <- function(s) {
  d <- as.Date(s, "%Y-%m-%d")
  return(format.Date(d, "%d %B, %Y AD"))
}

reign_duration <- relation(
  rep(emperors$name, 2),
  nice_date(c(emperors$reign.start, emperors$reign.end)),
  relation_type = NULL,
  atomic = FALSE, named = TRUE
)
reign_duration(c("Vespasian", "Titus", "Domitian"))
#> $Vespasian
#> [1] "21 December, 69 AD" "24 June, 79 AD"    
#> 
#> $Titus
#> [1] "24 June, 79 AD"      "13 September, 81 AD"
#> 
#> $Domitian
#> [1] "14 September, 81 AD" "18 September, 96 AD"

## Or just for fun...
obituary <- with(
  emperors,
  relation(
    A = rep(name, 3),
    B = c(
      paste0("Born in ", birth.cty, ", ", birth.prv, " on ", nice_date(birth)),
      paste0("Came to power by ", rise, " on ", nice_date(reign.start)),
      paste0("Died from ", cause, " by ", killer, " on ", nice_date(death))
    ),
    relation_type = NULL,
    atomic = FALSE, named = TRUE
  )
)

obituary(
  c("Marcus Aurelius", "Commodus", "Pertinax", "Didius Julianus", "Septimus Severus", "Caracalla")
)
#> $`Marcus Aurelius`
#> [1] "Born in Rome, Italia on 26 April, 121 AD"               
#> [2] "Came to power by Birthright on 07 March, 161 AD"        
#> [3] "Died from Natural Causes by Disease on 17 March, 180 AD"
#> 
#> $Commodus
#> [1] "Born in Lanuvium, Italia on 31 August, 161 AD"                     
#> [2] "Came to power by Birthright on 01 January, 177 AD"                 
#> [3] "Died from Assassination by Praetorian Guard on 31 December, 192 AD"
#> 
#> $Pertinax
#> [1] "Born in Alba, Italia on 01 August, 126 AD"                             
#> [2] "Came to power by Appointment by Praetorian Guard on 01 January, 193 AD"
#> [3] "Died from Assassination by Praetorian Guard on 28 March, 193 AD"       
#> 
#> $`Didius Julianus`
#> [1] "Born in Milan, Italia on 30 January, 133 AD"     
#> [2] "Came to power by Purchase on 28 March, 193 AD"   
#> [3] "Died from Execution by Senate on 01 July, 193 AD"
#> 
#> $`Septimus Severus`
#> [1] "Born in Leptis Magna, Libya on 11 April, 145 AD"           
#> [2] "Came to power by Seized Power on 09 April, 193 AD"         
#> [3] "Died from Natural Causes by Disease on 04 February, 211 AD"
#> 
#> $Caracalla
#> [1] "Born in Lugdunum, Gallia Lugdunensis on 04 April, 188 AD"    
#> [2] "Came to power by Birthright on 01 January, 198 AD"           
#> [3] "Died from Assassination by Other Emperor on 08 April, 217 AD"
```
