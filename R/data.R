#' Data from the periodic table of elements
#'
#' A dataset containing atomic numbers, chemical symbols, and
#' names of 118 elements.
#'
#' @format A data frame with 118 rows and 3 variables:
#' \describe{
#'   \item{Z}{Atomic number}
#'   \item{Symbol}{Chemical symbol}
#'   \item{Name}{Name of element}
#' }
#' @source \url{https://en.wikipedia.org/wiki/Symbol_(chemistry)}
"elements"
#' Data of US state names and abbreviations
#'
#' A dataset containing names of US states and their abbreviations under various standards
#' including ANSI, ISO, USPS, USCG, GPO, Associated Press.
#'
#' @format A data frame with 51 rows and 10 variables:
#' \describe{
#'   \item{Name}{US Region Name}
#'   \item{Type}{State, State (Commonwealth), or Federal District}
#'   \item{ANSI}{American National Standards Institute 2-letter code}
#'   \item{ANSI_digit}{American National Standards Institute 2-digit code. This is stored as a character to ensure all entries are two characters long.}
#'   \item{USPS}{United States Postal Service 2-letter code}
#'   \item{USCG}{United States Coast Guard 2-letter code (used as prefices for vessel numbers).}
#'   \item{GPO}{US Government Printing Office abbreviations}
#'   \item{AP}{Associated Press Stylebook abbreviations}
#' }
#' @source \url{https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations}
"US_states"