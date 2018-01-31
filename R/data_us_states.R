#' Data of US state names and abbreviations
#'
#' A dataset containing names of US states and their abbreviations under various standards
#' including ANSI, ISO, USPS, USCG, GPO, Associated Press.
#'
#' @format A data frame with 51 rows and 10 variables:
#' \describe{
#'   \item{Name}{US Region Name}
#'   \item{Type}{State, State (Commonwealth), or Federal District}
#'   \item{Capital}{State Capital}
#'   \item{ANSI}{American National Standards Institute 2-letter code}
#'   \item{ANSI_digit}{American National Standards Institute 2-digit code. This is stored as a character to ensure all entries are two characters long}
#'   \item{ISO}{International Organization for Standardization 3166-2 code for US States}
#'   \item{USPS}{United States Postal Service 2-letter code}
#'   \item{USCG}{United States Coast Guard 2-letter code (used as prefices for vessel numbers)}
#'   \item{GPO}{US Government Printing Office abbreviations}
#'   \item{AP}{Associated Press Stylebook abbreviations}
#' }
#' @source \url{https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations}
"US_states"