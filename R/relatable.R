#' relatable: Functions for Mapping Key-Value Pairs, Many-to-Many, One-to-Many, and Many-to-One Relations
#'
#' The \code{relatable} package provides two functions to safely map from a vector of keys to a vector of values, determine properties of a given relation, or ensure a relation conforms to a given type, such as many-to-many, one-to-many, injective, surjective, or bijective. Permits default return values for use similar to a vectorised switch statement, as well as safely handling large vectors, NAs, and duplicate mappings.
#'
#'
#' @section Functions:
#'
#' \describe{
#'   \item{\code{\link{relate}}}{Returns a vector \eqn{Y = F(X)} where \eqn{F} maps each element of input vector \code{X} from its position in vector \code{A} to its corresponding position in vector \eqn{B}.}
#'   \item{\code{\link{relation}}}{Returns a function \eqn{F} that maps each element of input vector \code{X} from its position in vector \code{A} to its corresponding position in vector \eqn{B}.}
#' }
#'
#' \code{\link{relate}}
#'
#' @seealso
#'
#' \code{\link{restrictions}}
#'
#' \code{\link{relation}}
#'
#' \href{https://github.com/domjarkey/relatable/blob/master/man/relate.Rd}{relate}
#'
#' \href{https://github.com/domjarkey/relatable/blob/master/man/relate.Rd}{relate}
#'
#' @docType package
#' @name relatable
NULL