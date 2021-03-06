#' relatable: Functions for Mapping Key-Value Pairs, Many-to-Many, One-to-Many,
#' and Many-to-One Relations
#'
#' The \code{relatable} package provides two functions to safely map from a
#' vector of keys to a vector of values, determine properties of a given
#' relation, or ensure a relation conforms to a given type, such as
#' many-to-many, one-to-many, injective, surjective, or bijective. Permits
#' default return values for use similar to a vectorised switch statement, as
#' well as safely handling large vectors, NAs, and duplicate mappings.
#'
#'
#' @section Functions:
#'
#'   \describe{ \item{\code{\link{relate}}}{Returns a vector \eqn{Y = F(X)}
#'   where \eqn{F} maps each element of input vector \code{X} from its position
#'   in vector \code{A} to its corresponding position in vector \eqn{B}.}
#'   \item{\code{\link{relation}}}{Returns a function \eqn{F} that maps each
#'   element of input vector \code{X} from its position in vector \code{A} to
#'   its corresponding position in vector \eqn{B}.} }
#'
#' @seealso
#'
#' \itemize{ \item{For a quick introduction to the functions of the package see
#' "Basic Usage" with \code{vignette("relatable-usage")} in R or
#' \href{https://cran.r-project.org/web/packages/relatable/vignettes/relatable-usage.html}{online}}
#' \item{For instructions on how to use the additional arguments of
#' \code{relate} and \code{relation} see "Relation Types and Restrictions" with
#' \code{vignette{"relatable-restrictions"}} in R or
#' \href{https://cran.r-project.org/web/packages/relatable/vignettes/relatable-restrictions.html}{online}}{}
#' }
#'
#' @docType package
#' @name relatable
NULL