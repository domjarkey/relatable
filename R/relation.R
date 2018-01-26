#' @rdname relate
#'
#' @export
relation <- function(A, B,
  default = NA,
  atomic = TRUE,
  named = FALSE,
  allow_default = TRUE,
  heterogeneous_outputs = FALSE,
  relation_type = "func",
  restrictions = list(),
  handle_duplicate_mappings = TRUE,
  report_properties = FALSE) {
  # list of valid arguments
  VALID_TYPES <- c(
    "many_to_many",
    "one_to_many",
    "many_to_one",
    "func",
    "injection",
    "surjection",
    "bijection"
  )
  VALID_RESTRICTIONS <- c(
    "min_one_y_per_x",
    "min_one_x_per_y",
    "max_one_y_per_x",
    "max_one_x_per_y"
  )
  # Set properties of relation to enforce
  props <- list(
    "min_one_y_per_x" = FALSE,
    "min_one_x_per_y" = FALSE,
    "max_one_y_per_x" = FALSE,
    "max_one_x_per_y" = FALSE
  )
  if (is.null(relation_type)) { # If relation_type is not specified, set properties by restrictions list
    for (r in names(restrictions)) {
      if (r %in% VALID_RESTRICTIONS) {
        props[r] <- restrictions[r]
      } else {
        stop('\"', r, '\"', ' is not a valid restriction. See help documentation for a list of valid restrictions.')
      }
    }
  } else { # If relation_type is specified, set properties by type
    if (!(relation_type %in% VALID_TYPES)) {
      stop('\"', relation_type, '\"', ' is not a valid type of relation. See help documentation for a list of valid types.')
    } else {
      props <- switch(
        relation_type,
        many_to_many =
          list(
            "min_one_y_per_x" = FALSE,
            "min_one_x_per_y" = FALSE,
            "max_one_y_per_x" = FALSE,
            "max_one_x_per_y" = FALSE
          ),
        one_to_many =
          list(
            "min_one_y_per_x" = FALSE,
            "min_one_x_per_y" = FALSE,
            "max_one_y_per_x" = FALSE,
            "max_one_x_per_y" = TRUE
          ),
        many_to_one =
          list(
            "min_one_y_per_x" = FALSE,
            "min_one_x_per_y" = FALSE,
            "max_one_y_per_x" = TRUE,
            "max_one_x_per_y" = FALSE
          ),
        func =
          list(
            "min_one_y_per_x" = TRUE,
            "min_one_x_per_y" = FALSE,
            "max_one_y_per_x" = TRUE,
            "max_one_x_per_y" = FALSE
          ),
        injection =
          list(
            "min_one_y_per_x" = TRUE,
            "min_one_x_per_y" = FALSE,
            "max_one_y_per_x" = TRUE,
            "max_one_x_per_y" = TRUE
          ),
        surjection =
          list(
            "min_one_y_per_x" = TRUE,
            "min_one_x_per_y" = TRUE,
            "max_one_y_per_x" = TRUE,
            "max_one_x_per_y" = FALSE
          ),
        bijection =
          list(
            "min_one_y_per_x" = TRUE,
            "min_one_x_per_y" = TRUE,
            "max_one_y_per_x" = TRUE,
            "max_one_x_per_y" = TRUE
          )
      )
    }
  }
  # Check atomic outputs are allowed
  if (atomic && (props$max_one_y_per_x == FALSE)) {
    stop('Many-to-many and one-to-many relations can only return list vectors. Use atomic = FALSE.')
  }
  # Check properties as necessary
  if (handle_duplicate_mappings == FALSE) {
    if (sum(unlist(props)) > 0) {
      if (length(A) > length(B) && props$min_one_y_per_x) {
        stop("Vector A is larger than B There may exist elements in the domain that do not map to an element in the codomain.")
      }
      if (length(A) < length(B) && props$min_one_x_per_y) {
        stop("Vector B is larger than A There may exist elements in the codomain that do not map from an element in the domain.")
      }
      if (length(unique(A)) != length(A) && props$max_one_y_per_x) {
        stop("Vector A contains duplicate elements which may map to different elements in the codomain.")
      }
      if (length(unique(B)) != length(B) && props$max_one_x_per_y) {
        stop("Vector B contains duplicate elements which may map from different elements in the domain.")
      }
    }
  }
  if (handle_duplicate_mappings || report_properties) {
    # Check and report each property thoroughly
    precise_properties <- list(
      "min_one_y_per_x" = FALSE,
      "min_one_x_per_y" = FALSE,
      "max_one_y_per_x" = TRUE,
      "max_one_x_per_y" = TRUE
    )
    # Choose an arbitrary default values c and d that are not elements of A and B respectively
    c <- 0; d <- 0
    while (c %in% A) {
      c <- c + 1
    }
    while (d %in% B) {
      d <- d + 1
    }
    # Compute the image of relation R(A) and image of inverse relation R_inv(B)
    im <- relate(A, A, B, default = d, atomic = FALSE, relation_type = NULL, named = F)
    inv_im <- relate(B, B, A, default = c, atomic = FALSE, relation_type = NULL, named = F)
    # Check if min_one_y_per_x holds
    if (sum(sapply(im, function(b) compare::compareCoerce(b, d)$result)) == 0) {
      precise_properties$min_one_y_per_x = TRUE
    }
    # if (sum(suppressWarnings(im == D), na.rm = TRUE) == 0) precise_properties$min_one_y_per_x = TRUE
    # Check if min_one_x_per_y holds
    if (sum(sapply(inv_im, function(a) compare::compareCoerce(a, c)$result)) == 0) {
      precise_properties$min_one_x_per_y = TRUE
    }
    # if (sum(suppressWarnings(inv_im == D), na.rm = TRUE) == 0) precise_properties$min_one_x_per_y = TRUE
    # Check if max_one_y_per_x does not hold
    for (i in seq_len(min(length(A), length(B)))) {
      if (compare::compareCoerce(B[i], unique(im[[i]]))$result == FALSE) {
        precise_properties$max_one_y_per_x = FALSE
        break
      }
    }
    # Check if max_one_x_per_y does not hold
    for (i in seq_len(min(length(A), length(B)))) {
      if (compare::compareCoerce(A[i], unique(inv_im[[i]]))$result == FALSE) {
        precise_properties$max_one_x_per_y = FALSE
        break
      }
    }
    # Report properties
    if (report_properties) {
      cat("Relation properties:\n")
      print(unlist(precise_properties))
    }
    # Compare true properties to restrictions
    violated_restrictions <- unlist(precise_properties) < unlist(props)
    if (sum(violated_restrictions) > 0) {
      warning(
        "Restrictions violated:\n",
        paste(names(props)[violated_restrictions], collapse = "; ")
      )
    }
  }
  # Define functions that compute Y = R(X)
  default_behaviour <- ifelse(
    allow_default,
    function(x) default,
    function(x) stop(x, " does not have a valid mapping to an element in the codomain.")
  )
  # If return elements y of Y as lists or atomic vectors
  apply_func <- ifelse(
    heterogeneous_outputs,
    lapply,
    sapply
  )
  rel <- ifelse(
    props$max_one_y_per_x,
    function(x) {
      if (x %in% A || (is.na(x) && list(x) %in% A)) {
        x_occurrences <- vapply(A,
          function(a) compare::compareCoerce(a, x)$result,
          FUN.VALUE = logical(1))
        x_indeces <- which(x_occurrences)
        if (length(x_indeces) != 0) {
          y <- apply_func(
            x_indeces,
            function(x_index) {
              ifelse(
                x_index <= length(B),
                unique(B[x_index]),
                default_behaviour(x)
              )
            }
          )
          return(y[[1]])
        }
      } else return(default_behaviour(x))
    },
    function(x) {
      if (x %in% A || (is.na(x) && list(x) %in% A)) {
        x_occurrences <- vapply(A,
          function(a) compare::compareCoerce(a, x)$result,
          FUN.VALUE = logical(1))
        x_indeces <- which(x_occurrences)
        if (length(x_indeces) != 0) {
          y <- apply_func(
            x_indeces,
            function(x_index) {
              ifelse(
                x_index <= length(B),
                unique(B[x_index]),
                default_behaviour(x)
              )
            }
          )
          return(y)
        }
      } else return(default_behaviour(x))
    }
  )
  # Return relation rel as a function based on values of named and atomic
  if (named) {
    if (atomic) {
      rel_func <- function(X) {
        Y <- as.list(rep(default, length(X)))
        names(Y) <- as.character(X)
        for (i in seq_along(X)) {
          Y[[i]] <- unname(rel(X[[i]]))
        }
        unlist(Y)
      }
    } else {
      rel_func <- function(X) {
        Y <- as.list(rep(default, length(X)))
        names(Y) <- as.character(X)
        for (i in seq_along(X)) {
          Y[[i]] <- unname(rel(X[[i]]))
        }
        Y
      }
    }
  } else {
    if (atomic) {
      rel_func <- function(X) {
        Y <- rep(default, length(X))
        for (i in seq_along(X)) {
          Y[[i]] <- unname(rel(X[[i]]))
        }
        Y
      }
    } else {
      rel_func <- function(X) {
        Y <- as.list(rep(default, length(X)))
        for (i in seq_along(X)) {
          Y[[i]] <- unname(rel(X[[i]]))
        }
        Y
      }
    }
  }
  return(invisible(rel_func))
}
