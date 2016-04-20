

lithozones_example <- list(
               name="Lithozones",
               .breaks=data.frame(top=c(0, 5, 10, 20), bottom=c(5, 10, 20, 25),
                                 name=c("Unit 1", "Unit 2", "Unit 3", "Unit 4"), stringsAsFactors = FALSE),
               .info=data.frame(name=c("Unit 1", "Unit 1", "Unit 2", "Unit 2", "Unit 3", "Unit 3",
                                      "Unit 4", "Unit 4"),
                               param=rep(c("gs", "description"), 4),
                               param.value=c("fg", "Clay, sandy.", "fg", "Clay, silty",
                                             "cg", "Sand", "fg", "Silt, sandy"), stringsAsFactors = FALSE)
               )
class(lithozones_example) <- c("linseg", "list")

#' Classify depths from linear segments
#'
#' @param linseg A \code{linseg} object (see \link{linseg})
#' @param depth A vector of depths to classify
#' @param value The parameter to return (defaults to 'name')
#' @param preference Preference for upper or lower unit should a depth match more than one unit.
#'
#' @return A character or factor
#' @export
#'
#' @examples
#' data(lithozones_example)
#' linseg.classify(lithozones_example, 5)
#' linseg.classify(lithozones_example, 5, preference="lower")
#' linseg.classify(lithozones_example, seq(0, 26, 2))
#' linseg.classify(lithozones_example, seq(0, 26, 2), param="gs") # gives grain size for each depth
#' linseg.classify(lithozones_example, seq(0, 26, 2), param="gss") # gives NAs
#'
linseg.classify <- function(linseg, depth, param="name", preference=c("upper", "lower")) {
  preference <- match.arg(preference) # enforces one of upper or lower
  return(sapply(depth, function(depth) {
    rows <- linseg$.breaks[(depth >= linseg$.breaks$top) & (depth <= linseg$.breaks$bottom),]
    if(nrow(rows) == 0) {
      return(NA)
    } else {
      name <- ifelse(preference == "upper", as.character(rows$name[which.min(rows$top)]),
                             as.character(rows$name[which.max(rows$top)]))
      if(param=="name") {
        return(name)
      } else {
        vals <- linseg$.info$param.value[linseg$.info$param == param & linseg$.info$name == name]
        if(length(vals) == 0) {
          return(NA)
        } else if(length(vals) > 1) {
          warning("Multiple values encountered for ", name, " for param ", param)
          return(vals[1])
        } else { # length == 1
          return(vals)
        }
      }
    }
  }))
}

#' Get or assign param values for linear segments
#'
#' @param linseg A \code{linseg} object (see \link{linseg})
#' @param name The segment name(s) for which to return parameters
#' @param param The parameter name to return
#'
#' @return A character vector of param values, or a \code{data.frame} if
#'         multiple values are passed for name and param
#' @export
#'
#' @examples
#' data(lithozones_example)
#' linseg.param(lithozones_example, "Unit 1", "gs")
#'
linseg.param <- function(linseg, name, param) {
  parammissing <- FALSE
  namemissing <- FALSE
  if(missing(name)) {
    name <- unique(linseg$.breaks$name)
    namemissing <- TRUE
  }
  if(missing(param)) {
    param <- unique(linseg$.info$param)
    parammissing <- TRUE
  }
  if(length(param == 1) && param == "name") {
    return(name)
  }
  rows <- linseg$.info[(linseg$.info$param %in% param) & (linseg$.info$name %in% name),]
  if((parammissing && length(name) > 1) || (namemissing > 1 && length(param) > 1) ||
     parammissing && namemissing || (length(name) > 1 && length(param) > 1)) {
    df <- as.data.frame(lapply(c("name", param), function(p) linseg.param(linseg, name, p)))
    rownames(df) <- NULL
    names(df) <- c("name", param)
    return(df)
  } else if(length(param) == 1) {
    # return a vector involving names
    # (or list if there are multiple values for the param)
    return(sapply(name, function(n) {
      r <- rows$param.value[rows$name == n]
      ifelse(length(r) == 0, NA, r)
    }))
  } else if(length(name) == 1) {
    # return a vector involving params
    # (or list if there are multiple values for the param)
    return(sapply(param, function(p) {
      r <- rows$param.value[rows$param == p]
      ifelse(length(r) == 0, NA, r)
    }))
  } else {
    # should never happen
    stop("Invalid input to function linseg.param()")
  }
}
