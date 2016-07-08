

#' Coerce to linear segment object
#'
#' Takes a \code{data.frame} with top/bottom/segment name information and returns a \code{linseg}
#' object.
#'
#' @param x A \code{data.frame} with top/bottom/segment name columns
#' @param top The name of the colunn containing the tops (as a string)
#' @param bottom The name of the column containing the bottom (as a string)
#' @param names The name of the column containing the names (as a string)
#' @param name The name associated with the linear segments (optional)
#'
#' @return A \code{linseg} object.
#' @export
#'
#' @examples
#' zonesdf <- data.frame(unit_top=c(0, 2, 4, 6, 8),
#'                       unit_bottom=c(2, 4, 6, 8, 10),
#'                       unit_name=c("Unit 1", "Unit 2", "Unit 3", "Unit 4", "Unit 5"),
#'                       unit_litho=c("sand", "silt", "clay", "sand", "clay"))
#' lsobj <- as.linseg(zonesdf, top="unit_top", bottom="unit_bottom", names="unit_name")
#'
as.linseg <- function(x, top="top", bottom="bottom", names="name", name=NA) {
  .breaks <- data.frame(top=as.numeric(x[[top]]),
                        bottom=as.numeric(x[[bottom]]),
                        name=as.character(x[[names]]), stringsAsFactors = FALSE)
  .info <- x[ ,!(colnames(x) %in% c(top, bottom))]
  .info$name <- .info[[names]]; .info[[names]] <- NULL
  .info <- reshape2::melt(.info, id.vars="name",
                variable.name="param", value.name="param.value")
  if(is.null(.info$param)) {
    .info$param <- NA
    .info$param.value <- NA
  }
  .info$param <- as.character(.info$param)
  .info$name <- as.character(.info$name)
  out <- list(.breaks=.breaks, .info=.info, name=name)
  class(out) <- c("linseg", "list")
  return(out)
}

#' Create a linear segment object
#'
#' Creates a linear segment object from minimal information.
#'
#' @param breaks A vector of segment breaks (must be at least length 2)
#' @param names A vector of names for the segments
#' @param name The name associated with the linear segments (optional)
#' @param ... Additional vectors of length equal to length of \code{names} that correspond
#'            to additional parameters (e.g. lithology, grain size)
#'
#' @return A \code{linseg} object.
#' @export
#'
#' @examples
#' ls <- linseg(c(0, 1, 2, 3))
#' plot(ls)
#' ls <- linseg(c(0, 1, 2, 3), names=c("Unit 1", "Unit 2", "Unit 3"),
#'              grain_size=c("fg", "mg", "cg"))
#' plot(ls)
#'
linseg <- function(breaks, names, name=NA, ...) {
  if(missing(breaks)) stop("At least two values must be specified as breaks")
  if(length(breaks) < 2) stop("At least two values must be specified as breaks")
  if(missing(names)) {
    names <- paste("Zone", 1:(length(breaks)-1))
  }
  .breaks <- sapply(2:length(breaks), function(x) {c(breaks[x-1], breaks[x], names[x-1])})
  .breaks <- data.frame(top=as.numeric(.breaks[1,]),
                        bottom=as.numeric(.breaks[2,]),
                        name=as.character(.breaks[3,]),
                        stringsAsFactors = FALSE)
  .info <- reshape2::melt(data.frame(name=as.character(names), ..., stringsAsFactors = FALSE),
                          id.vars="name", variable.name="param", value.name="param.value")
  if(is.null(.info$param)) {
    .info$param <- NA
    .info$param.value <- NA
  }
  out <- list(.breaks=.breaks, .info=.info, name=name)
  class(out) <- c("linseg", "list")
  return(out)
}


#' Classify depths from linear segments
#'
#' @param linseg A \code{linseg} object (see \link{linseg})
#' @param depth A vector of depths to classify
#' @param param The parameter to return (defaults to 'name')
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
#' linseg.param(lithozones_example, name="Unit 1")
#' linseg.param(lithozones_example, param="gs")
#' linseg.param(lithozones_example, param=c("gs", "fishing"), name=c("Unit 1", "Unit 8"))
#' linseg.param(lithozones_example, param=c("gs", "fishing"))
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
  if((length(param) == 1) && (param == "name")) {
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

#' Plot a linear segment object
#'
#' Provide a visual of a \code{linseg} object.
#'
#' @param x A \code{linseg} object
#' @param label_by A parameter with which to label segments or \code{NULL} for no label.
#' @param reversed Logical describing if the y axis should be reversed.
#' @param ylab Label for the Y axis
#' @param xlab Label for the X axis
#' @param sub Subtitle for the plot
#' @param xright Right X value for plot rectangles (can be a vector)
#' @param xleft Left X value for plot rectangles (can be a vector)
#' @param xlim vector of length 2 for plot X range.
#' @param ylim vector of length 2 for plot Y range (overrides \code{reversed} parameter)
#' @param col Colour vector for segments
#' @param label.col Colour vector for labels
#' @param label.margin Margin vector for labels
#' @param ... Formatting options to be passed on to \code{rect}.
#'
#' @export
#'
#' @examples
#' data(lithozones_example)
#' plot(lithozones_example)
#'
plot.linseg <- function(x, ..., label_by="name", reversed=TRUE, ylab=NA, xlab=NA, sub=NA,
                        xright=1, xleft=0, xlim=c(0, 4), ylim=NULL, col=NULL,
                        label.col="black", label.margin=0.1) {
  yvax <- c(x$.breaks$top, x$.breaks$bottom)
  if(is.null(ylim)) {
    ylim <- ifelse(rep(reversed, 2), c(max(yvax), min(yvax)), c(min(yvax), max(yvax)))
  }
  plot(c(0, 1), c(min(yvax), max(yvax)), ylim=ylim, xlim=c(0, 4), axes=FALSE, pch=".",
       xlab=xlab, ylab=ylab, sub=sub)
  linseg.rects(x, xright=xright, xleft=xleft, col=col, ...)
  if(!(is.null(label_by) || is.na(label_by))) {
    linseg.labels(x, xright, label_by = label_by, col=label.col, margin=label.margin)
  }
  axis(2)
  title(x$name)
}

linseg.rects <- function(ls, xleft=0, xright=1, col=NULL, ...) {
  if(is.null(col)) {
    col=2:(2+nrow(ls$.breaks))
  }
  rect(xleft, ls$.breaks$top, xright, ls$.breaks$bottom, col=col, ...)
}

linseg.labels <- function(ls, x=1, label_by="name", col="black",
                          margin=0.1, adj=c(0, 0.5), ...) {
  avgs <- (ls$.breaks$top + ls$.breaks$bottom) / 2.0
  text(x + margin, avgs, labels=linseg.classify(ls, depth = avgs, param = label_by),
       adj=adj)
}
