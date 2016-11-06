#' Autoplot a qualifier/tag structure
#'
#' @param x A \link{qtag} object
#' @param xvar Column to be used on the x-axis
#' @param yvar Column to be used on the y-axis
#' @param facets Column to be used as facetting variable
#' @param ... Passed on to \code{aes()}
#'
#' @return A ggplot object
#'
#' @importFrom ggplot2 autoplot
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(pocmaj)
#' pocmaj <- qtag(pocmaj, qualifiers=c("core", "depth"))
#' autoplot(pocmaj)
#' autoplot(pocmaj, subset=core=="MAJ-1" & column %in% c("Ca", "Ti"))
#' autoplot(pocmaj, shape="core")
#'
autoplot.qtag.long <- function(x, xvar, yvar, facets, subset, ...) {
  x <- aggregate(x, mean, err=sd(., na.rm = TRUE)/sum(!is.na(.)))
  if(!missing(subset)) {
    x <- x[eval(substitute(subset), envir=x), ]
  }
  qualifiers <- attr(x, "qualifiers")
  values <- attr(x, "values")
  mapping <- aes_string(...)
  types <- sapply(qualifiers, function(qual) class(x[[qual]])[1])
  numqualifiers <- qualifiers[types %in% c("numeric", "integer")]
  nonnumqualifiers <- qualifiers[!(qualifiers %in% numqualifiers) & !(qualifiers %in% mapping)]
  if(numqualifiers > 0) {
    if(missing(xvar) && missing(yvar)) {
      yvar <- numqualifiers[length(numqualifiers)]
      xvar <- values
    } else if(missing(xvar)) {
      if(yvar == values) {
        xvar <- numqualifiers[length(numqualifiers)]
      } else {
        xvar <- values
      }
    } else if(missing(yvar)) {
      if(xvar == values) {
        yvar <- numqualifiers[length(numqualifiers)]
      } else {
        yvar <- values
      }
    }
    facet_scales <- "fixed"
    if(yvar == values) {
      facet_scales <- "free_y"
    } else if(xvar == values) {
      facet_scales <- "free_x"
    }

    ggfacet <- facet_null()
    nonnumindex <- length(nonnumqualifiers)
    if(missing(facets)) {
      if(length(nonnumqualifiers) > 0) {
        # use last non-numeric qualifier
        ggfacet <- facet_wrap(as.formula(paste0("~", nonnumqualifiers[nonnumindex])), scales = facet_scales)
        nonnumindex <- nonnumindex - 1
      } else {
        ggfacet <- facet_null()
      }
    } else if(is.null(facets)) {
      ggfacet <- facet_null()
    } else if(attr(terms.formula(facets), "response") == 1) {
      # 2- sided formula
      ggfacet <- facet_grid(facets, scales = facet_scales)
      chrfacets <- unlist(lapply(attr(terms.formula(facets), "variables")[-1], deparse))
      nonnumqualifiers <- nonnumqualifiers[!(nonnumqualifiers %in% chrfacets)]
      nonnumindex <- length(nonnumqualifiers)
    } else {
      # 1-sided formula
      chrfacets <- unlist(lapply(attr(terms.formula(facets), "variables")[-1], deparse))
      ggfacet <- facet_wrap(facets, scales = facet_scales)
      nonnumqualifiers <- nonnumqualifiers[!(nonnumqualifiers %in% chrfacets)]
      nonnumindex <- length(nonnumqualifiers)
    }

    if((nonnumindex > 0) && !("colour" %in% names(mapping))) {
      mapping <- c(mapping, aes_(colour=as.name(nonnumqualifiers[nonnumindex])))
      nonnumindex <- nonnumindex - 1
    }
    if((nonnumindex > 0) && !("shape" %in% names(mapping))) {
      mapping <- c(mapping, aes_(shape=as.name(nonnumqualifiers[nonnumindex])))
      nonnumindex <- nonnumindex - 1
    }

    errorbars <- NULL
    if("err" %in% names(x)) {
      if(values == xvar) {
        nonvalrange <- range(x[yvar])
        errbarheight <- (nonvalrange[2]-nonvalrange[1]) / 50.0
        errorbars <- geom_errorbarh(aes_string(xmin=sprintf("%s-%s", xvar, "err"),
                                               xmax=sprintf("%s+%s", xvar, "err")),
                                    height=errbarheight)
      } else if(values == yvar) {
        nonvalrange <- range(x[xvar])
        errbarheight <- (nonvalrange[2]-nonvalrange[1]) / 50.0
        errorbars <- geom_errorbar(aes_string(ymin=sprintf("%s-%s", yvar, "err"),
                                              ymax=sprintf("%s+%s", yvar, "err")),
                                   width=errbarheight)
      }
    }

    yrev <- scale_y_reverse()
    if(.is_ad(x[yvar])) {
      yrev <- NULL
    }
    mapping <- c(mapping, aes_(x=as.name(xvar), y=as.name(yvar)))
    class(mapping) <- "uneval"
    return(ggplot(x, mapping) + geom_path() + errorbars + geom_point() + ggfacet + yrev)
  } else {
    stop("Cannot use autoplot with zero numeric qualifiers")
  }
}

#' @export
#' @rdname autoplot.qtag.long
autoplot.qtag.wide <- function(x, ...) {
  autoplot(long(x), ...)
}
