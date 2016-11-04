

#' Construct a qualifier/tag structure
#'
#' @param df A \code{data.frame} or similar object
#' @param values Column names containing the values of interest
#' @param qualifiers Column names of qualifying values
#' @param tags Column names of tag values
#' @param summarised Pass \code{TRUE} for pre-summarised data.
#' @param quiet Use \code{quiet=TRUE} to suppress error messages
#'
#' @return An object of type \code{qtag}, which is essentially the unchanged
#'   input with qualifiers, values, and tags information attached.
#'
#' @export
#'
#' @examples
#' data("pocmaj")
#' pocmaj <- qtag(pocmaj, qualifiers = c("core", "depth"))
#' long(pocmaj)
#' aggregate(pocmaj)
#' aggregate(long(pocmaj))
#'
qtag <- function(df, values, qualifiers, tags, summarised=FALSE, quiet=FALSE) {
  if("qtag" %in% class(df)) return(df)
  dfnames <- names(df)
  if(missing(qualifiers)) {
    df$.row_id <- 1:nrow(df)
    qualifiers <- ".row_id"
  } else {
    qualifiers <- as.character(qualifiers)
  }
  if(missing(tags)) {
    tags <- NULL
  } else {
    tags <- as.character(tags)
  }
  if(missing(values)) {
    valuecol <- dfnames[!(dfnames %in% qualifiers) & !(dfnames %in% tags)]
    if(!quiet) message("Assuming value colums ", paste0("'", valuecol, "'", collapse = ", "))
  } else {
    valuecol <- as.character(values)
    if(any(!(valuecol %in% dfnames))) stop("Could not find at least one column in value columns")
  }

  if(!quiet) {
    ignored <- dfnames[!(dfnames %in% qualifiers) & !(dfnames %in% tags) & !(dfnames %in% valuecol)]
    if(length(ignored) > 0) message("Ignoring columns ", paste0("'", ignored, "'", collapse=", "))
  }

  attr(df, "qualifiers") <- qualifiers
  attr(df, "values") <- valuecol
  attr(df, "tags") <- tags
  attr(df, "summarised") <- summarised
  if(length(valuecol) > 1) {
    class(df) <- c("qtag.wide", "qtag", class(df))
  } else {
    class(df) <- c("qtag.long", "qtag", class(df))
  }
  return(df)
}



#' Convert data to long format
#'
#' @param x A \link{qtag} object
#' @param ... Passed to other methods
#'
#' @return A (possibly unchanged) \code{qtag.long} data.frame
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- qtag(pocmaj, qualifiers = c("core", "depth"))
#' long(pocmaj)
#'
long <- function(x, ...) {
  UseMethod("long")
}


#' Convert data to wide format
#'
#' @param x A \link{qtag} object
#' @param ... Passed to other methods
#'
#' @return A (possibly unchanged) \code{qtag.wide} data.frame
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- qtag(pocmaj, qualifiers = c("core", "depth"))
#' pocmajlong <- long(pocmaj)
#' wide(pocmajlong)
#' wide(pocmaj)
#'
wide <- function(x, ...) {
  UseMethod("wide")
}
# brick <- function(x, ...) UseMethod("brick") not implemented

#' @rdname long
#' @export
long.qtag.long <- function(x, ...) {
  return(x)
}

#' @rdname long
#' @export
long.qtag.wide <- function(qtag, varname="column", quiet=FALSE) {
  valuecol <- attr(qtag, "values")
  qualifiers <- attr(qtag, "qualifiers")
  tags <- attr(qtag, "tags")
  dfmelt <- reshape2::melt(qtag[c(qualifiers, valuecol)], id.vars=qualifiers, measure.vars=valuecol, value.name="values", variable.name=varname)
  if(length(tags) > 0) {
    dfmelt <- merge(dfmelt, qtag[c(qualifiers, tags)], by=qualifiers, all.x=TRUE)
  }
  attr(dfmelt, "values") <- "values"
  attr(dfmelt, "qualifiers") <- c(qualifiers, varname)
  attr(dfmelt, "tags") <- tags
  attr(dfmelt, "summarised") <- attr(qtag, "summarised")
  class(dfmelt) <- c("qtag.long", "qtag", class(dfmelt))
  if(!quiet) message("Assigning values column 'values' and qualifier '", varname, "'")
  return(dfmelt)
}

#' @rdname wide
#' @export
wide.qtag.wide <- function(x) {
  return(x)
}

#' @rdname wide
#' @export
wide.qtag.long <- function(qtag, colvar, fun.aggregate, ..., quiet=FALSE) {
  qualifiers <- attr(qtag, "qualifiers")
  if(missing(colvar)) {
    # assume it is the last qualifier
    colvar <- qualifiers[length(qualifiers)]
    if(!quiet) message("Assuming column variable '", colvar, "'")
  }
  if(missing(fun.aggregate)) {
    # assume mean
    fun.aggregate <- mean
    if(!attr(qtag, "summarised") && !quiet) message("Assuming aggregation function 'mean'")
  }
  castvars <- qualifiers[qualifiers != colvar]

  dfwide <- reshape2::dcast(qtag, formula=as.formula(paste0(paste0("`", castvars, "`", collapse="+"), "~`", colvar, "`")),
                  fun.aggregate=fun.aggregate, value.var=attr(qtag, "values"), ...)
  dfnames <- names(df)
  attr(dfwide, "qualifiers") <- castvars
  attr(dfwide, "values") <- dfnames[!(dfnames %in% castvars)]
  attr(dfwide, "summarised") <- TRUE
  class(dfwide) <- c(class(dfwide), "qtag", "qtag.wide")
  return(dfwide)
}


#' Group a qualifier/tag structure
#'
#' Essentially a shortcut for grouping a \link{qtag} object by its qualifiers
#'
#' @param qtag
#'
#' @return A \code{dplyr} grouped data frame
#' @export
#'
#' @examples
#' data(pocmaj)
#' library(dplyr)
#' pocmajqt <- qtag(pocmaj, qualifiers = c("core", "depth"))
#' pocmajqt %>% group() %>% summarise(mean(Ca))
#' # equivalent to
#' pocmaj %>% group_by(core, depth) %>% summarise(mean(Ca))
#'
#'
group <- function(qtag) {
  qualifiers <- attr(qtag, "qualifiers")
  do.call(dplyr::group_by_, c(list(qtag), as.list(qualifiers)))
}


#' Aggregate/Summarise a qualifier/tag structure
#'
#' Summarises a \link{qtag} object such that one value exists for every unique
#' qualifier combination. This is useful for summarising replicates.
#'
#' @param x A \link{qtag} object
#' @param ... A parameter including at least one unnamed parameter for summarising values.
#'  Additional parameters may be used for aggregating a long format.
#'
#' @return A (possibly unchanged) \code{qtag} object
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmajqt <- qtag(pocmaj, qualifiers=c("core", "depth"))
#' aggregate(pocmajqt)
#' aggregate(pocmajqt, mean)
#' aggregate(long(pocmajqt), mean, sd, length)
#'
aggregate.qtag.long <- function(x, ...) {
  if(attr(x, "summarised")) return(x)

  qualifiers <- attr(x, "qualifiers")
  funformats <- generate.call(...)
  argnames <- names(funformats)
  values <- attr(x, "values")
  sumargs <- list()
  sumargs[[".vals"]] <- gsub(x=funformats[1], pattern="%s", replacement=values, fixed=TRUE)
  tags <- c()
  if(length(funformats) > 1) {
    for(i in 2:length(funformats)) {
      sumargs[[argnames[i]]] <- gsub(x=funformats[i], pattern="%s", replacement=values, fixed=TRUE)
    }
    tags <- argnames[2:length(argnames)]
  }
  dfs <- data.frame(do.call(dplyr::summarise_, c(list(group(x)), as.list(sumargs))))
  dfs <- plyr::rename(dfs, c(".vals"=values))
  attr(dfs, "values") <- values
  attr(dfs, "qualifiers") <- qualifiers
  attr(dfs, "format") <- attr(x, "format")
  attr(dfs, "summarised") <- TRUE
  attr(dfs, "tags") <- tags
  class(dfs) <- c("qtag.long", "qtag", class(dfs))
  return(dfs)
}

#' @rdname aggregate.qtag.long
#' @export
aggregate.qtag.wide <- function(x, ...) {
  if(attr(x, "summarised")) return(x)

  qualifiers <- attr(x, "qualifiers")
  funformats <- generate.call(...)
  if(length(funformats) > 1) {
    # need to return as a brick
    stop("Not implemented")
    argnames <- names(funformat)
  }
  values <- attr(x, "values")

  sumargs <- list()
  for(col in values) {
    sumargs[[col]] <- gsub(x=funformats, pattern="%s", replacement=col, fixed=TRUE)
  }
  dfs <- data.frame(do.call(dplyr::summarise_, c(list(group(x)), as.list(sumargs))))
  attr(dfs, "values") <- values
  attr(dfs, "qualifiers") <- qualifiers
  attr(dfs, "format") <- attr(x, "format")
  attr(dfs, "summarised") <- TRUE
  class(dfs) <- c("qtag.wide", "qtag", class(dfs))
  return(dfs)
}

#' Fortify a qualifier/tag structure for use in ggplot
#'
#' @param x A \link{qtag} object
#' @param skip_aggregate Pass \code{TRUE} to skip aggregation
#' @param ... Passed on to \code{aggregate} (or \link{long} if aggregate is \code{FALSE})
#'
#' @return A \code{qtag.long} object
#' @importFrom ggplot2 fortify
#' @export
#'
#' @examples
#' data(pocmaj)
#' library(ggplot2)
#' pocmaj <- qtag(pocmaj, qualifiers=c("core", "depth"))
#' fortify(pocmaj)
#' ggplot(pocmaj, aes(x=values, y=depth, col=core)) + geom_point() +
#'  scale_y_reverse() + facet_wrap(~column)
#'
fortify.qtag <- function(x, skip_aggregate=FALSE, ...) {
  if(skip_aggregate) {
    return(long(x, ...))
  } else {
    return(aggregate(long(x), ...))
  }
}

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
#' data(pocmaj)
#' autoplot(qtag(pocmaj, qualifiers=c("core", "depth")))
#'
autoplot.qtag.long <- function(x, xvar, yvar, facets, ...) {
  x <- aggregate(x, mean, err=sd(., na.rm = TRUE)/sum(!is.na(.)))
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


# private for now, generates the lazyeval-like behaviour of the aggregate functions
generate.call <- function(..., .quiet=FALSE) {
  sumargs <- sapply(substitute(list(...))[-1], deparse)
  if(length(sumargs) > 0) {
    argnames <- names(sumargs)
    if(is.null(argnames)) {
      argnames <- rep("", length(sumargs))
    }
    if(argnames[1] != "") stop("Need 1 unnamed argument to apply to values")
    argsformatted <- sapply(sumargs, function(aggfun) {
      if(grepl(x=aggfun, pattern="(", fixed=TRUE)) {
        gsub(x=aggfun, pattern="(.", replacement="(%s", fixed=TRUE)
      } else {
        paste0(aggfun, "(%s)")
      }
    })
    names(argsformatted) <- sapply(1:length(sumargs), function(i) {
      ifelse(i==1, "", ifelse(argnames[i]=="", sumargs[i], argnames[i]))
    })
    return(argsformatted)
  } else {
    if(!.quiet) message("No aggregation expressions, using function 'mean'")
    out <- "mean(%s)"
    names(out) <- ""
    return(out)
  }
}

.is_ad <- function(x) {
  r <- range(x)
  return((r[2] <= 2200) && (r[1] >= 1000))
}
