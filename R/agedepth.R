#' Create Age Depth Function
#'
#' Creates a function that transforms depths into ages based on a set of known depths and known ages.
#' Interpolation is linear, and extrpolation is based on average sedimentation rates. Error
#' is not interpolated but is included in the final output if information for that depth is
#' requested (useful for putting error bars on age-depth model plots). If \code{depths} are
#' provided, this funcion returns a \code{data.frame} of the interpolated/extrapolated values.
#'
#' @param knowndepths A vector of depths with known ages
#' @param knownages A vector of ages of the depths specified
#' @param age_err A vector of error values for the ages specified
#' @param extrapolate An extrapolation method for depths beyond the maximum specified depth:
#'                    one of 'average' or 'last'.
#' @param depths If specified, will return a \code{data.frame} of the depth, age, error, and type
#'               of each depth specified.
#' @param ... If \code{depths} is specified, arguments to pass to the age depth function
#'            (currently \code{cols=c("depth", "age", "err", "type")}) is supported.
#'
#' @return A function with the signature \code{function(depth, cols=c("depth", "age", "err", "type"))}
#'         that transforms a depth into a \code{data.frame} with the given \code{cols}.
#' @export
#'
#' @examples
#' # make an age depth function and use it
#' ad_func <- agedepth(c(17, 10, 6, 0), c(1880, 1940, 1970, 2015.9))
#' ad_func(4)
#' ad_func(6)
#' ad_func(c(0, 1, 2, 3, 4, 5, 6))
#'
#' # specify depths to return a data.frame instead of a function
#' agedepth(c(17, 10, 6, 0), c(1880, 1940, 1970, 2015.9), depths=c(0, 1, 2, 3, 4, 5, 6))
#'
#' # make an age-depth plot using ggplot
#' library(ggplot2)
#' ages <- agedepth(c(17, 10, 6, 0), c(1880, 1940, 1970, 2015.9),
#'                  depths=c(0, 6, 10, 17:22))
#' ggplot(ages, aes(x=age, y=depth, lty=type)) + geom_line() + scale_y_reverse()
#'
agedepth <- function(knowndepths, knownages, age_err=NA, extrapolate="average", depths=NULL, ...) {
  df <- data.frame(depth=knowndepths, age=knownages, err=age_err)
  df <- df[order(df$depth),]
  if(extrapolate=="average") {
    fit <- lm(age~depth, data=df)
    mhigh <- mlow <- coef(fit)[2]
  } else if(extrapolate=="last") {
    pts <- df[(nrow(df)-1:nrow(df)),]
    mhigh <- (pts$age[2] - pts$age[1]) / (pts$depth[2] - pts$depth[1])
    pts <- df[1:2,]
    mlow <- (pts$age[2] - pts$age[1]) / (pts$depth[2] - pts$depth[1])
  } else {
    stop("invalid extrapolation method: ", extrapolate)
  }

  adfun <- function(depth, cols=c("depth", "age", "err", "type")) {
    foreach <- foreach::foreach
    `%do%` <- foreach::`%do%`
    d <- NULL; rm(d) # CMD trick
    finaldf <- foreach(d=depth, .combine=rbind) %do% {
      if(d %in% df$depth) {
        age <- df$age[df$depth==d]
        err <- df$err[df$depth==d]
        type <- "measured"
      } else if(d < min(df$depth)) {
        #extrapolate using mlow
        age <- df$age[1] + mlow * (d - df$depth[1])
        err <- NA
        type <- "extrapolated"
      } else if(d > max(df$depth)) {
        #extrapolate using last point and mhigh
        age <- df$age[nrow(df)] + mhigh * (d - df$depth[nrow(df)])
        err <- NA
        type <- "extrapolated"
      } else {
        age <- approx(df$depth, df$age, xout=d)$y
        err <- NA
        type <- "interpolated"
      }
      data.frame(depth=d, age=age, err=err, type=type)
    }
    rownames(finaldf) <- NULL
    if(length(cols)==1) {
      return(finaldf[[cols]])
    } else {
      return(finaldf[cols])
    }
  }

  if(is.null(depths)) {
    return(adfun)
  } else {
    return(adfun(depths, ...))
  }

}
