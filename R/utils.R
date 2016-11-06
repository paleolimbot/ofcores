

#' Melt multiple sets of columns in parallel
#'
#' Essentially this is a wrapper around \code{reshape2::melt.data.frame} that
#' is able to \code{cbind} several melt operations.
#'
#' @param x A data.frame
#' @param id.vars vector of ID variable names
#' @param variable.name Column name to use to store variables
#' @param ... Named arguments specifying the \code{measure.vars} to be stored to the
#'   column name specified.
#' @param factorsAsStrings Control whether factors are converted to character when melted as
#'   measure variables.
#'
#' @return A melted \code{data.frame}
#' @export
#'
#' @examples
#' data(pocmajpb210)
#' melt.parallel(pb210,
#'               id.vars=c("core", "depth"),
#'               values=c("Pb210", "age", "sar"),
#'               err=c("Pb210_sd", "age_sd", "sar_err"))
#'
melt.parallel <- function(x, id.vars, variable.name="variable", ..., factorsAsStrings=TRUE) {
  combos <- list(...)
  combonames <- names(combos)
  if(length(combos)==0) stop("Need at least one set of columns to melt")
  if(length(combonames) != length(combos)) stop("All arguments must be named")
  lengths <- unique(sapply(combos, length))
  if(length(lengths) > 1) stop("All melted columns must have the same number of source columns")
  melted <- lapply(combonames, function(varname) {
    reshape2::melt(x, id.vars=id.vars, measure.vars=combos[[varname]], value.name=varname,
                   factorsAsStrings=factorsAsStrings)
  })
  iddata <- melted[[1]][c(id.vars, variable.name)]
  melted <- lapply(melted, function(df) df[names(df) %in% names(combos)])
  do.call(cbind, c(list(iddata), melted))
}
