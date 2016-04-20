

strat.multiplot <- function (d, d2=NULL, yvar = NULL, yvar2=NULL, graph.widths=1, minmax=NULL, scale.percent = FALSE, scale.minmax = TRUE,
    xLeft = 0.07, xRight = 1, yBottom = 0.07, yTop = 0.8, title = "", cex.title=1.8, y.axis=TRUE,
    min.width = 5, ylim = NULL, y.rev = FALSE, y.tks=NULL, ylabel = "", cex.ylabel=1, cex.yaxis=1,
    xSpace = 0.01, x.pc.inc=10, x.pc.lab=TRUE, x.pc.omit0=TRUE, wa.order = "none", plot.line = TRUE, 
    col.line = "black", lwd.line = 1, lty.line=1, col.line2="black", lwd.line2=1, lty.line2=2,
    plot.bar = FALSE, lwd.bar = 1, col.bar = "grey", sep.bar = FALSE, 
    plot.poly = FALSE, col.poly = "grey", col.poly.line = "black", lwd.poly = 1,
    plot.symb = TRUE, symb.pch=19, symb.cex=0.4, symb2.pch=18, symb2.cex=0.6, x.names=NULL,
    cex.xlabel = 1.1, srt.xlabel=90, mgp=NULL, cex.axis=.8, clust = NULL, clust.width=0.1,
    orig.fig=NULL, add=FALSE, error.matrix = NULL, error.matrix2=NULL, error.width=0.04, error.col=0, 
    error.lwd=0.6, ...)
{
  fcall <- match.call(expand.dots=TRUE)
  if (!is.null(clust)) {
    if (class(clust)[1]!="chclust") 
      stop("clust must be a chclust object")
  }
  if (!is.null(clust)) 
    xRight = xRight - clust.width
  if (is.null(yvar)) {
    yvar <- 1:nrow(d)
    if (is.null(ylim)) {
      if(is.null(d2)) {
        ylim=c(0.5, nrow(d)+0.5)
      } else {
        ylim=c(0.5, max(nrow(d), nrow(d2))+0.5)
      }
    }
  }
  
  if(is.null(yvar2)) {
    yvar2 <- yvar
  }
  
  if (is.null(x.names))
    x.names=colnames(d)   
  if (is.null(ylim)) {
    if(is.null(yvar2)) {
      ylim = c(min(yvar, na.rm=TRUE), max(yvar, na.rm=TRUE))
    } else {
      ylim <- c(min(yvar, yvar2, na.rm=TRUE), max(yvar, yvar2, na.rm=TRUE))
    }
  }
  oldfig = par("fig")
  oldmai <- par("mai")
  if (is.null(orig.fig)) {
    orig.fig = par("fig")
  }
  if(!is.null(d2)) {
    if(dim(d)[2]!=dim(d2)[2]) stop("Dimentions of d and d2 do not match")
    td2 <- d2
  } else {
    td2 <- d
  }
  if(!is.null(error.matrix)) {
    if(!all(dim(d)==dim(error.matrix))) stop("Dimentions of d and error.matrix do not match")
    terror.matrix <- error.matrix
  } else {
    terror.matrix <- matrix(rep(0, nrow(d)*ncol(d)), nrow=nrow(d), ncol=ncol(d))
  }
  if(!is.null(error.matrix2)) {
    if(!all(dim(d2)==dim(error.matrix2))) stop("Dimentions of d2 and error.matrix2 do not match")
    terror.matrix2 <- error.matrix2
  } else {
    terror.matrix2 <- matrix(rep(0, nrow(d)*ncol(d)), nrow=nrow(d), ncol=ncol(d))
  }

  
  nsp <- ncol(d)
  nsam <- nrow(d)
  
  if (scale.percent==TRUE & length(x.pc.inc) > 1) {
    if (length(x.pc.inc) != nsp)
      stop("length of x.pc.inc should equal number of curves")
  } else {
    x.pc.inc <- rep(x.pc.inc[1], nsp)
  }
  if (!is.null(minmax)) {
    if (ncol(minmax) != 2)
      stop("minmax should have 2 columns")
    if (nrow(minmax) != nsp)
      stop("number of rows of minmax should equal number of curves")
  }
  par(mai = c(0, 0, 0, 0))
  if (length(graph.widths) == 1)
    graph.widths <- rep(1, nsp)
  if (length(graph.widths) != nsp)
    stop("Length of graph.widths should equal number of curves")
  cc.line <- rep(col.line, length.out=nsp)
  cc.line2 <- rep(col.line2, length.out=nsp)
  if (sep.bar)
    cc.bar <- rep(col.bar, length.out=nsam)
  else      
    cc.bar <- rep(col.bar, length.out=nsp)
  cc.poly <- rep(col.poly, length.out=nsp)
  cc.poly.line <- rep(col.poly.line, length.out=nsp)
  inc <- 0.002
  if (wa.order == "topleft" || wa.order == "bottomleft") {
    colsum <- colSums(d)
    opt <- (t(d) %*% yvar)/colsum
    if ((wa.order == "topleft" & !y.rev) | (wa.order == "bottomleft" & y.rev))
      opt.order <- rev(order(opt))
    else opt.order <- order(opt)
    d <- d[, opt.order]
    if (!is.null(minmax)) 
      minmax <- minmax[opt.order, ]
    if (!is.null(x.names))
      x.names <- x.names[opt.order]
  }
  if (scale.percent) {
    colM <- apply(d, 2, max)
    colM <- floor((colM + 5)/5) * 5
    colM[colM < min.width] <- min.width
    colM.sum <- sum(colM)
  }
  else {
    colM.sum <- sum(graph.widths)
    colM <- graph.widths
  }
  xLen <- xRight - xLeft
  xInc <- xLen - ((nsp + 1) * xSpace)
  inc <- xInc/colM.sum
  if (inc < 0.0)
    stop("Too many variables, curves will be too small. Reduce xSpace.")
  x1 <- xLeft
  #    par(fig = c(x1, x1+0.4, yStart, yTop))
  par(fig = figCnvt(orig.fig, c(x1, min(x1+0.4, .9), yBottom, yTop)), new=add)
  if (y.rev) {
    tmp <- ylim[1]
    ylim[1] <- ylim[2]
    ylim[2] <- tmp
  }
  plot(0, 0, cex = 0.5, xlim = c(0, 1), axes = FALSE, type = "n", yaxs = "r", ylim = ylim, ...)
  usr1 <- par("usr")
  if (y.axis) {
    if (is.null(y.tks))
      y.tks <- axTicks(2)
    ax <- axis(side = 2, las = 1, at = y.tks, labels = as.character(y.tks), cex.axis=cex.yaxis, xpd=NA)
    x1 <- x1 + xSpace
    mtext(title, adj = 0, line = 5, cex = cex.title)
    mtext(ylabel, side = 2, line = 2.5, cex=cex.ylabel)
  }
  ty <- ifelse(plot.line, "l", "n")
  tcll <- -.3
  if ("tcl" %in% names(fcall))
    tcll <- eval(fcall$tcl)
  spc <- 0
  if ("las" %in% names(fcall)) {
    if ((eval(fcall$las)) == 2)
      spc = 0.3
  }
  figs <- vector("list", length=nsp)
  usrs <- vector("list", length=nsp)
  for (i in 1:nsp) {
    par(new = TRUE)
    par(lend = "butt")

    inc2 <- inc * colM[i]
    par(fig = figCnvt(orig.fig, c(x1, min(1, x1 + inc2), yBottom, yTop)))
    if (!is.null(minmax)) {
      plot(d[, i], yvar, cex = 0.5, axes = FALSE, xaxs = "i", 
           type = "n", yaxs = "r", ylim = ylim, xlim=c(minmax[i, 1], minmax[i,2]), ...)
    } else { #adjust minmax for error bars/d2
      minval <- min(d[,i], d[,i]-terror.matrix[,i], td2[,i], td2[,i]-terror.matrix2[,i], na.rm=TRUE)
      maxval <- max(d[,i], d[,i]+terror.matrix[,i], td2[,i], td2[,i]+terror.matrix2[,i], na.rm=TRUE)
      plot(d[, i], yvar, cex = 0.5, axes = FALSE, xaxs = "i", 
           type = "n", yaxs = "r", ylim = ylim, xlim=c(minval, maxval), ...)
      
    } 
    
    tks <- axTicks(1)
    us <- par("usr")
    
    if (plot.poly) {
      y <- c(min(yvar, na.rm=TRUE), yvar, max(yvar, na.rm=TRUE))
      x <- c(us[1], d[, i], us[1])
      polygon(x, y, col = cc.poly[i], border = cc.poly.line[i], lwd=lwd.poly)
    }
    if (plot.bar) {
      if (sep.bar) {
        segments(rep(us[1], nsam), yvar, d[, i], yvar, lwd = lwd.bar, col = cc.bar)
        
      } else {
        segments(rep(us[1], nsam), yvar, d[, i], yvar, lwd = lwd.bar, col = cc.bar[i])               }
    }
    if(y.rev) {
      topy <- min(yvar, na.rm=TRUE)
      bty <- max(yvar, na.rm=TRUE)
    } else {
      topy <- max(yvar, na.rm=TRUE)
      bty <- min(yvar, na.rm=TRUE)
    }
    lines(c(us[1], us[1]), c(bty, topy), lwd=0.5, xpd=NA, ...)
    if(!plot.bar) {
      lines(c(us[1], us[2]), c(topy, topy), lwd=0.5, xpd=NA, ...)
    }
    
    #ERROR (changed order here so that error bars are on top of segments, below points)
    if (!is.null(error.matrix)) {
      errors <- error.matrix[,i]
      errors[errors==0] <- NA
      arrows(d[,i]+errors, yvar, d[,i]-errors, yvar, angle=90, 
             length=error.width, code=3, lwd=error.lwd)
    }
    
    if (plot.symb) {
      points(d[, i], yvar, pch=symb.pch, cex=symb.cex, xpd=NA)
    }
    
    if (ty == "l") {
      lines(d[, i], yvar, col = cc.line[i], lwd = lwd.line, lty=lty.line)
    }
    
    #NOW PLOT FOR D2
    if(!is.null(d2)) {
      if (plot.poly) {
        y <- c(min(yvar2, na.rm=TRUE), yvar2, max(yvar2, na.rm=TRUE))
        x <- c(us[1], d2[, i], us[1])
        polygon(x, y, col = cc.poly[i], border = cc.poly.line[i], lwd=lwd.poly)
      }
      if (plot.bar) {
        if (sep.bar) {
          segments(rep(us[1], nsam), yvar2, d2[, i], yvar, lwd = lwd.bar, col = cc.bar)
          
        } else {
          segments(rep(us[1], nsam), yvar2, d2[, i], yvar, lwd = lwd.bar, col = cc.bar[i])               }
      }
      
      if(!is.null(error.matrix2)) {
        errors <- error.matrix2[,i]
        errors[errors==0] <- NA
        arrows(d2[,i]+errors, yvar2, d2[,i]-errors, yvar2, angle=90, 
               length=error.width, code=3, lwd=error.lwd)
      }
      if (plot.symb) {
        points(d2[, i], yvar2, pch=symb2.pch, cex=symb2.cex, xpd=NA)
      }
      
      if (ty == "l") {
        lines(d2[, i], yvar2, col = cc.line2[i], lwd = lwd.line2, lty=lty.line2)
      }
    }
      
    mgpX <- if (is.null(mgp)) { c(3, max(0.0, spc-tcll),0) } else { mgp }
    if (scale.minmax) {
      nn <- length(axTicks(1))
      tk <- c(axTicks(1)[1], axTicks(1)[nn])
      axis(side = 1, at = tk, labels = as.character(tk), cex.axis=cex.axis, mgp=mgpX, ...)
    } else {
      axis(side = 1, cex.axis=cex.axis, mgp=mgpX, ...)
    }
    x1 <- x1 + inc2 + xSpace
    
    #        tks1 <- axTicks(1)
    usr2 <- par("usr")
    tks1 <- usr2[1]
    
    r <- (usr1[4] - usr1[3]) * 0.01
    pos <- usr1[4]+r
    if (y.rev)
      pos <- usr1[4]-r
    if (srt.xlabel < 90)
      text(tks1[1], pos, labels=x.names[i], adj = c(0, 0), srt=srt.xlabel, cex = cex.xlabel, xpd=NA)
    else
      text(tks1[1], pos, labels=x.names[i], adj = c(0, 1), srt=srt.xlabel, cex = cex.xlabel, xpd=NA)
    
    usrs[[i]] <- usr2   
    figs[[i]] <- par("fig")
  }
  if (!is.null(clust)) {
    par(fig = figCnvt(orig.fig, c(x1, xRight+clust.width, yBottom, yTop)))
    par(mar=c(0,0,0,0))
    par(new = TRUE)
    #        plot(clust, horiz = TRUE, xaxt.rev=yaxt.rev, leaflab = "none", cex.axis = 0.5, yaxt.rev=TRUE)
    #        if(y.rev)
    #           clust <- rev(clust)
    mgpX <- if (is.null(mgp)) { c(2, .5, 0) } else { mgp }
    plot(clust, xvar=yvar, horiz=TRUE, x.rev=y.rev, labels=rep("", length(yvar)), hang=-1, mgp=mgpX, cex.axis=cex.axis, ...)
  }
  par(mai = oldmai)
  oldfig[oldfig < 0] <- 0
  par(fig = oldfig)
  ll <- list(call=fcall, box=c(xLeft=xLeft, xRight=xRight, yBottom=yBottom, yTop=yTop), usr = usr1, yvar=yvar, ylim=ylim, y.rev=y.rev, figs=figs, usrs=usrs)
  invisible(ll)
}