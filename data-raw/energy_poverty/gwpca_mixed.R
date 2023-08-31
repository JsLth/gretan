wpca <- function(x, wt, types, ...) {
  dots <- list(...)
  dots <- dots[names(dots) %in% names(formals(mixedCor))]
  local.center <- sweep(x, 2, colSums(sweep(x, 1, wt, '*')) / sum(wt))
  mid.x <- sweep(local.center, 1, sqrt(wt), '*')
  c <- types$continuous
  d <- types$dichotomous
  p <- types$polytomous
  mid.x[, colnames(mid.x) %in% c(d, p)] <- round(
    mid.x[, colnames(mid.x) %in% c(d, p)],
    digits = 0
  )
  mid.x <- apply(mid.x, 2, function(x) {
    if (any(x < 0)) {
      x <- x + abs(min(x))
    }
    x
  })
  args <- c(list(data = mid.x, c = c, d = d, p = p), dots)
  corr <- try(suppressMessages(suppressWarnings(do.call(mixedCor, args))), silent = TRUE)
  if (inherits(corr, "try-error")) 
    corr <- cor(mid.x)
  else
    corr <- corr$rho
  principal(corr)
}


robustSvd <- function(x) {
  pc <- princomp(covmat = covMcd(x, alpha = 3 / 4)$cov)
  list(v = pc$loadings, d = pc$sdev)
}


wt.median <- function(x,wt) {
  wt.median.1 <- function(x, wt)	{
    ox <- order(x)
    wox <- cumsum(wt[ox])
    posn <- which.min(abs(wox - 0.5))
    return(x[ox][posn])
  }
  apply(x, 2, wt.median.1, wt)
}


rwpca <- function(x, wt, type, nu = 0, nv = 2) {
  if (!missing(type)) stop("Robust GWPCA cannot handle mixed data.")
  mids <- sweep(x, 2, wt.median(x, wt))
  res <- robustSvd(sweep(mids, 1, wt, '*'))
  res$v <- res$v[, 1:nv]
  return(res)
}



gwpca <- function(data,
                  elocat,
                  vars,
                  k = 2,
                  robust = FALSE,
                  kernel = "bisquare",
                  adaptive = FALSE,
                  bw,
                  p = 2,
                  theta = 0,
                  longlat = F,
                  cv = T,
                  scores=F,
                  dMat,
                  ...) {
  ##Record the start time
  timings <- list()
  timings[["start"]] <- Sys.time()
  if (is(data, "Spatial")) {
    p4s <- proj4string(data)
    dp.locat <- coordinates(data)
    polygons <- NULL
    if (is(data, "SpatialPolygonsDataFrame"))
      polygons <- polygons(data)
  }
  else
    stop("Given data must be a Spatial*DataFrame or data.frame object")
  
  if (missing(elocat)) {
    ep.given <- FALSE
    elocat <- coordinates(data)
  }
  else {
    ep.given <- T
    if (is(elocat, "Spatial")) {
      espdf <- elocat
      elocat <- coordinates(espdf)
    } else if (is.numeric(elocat) && dim(elocat)[2] == 2) {
      elocat <- elocat
    } else {
      warning("Output loactions are not packed in a Spatial object, and it has to be a two-column numeric vector")
      elocat <- dp.locat
    }
  }
  data <- as(data, "data.frame")
  dp.n <- nrow(data)
  ep.n <- nrow(elocat)
  if (missing(dMat)) {
    DM.given <- F
    DM1.given <- F
    if (dp.n + ep.n <= 10000) {
      dMat <- gw.dist(
        dp.locat = dp.locat,
        rp.locat = elocat,
        p = p,
        theta = theta,
        longlat = longlat
      )
      DM.given <- T
    }
  }
  else {
    DM.given <- T
    DM1.given <- T 
    dim.dMat <- dim(dMat)
    if (dim.dMat[1] != dp.n || dim.dMat[2] != ep.n)
      stop("Dimensions of dMat are not correct")
  }
  
  if (missing(vars)) {
    stop("Variables input error")
  } else {
    var_types <- vars
    vars <- unlist(vars)
  }
    
  if (missing(bw) || bw <= 0)
    stop("Bandwidth is not specified incorrectly")
  len.var <- length(vars)

  col.nm <- colnames(data)
  var.idx <- match(vars, col.nm)[!is.na(match(vars, col.nm))]
  if (length(var.idx) == 0) stop("Variables input doesn't match with data")
  x <- data[,var.idx]
  x <- as.matrix(x)
  var.nms <- colnames(x)
  var.n <- ncol(x)
  if (len.var > var.n)
    warning("Invalid variables have been specified, please check them again!")

  pca.res <- princomp(x, cor = TRUE, scores = scores)

  w <- array(0, c(ep.n, var.n, k))
  
  if (scores) {
    gwpca.scores <- list()
  } else {
    gwpca.scores <- NULL
  }
  
  d <- matrix(0, ep.n, var.n)
  
  if (robust == FALSE) {
    pcafun = wpca
  } else {
    pcafun = rwpca
  }
    
  for (i in cli::cli_progress_along(1:ep.n)) {
    if (DM.given)
      dist.vi <- dMat[,i]
    else {
      if (ep.given) {
        dist.vi <- gw.dist(dp.locat, elocat, focus = i, p, theta, longlat) 
      } else {
        dist.vi <- gw.dist(dp.locat, focus = i, p = p, theta = theta, longlat = longlat) 
      }
    }
    wt <- gw.weight(dist.vi, bw,kernel, adaptive)
    use <- wt > 0
    wt <- wt[use]
    if (length(wt) <= 5) {
      expr <- paste("Too small bandwidth at location: ", i)
      warning(paste(expr, "and the results can't be given there.", sep = ", "))
      next
    }
    temp <- pcafun(x[use, ], wt, types = var_types, nu = 0, nv = k, ...)
    w[i, , ] <- temp$loadings[, k]
    d[i, ] <- sqrt(temp$values)
    
    if (scores) {
      scores.i <- c()
      for (j in 1:k) {
        score <- t(x[use, ]) * temp$v[, j]
        scores.i <- cbind(scores.i, apply(score, 2, sum))
      }
      gwpca.scores[[i]] <- scores.i
    }
  }
  
  if (!is.null(rownames(x))) dimnames(w)[[1]] <- rownames(x)
  if (!is.null(colnames(x))) dimnames(w)[[2]] <- colnames(x)
  dimnames(w)[[3]] <- paste("PC", 1:k, sep = '')
  CV <- numeric(dp.n)
  if (cv) {
    CV <- gwpca.cv.contrib(x, dp.locat, bw, k, robust,kernel,adaptive, p, theta, longlat, dMat)
  }
  GW.arguments <- list(
    vars = vars,
    k = k,
    bw = bw,
    kernel = kernel,
    adaptive = adaptive,
    p = p,
    theta = theta,
    longlat = longlat,
    dp.n = dp.n,
    DM.given = DM.given,
    scores = scores
  )
  
  if (robust == FALSE) {
    d1 <- (d / (sum(wt) ^ 0.5)) ^ 2
  } else {
    d1 <- d^2
  }
  
  local.PV <- matrix(d1[, 1:k]) / rowSums(d1) * 100
  var.names <- c()
  
  for (i in 1:k) {
    var.names <- c(var.names, paste(paste("Comp", i, sep = "."), "PV", sep = "_"))
  }
  
  win.var.pc1 <- max.col(abs(w[, , 1]))
  res.df <- data.frame(local.PV, rowSums(local.PV), vars[win.var.pc1])
  names(res.df) <- c(var.names, "local_CP", "win_var_PC1")
  
  if (!is.null(polygons)) {
    rownames(res.df) <- sapply(slot(polygons, "polygons"), function(i) slot(i, "ID"))
    SDF <- SpatialPolygonsDataFrame(Sr = polygons, data = res.df, match.ID = FALSE)
  } else {
    SDF <- SpatialPointsDataFrame(
      coords = elocat,
      data = res.df,
      proj4string = CRS(p4s),
      match.ID = FALSE
    )
  }
  
  timings[["stop"]] <- Sys.time()
  res <- list(
    pca = pca.res,
    loadings = w,
    SDF = SDF,
    gwpca.scores = gwpca.scores,
    var = d1,
    local.PV = local.PV,
    GW.arguments = GW.arguments,
    CV = CV,
    timings = timings
  )
  
  class(res) <- "gwpca"
  invisible(res) 
}

# Cross-validation function to optimally find a fixed or adaptive bandwidth... 
gwpca.cv <- function(bw,
                     x,
                     loc,
                     k = 2,
                     robust = FALSE,
                     kernel = "bisquare",
                     adaptive = FALSE,
                     p = 2,
                     theta = 0,
                     longlat = FALSE,
                     dMat) {
  if (missing(dMat))
    DM.given <- F
  else
    DM.given <- T
  n <- nrow(loc)
  m <- ncol(x)
  w <- array(0,c(n,m,k))

  score <- 0
  if (robust == FALSE)
    pcafun = wpca
  else
    pcafun = rwpca
  for (i in cli::cli_progress_along(1:n)) {
    if (DM.given)
      dist.vi <- dMat[, i]
    else
    {
      dist.vi <- gw.dist(loc, focus = i, p = p, theta = theta, longlat = longlat) 
    }
    wt <- gw.weight(dist.vi,bw,kernel,adaptive)
    wt[i] <- 0
    use <- wt > 0
    wt <- wt[use]
    if (length(wt) <= 1)
    {
      score <- Inf
      expr <- paste("Too small bandwidth: ", bw)
      warning(paste(expr, "and the CV value can't be given there.", sep = ", "))
      break
    }
    v <- pcafun(x[use,],wt,nu = 0,nv = k)$v
    v <- v %*% t(v)
    score <- score + sum((x[i, ] - x[i, ] %*% v)) ** 2
  }
  if (adaptive)
    cat("Adaptive bandwidth(number of nearest neighbours):", bw, "CV score:", score, "\n")
  else
    cat("Fixed bandwidth:", bw, "CV score:", score, "\n")
  score
}

# Contribution of each observation to the score statistic used in cross-validation for gwpca
# Outliers taken to correspond to high score (residual) values... 
gwpca.cv.contrib <- function(x,
                             loc,
                             var_types,
                             bw,
                             k = 2,
                             robust = FALSE,
                             kernel = "bisquare",
                             adaptive = FALSE,
                             p = 2,
                             theta = 0,
                             longlat = FALSE,
                             dMat)
{
  if (missing(dMat))
    DM.given <- F
  else
    DM.given <- T
  n <- nrow(loc)
  m <- ncol(x)
  w <- array(0,c(n, m, k))
  score <- numeric(n)
  if (robust == FALSE)
    pcafun = wpca
  else
    pcafun = rwpca
  for (i in 1:n) {
    if (DM.given)
      dist.vi <- dMat[,i]
    else {
      dist.vi <- gw.dist(loc, focus = i, p = p, theta = theta, longlat = longlat) 
    }
    wt <- gw.weight(dist.vi, bw,kernel, adaptive)
    wt[i] <- 0
    use <- wt > 0
    wt <- wt[use]
    if (length(wt) <= 1) {
      score[i] <- Inf
      expr <- paste("Too small bandwidth: ", bw)
      warning(paste(expr, "and the CV value can't be given there.", sep = ", "))
      break
    }
    v <- pcafun(x[use, ], wt, types = var_types, nu = 0, nv = k)$v
    v <- v %*% t(v)
    score[i] <- sum((x[i, ] - x[i, ] %*% v)) ** 2
  }
  score
}