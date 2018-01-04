#' Support functions not available to user, just other functions
#' 

trace <- function(x) mean(diag(x))
reduce <- function(x) which(x %% 1:x == 0)
prime <- function(x) !any(x %% 2:(x-1) == 0)


rm.NA.cols <- function(X) length(na.omit(X)) ## ## Function that removes columns of all NA from individual environments (not all traits are evaluated across all envs)

is.max <- function(list.in){
  return(which(list.in == max(list.in, na.rm = T)))
}

is.min <- function(list.in){
  return(which(list.in == min(list.in, na.rm = T)))
}

round.up <- function(x,to=10){
  to*(x%/%to + as.logical(x%%to))
}

### finds rho for row and column
find.rhos <- function(rhos, fr.in){
  for(i in 1:length(fr.in)){x <- fr.in[i]; assign(names(x), x[[1]])}
  print(rhos)

  Z.fr[["AR1"]] <- model.matrix(~ -1+ row:column, data = data.fr)
  
  K.fr[["AR1"]] <- rhos[1]^colDist.fr %x% rhos[2]^rowDist.fr

  return(-emmremlMultiKernel(y=data.fr[,trait.fr], X=X.fr, Zlist=Z.fr, Klist=K.fr)$loglik)
}


optim.2 <- function(fr.in.2=fr.list){ ## Have to do this becasue EMMREML uses optim... was leading to errors
  fr.out <- optim(par = c(.5,.5), fn = find.rhos, control = list(reltol = 1e-6), fr.in=fr.in.2)
  return(fr.out)
}


## Merge multiple dfs stored in a list
merge.all <- function(df.list, by.m=NULL){
  i<-1
  while(i <= length(df.list)){
    if(i == 1) merged.df <- get(df.list[i]) else merged.df <- merge(merged.df, get(df.list[i]), by=by.m, all.x=T)
    i <- i+1
  }
  return(merged.df)
}


## Harmonic means
harm.mean <- function(x){ ## Calculates harmonic mean of a vector
  if(0 %in% x) x <- subset(x, x > 0)
  harmean <- 1/mean(1/x)
  return(harmean)
}


### lmm.fun
#lmm.list.in <- blup.list
lmm.fun <- function(lmm.list.in){
  for(i in 1:length(lmm.list.in)){x <- lmm.list.in[i]; assign(names(x), x[[1]])}
  
  if(is.null(Vformula.lmm)) new.Vformula.lmm <- ~ lmm.R else new.Vformula.lmm <- as.formula(paste("~", paste(c(attr(terms.formula(Vformula.lmm), "term.labels"), "lmm.R"), collapse = " + "), sep = " "))
  
  return(regress(formula.lmm, new.Vformula.lmm, pos = rep(T, 100), identity=F, data = data.lmm, verbose = as.numeric(lmm.verbose)))
}

### Forms R matrix from list of multiple trial-specific matrices
make.R <-function(mr.list.in=NULL){
  for(i in 1:length(mr.list.in)){x <- mr.list.in[i]; assign(names(x), x[[1]])}
  final.R <- matrix(0, nrow.mr, nrow.mr)

  for(r in 1:length(trial.mats)){
    final.R[dims.mr[[r]], dims.mr[[r]]] <- trial.mats[[r]]
  }
  return(final.R)
}

### Moving average function -- utilizes Technow's mvngGrAd() package
#mv.in = mvavg.input
mvng.avg <- function(mv.in=NULL){  
  ## Unpack input
  for(i in 1:length(mv.in)){x <- mv.in[i]; assign(names(x), x[[1]])}
  
  if(!exists("toMaximize")){
    toMaximize <- function(combs){
      mvAvg.results <- movingGrid(rows, cols, obsPhe = as.numeric(data.mv[,trait.mv]), shapeCross=list(1:combs[1], 1:combs[1], 1:combs[2], 1:combs[2]), layers=c(1:combs[3]), excludeCenter=T)
      if(any(is.na(mvAvg.results@movingMean))) -Inf else mvAvg.results@correlation
    }
    toMaximize.no_l <- function(combs){
      mvAvg.results <- movingGrid(rows, cols, obsPhe = as.numeric(data.mv[,trait.mv]), shapeCross=list(1:combs[1], 1:combs[1], 1:combs[2], 1:combs[2]), layers=NULL, excludeCenter=T)
      if(any(is.na(mvAvg.results@movingMean))) -Inf else mvAvg.results@correlation
    }
    toMaximize.no_lr <- function(combs){
      mvAvg.results <- movingGrid(rows, cols, obsPhe = as.numeric(data.mv[,trait.mv]), shapeCross=list(NULL, NULL, 1:combs, 1:combs), layers=NULL, excludeCenter=T)
      if(any(is.na(mvAvg.results@movingMean))) -Inf else mvAvg.results@correlation
    }
    toMaximize.no_lc <- function(combs){
      mvAvg.results <- movingGrid(rows, cols, obsPhe = as.numeric(data.mv[,trait.mv]), shapeCross=list(1:combs, 1:combs, NULL, NULL), layers=NULL, excludeCenter=T)
      if(any(is.na(mvAvg.results@movingMean))) -Inf else mvAvg.results@correlation
    } 
  }
  
  if(is.null(grid.mv)){ ## If a grid size isn't provided, mvng.avg will find the 'optimal'
    
    if(is.null(max.dims)){
      nth_sth <- 1:(fld.dims[1]-1) ## Doing -1 makes it possible for even plots in the corners to have the whole row/colun included (i.e. the whole field for corner plots)
      east_west <- 1:(fld.dims[2]-1)
    } else{
      nth_sth <- 1:max.dims[1] ## l is limited by the # of rows and columns
      east_west <- 1:max.dims[2]
    }
    
    cat("\nOptimizing moving grid size for", trait.mv, "in", trial.mv, "\n")
    combs.no_l <- cbind(rep(nth_sth, each= length(east_west)), rep(east_west, times= length(nth_sth)))
    combs <- cbind(combs.no_l, apply(combs.no_l, 1, min))
    
    max.list <- list()
    cat("\nStep 1 of 4\n")
    max.list[[1]] <- list("cor.list"=cor.list <- pbapply(combs, 1, toMaximize), "max"=max(cor.list, na.rm = T), "max.pos"=is.max(cor.list))
    cat("\nStep 2 of 4\n")
    max.list[[2]] <- list("cor.list"=cor.list <- pbapply(combs.no_l, 1, toMaximize.no_l), "max"=max(cor.list, na.rm = T), "max.pos"=is.max(cor.list))
    cat("\nStep 3 of 4\n")
    max.list[[3]] <- list("cor.list"=cor.list <- pbapply(as.matrix(east_west), 1, toMaximize.no_lr), "max"=max(cor.list, na.rm = T), "max.pos"=is.max(cor.list))
    cat("\nStep 4 of 4\n")
    max.list[[4]] <- list("cor.list"=cor.list <- pbapply(as.matrix(nth_sth), 1, toMaximize.no_lc), "max"=max(cor.list, na.rm = T), "max.pos"=is.max(cor.list))
    
    chosen.pos <- is.max(unlist(lapply(max.list, function(x){return(x$max)}))); chosen.pos <- chosen.pos[length(chosen.pos)]
    chosen <- max.list[[chosen.pos]]
    
    if(chosen.pos == 1) {
      r <- combs[chosen$max.pos, 1]; c <- combs[chosen$max.pos, 2]; l <- combs[chosen$max.pos, 3]
      mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, 1:c, 1:c), layers=c(1:l), excludeCenter=T)
    }
    if(chosen.pos == 2) {
      r <- combs.no_l[chosen$max.pos, 1]; c <- combs.no_l[chosen$max.pos, 2]; l <- 0
      mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, 1:c, 1:c), layers=NULL, excludeCenter=T)
    }
    if(chosen.pos == 3) {
      r <- 0; c <- east_west[chosen$max.pos]; l <- 0
      mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(NULL, NULL, 1:c, 1:c), layers=NULL, excludeCenter=T)  
    }
    if(chosen.pos == 4) {
      r <- nth_sth[chosen$max.pos]; c <- 0; l <- 0
      mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, NULL, NULL), layers=NULL, excludeCenter=T)  
    }
    
  } else{ ### If user chooses to define grid size then it jumps to here
    
    if(length(grid.mv) != 3) stop("A numeric vector of length 3 (row, column, diagonal) is required for each trial in grid.sizes")
    r <- grid.mv[1]
    c <- grid.mv[2]
    l <- grid.mv[3]
    if(!any(c(r,c,l) == 0)) mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, 1:c, 1:c), layers=c(1:l), excludeCenter=T)
    if(r == 0) mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(NULL, NULL, 1:c, 1:c), layers=c(1:l), excludeCenter=T)
    if(c == 0) mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, NULL, NULL), layers=c(1:l), excludeCenter=T)
    if(l == 0) mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, 1:c, 1:c), layers=NULL, excludeCenter=T)
    
    if(any(is.na(mv.correction@movingMean))) stop("The defined mvavg grid is resulting in NA covariates. Change one or more dimension.")
  }
    
  mv.means <- mv.correction@movingMean
  return(list(mvavg.adj=mvngGrAd::fitted(mv.correction), mvavg.covs= (mv.means - mean(mv.means, na.rm = T)), mvavg.grid.sz=c(r,c,l)))
} ## End of moving average


tt.update <- function(orig, response=T, obj, pos="end"){ ## pos can be "beginning" or "end"
  y <- NULL; if(response) y <- orig[[2]]
  if(pos == "beginning") return(as.formula(paste(y, "~", paste(c(obj, attr(terms.formula(orig), "term.labels")), collapse = " + "), sep = " ")))
  if(pos == "end") return(as.formula(paste(y, "~", paste(c(attr(terms.formula(orig), "term.labels"), obj), collapse = " + "), sep = " ")))
}

FldTrial.lm <- function(y, X){
  n <- nrow(X)
  k <- ncol(X)
  
  suppressWarnings(inv.tX.X <- chol2inv(chol(t(X) %*% X)))
  if(!exists("inv.tX.X")) inv.tX.X <- MASS::ginv(t(X) %*% X) ## If chol fails then use general inverse of MASS package
  
  beta <- matrix(inv.tX.X %*% t(X) %*% y, ncol = 1, dimnames = list(colnames(X), NULL))
  
  yhat <- X %*% beta
  e <- as.vector(y - yhat)
  
  sigma.samp <- (t(e) %*% e) / (n-k)
  varB <- sigma.samp %x% inv.tX.X
  se.B <- sqrt(sigma.samp) * sqrt(diag(inv.tX.X))
  rownames(varB) <- colnames(varB) <- rownames(beta)
  
  return(list(betahat=beta, resids=e, varbetahat= diag(varB), stderrbetahat=se.B, dfErr=(n-k)))
}


### Based on 'predictmeans' pacakge
calc.ASED <- function(K, vcov.m){
  nK <- nrow(K)
  
  kindx <- 1:nK
  CM <- matrix(0, nrow = nK * (nK - 1)/2, ncol = nK)
  t <- 1
  for (i in 2:nK) {
    for (j in 1:(i - 1)) {
      CM[t, ] <- (kindx == j) - (kindx == i)
      #varn1[t] <- rnK[i]
      #varn2[t] <- rnK[j]
      t <- t + 1
    }
  }
  rK <- CM %*% K
  
  dses <- as.numeric(apply(rK, 1, function(x) {
    y <- matrix(x, nrow = 1)
    sqrt(y %*% tcrossprod(vcov.m, y))
  }))
  
  return(mean(dses))  
}


#### design.aibd design functions
## Scenario 2.1 function 
scenario2.1 <- function(perChk.start, return.params){
  perChk <- NA
  minPlots <- ceiling((1+perChk.start) * nEntries)
  min.nFieldCols <- ceiling(minPlots / nFieldRows)
  
  while(prime(min.nFieldCols)) min.nFieldCols <- min.nFieldCols + 1
  
  cand.nBlkCols <- reduce(min.nFieldCols) ## leave in this order to test the largest blocks possible first
  cand.nColsPerBlk <- min.nFieldCols / cand.nBlkCols
  cands2remove <- unique(c(which(cand.nColsPerBlk < minColBlkDim | cand.nColsPerBlk > maxColBlkDim | cand.nColsPerBlk == min.nFieldCols), which(cand.nBlkCols*nBlkRows < nBlks.min)))
  if(length(cands2remove) > 0){
    cand.nBlkCols <- cand.nBlkCols[-cands2remove]
    cand.nColsPerBlk <- cand.nColsPerBlk[-cands2remove]
    cand.nBlks <- cand.nBlkCols * nBlkRows      
  }
  
  if(length(cand.nBlkCols) == 0){
    return(1)
  } else{
    for(c in 1:length(cand.nBlks)){
      nTotal.tmp <- nEntries + cand.nBlks[c] + nChk2*nChk2.min
      exp.size.tmp <- min.nFieldCols*nFieldRows
      nFill.tmp <- exp.size.tmp - nTotal.tmp
      
      if(nFill.tmp >= 0){perChk.tmp <- (exp.size.tmp - nEntries) / exp.size.tmp} else{perChk.tmp <- 1}
      
      if(perChk.tmp >= minPerChks & perChk.tmp <= maxPerChks){
        perChk <- perChk.tmp
        break
      }
    }
  }
  if(!return.params & !is.na(perChk)) return(perChk)
  if(!return.params & is.na(perChk)) return(1)
  if(return.params) return(list(perChk=perChk, nFill=nFill.tmp, nFieldCols=min.nFieldCols, nBlkCols=cand.nBlkCols[c], nBlks=cand.nBlks[c])) 
}

## Scenario 2.2 function -- finds nFieldRows and nRowsPerBlk
scenario2.2 <- function(perChk.start, return.params){
  perChk <- NA
  minPlots <- ceiling((1+perChk.start) * nEntries)
  min.nFieldRows <- ceiling(minPlots / nFieldCols)
  
  while(prime(min.nFieldRows)) min.nFieldRows <- min.nFieldRows + 1
  
  cand.nBlkRows <- reduce(min.nFieldRows) ## leave in this order to test the largest blocks possible first
  cand.nRowsPerBlk <- min.nFieldRows / cand.nBlkRows
  cands2remove <- unique(c(which(cand.nRowsPerBlk < minRowBlkDim | cand.nRowsPerBlk > maxRowBlkDim | cand.nRowsPerBlk == min.nFieldRows), which(cand.nBlkRows*nBlkCols < nBlks.min)))
  if(length(cands2remove) > 0){
    cand.nBlkRows <- cand.nBlkRows[-cands2remove]
    cand.nRowsPerBlk <- cand.nRowsPerBlk[-cands2remove]
    cand.nBlks <- cand.nBlkRows * nBlkCols    
  }
  
  if(length(cand.nBlkRows) == 0) return(1) else{
    for(c in 1:length(cand.nBlks)){
      nTotal.tmp <- nEntries + cand.nBlks[c] + nChk2*nChk2.min
      exp.size.tmp <- min.nFieldRows*nFieldCols
      nFill.tmp <- exp.size.tmp - nTotal.tmp
      
      perChk.tmp <-  if(nFill.tmp >= 0) (exp.size.tmp - nEntries) / exp.size.tmp else perChk.tmp <- 1
      
      if(perChk.tmp >= minPerChks & perChk.tmp <= maxPerChks){
        perChk <- perChk.tmp
        break
      }
    }
  }
  
  if(!return.params & !is.na(perChk)) return(perChk)
  if(!return.params & is.na(perChk)) return(1)
  if(return.params) return(list(perChk=perChk, nFill=nFill.tmp, nFieldRows=min.nFieldRows, nBlkRows=cand.nBlkRows[c], nBlks=cand.nBlks[c]))
}

## Scenario 3 Function - User does not provide block dimensions
scenario3 <- function(perChk.start, return.params){
  perChk <- NA
  
  ## Set up candidate row dimensions
  cand.nBlkRows <- reduce(nFieldRows)
  cand.nRowsPerBlk <- nFieldRows / cand.nBlkRows
  cand.rows2remove <- which(cand.nRowsPerBlk < minRowBlkDim | cand.nRowsPerBlk > maxRowBlkDim)
  if(length(cand.rows2remove > 0)){
    cand.nBlkRows <- cand.nBlkRows[-cand.rows2remove]
    cand.nRowsPerBlk <- cand.nRowsPerBlk[-cand.rows2remove]      
  }
  
  ## Set up candidate column dimensions
  cand.nBlkCols <- reduce(nFieldCols) ## leave in this order to test the largest blocks possible first
  cand.nColsPerBlk <- nFieldCols / cand.nBlkCols
  cand.cols2remove <- which(cand.nColsPerBlk < minColBlkDim | cand.nColsPerBlk > maxColBlkDim | cand.nColsPerBlk == nFieldCols)
  if(length(cand.cols2remove) > 0){
    cand.nBlkCols <- cand.nBlkCols[-cand.cols2remove]
    cand.nColsPerBlk <- cand.nColsPerBlk[-cand.cols2remove]
  }
  
  ## List of candidate block sizes
  cand.nBlks.mat <- rbind(rep(cand.nBlkRows, times=length(cand.nBlkCols)), rep(cand.nBlkCols, each=length(cand.nBlkRows)))
  cand.nBlks <- apply(cand.nBlks.mat, 2, function(X){return(X[1]*X[2])})
  cand.blks2remove <- which(cand.nBlks < nBlks.min)
  if(length(cand.blks2remove) > 0){
    cand.nBlks.mat <- as.matrix(cand.nBlks.mat[,-cand.blks2remove])
    cand.nBlks <- cand.nBlks[-cand.blks2remove] 
  }
  
  if(length(cand.nBlks) == 0){
    return(1)
  } else{
    for(c in 1:length(cand.nBlks)){
      nTotal.tmp <- nEntries + cand.nBlks[c] + nChk2*nChk2.min
      exp.size.tmp <- nFieldCols*nFieldRows
      nFill.tmp <- exp.size.tmp - nTotal.tmp
      
      if(nFill.tmp >= 0){perChk.tmp <- (exp.size.tmp - nEntries) / exp.size.tmp} else{perChk.tmp <- 1}
      
      if(minPerChks <= perChk.tmp & perChk.tmp <= maxPerChks){
        perChk <- perChk.tmp
        break
      }
    }
  }
  if(!return.params & !is.na(perChk)) return(perChk)
  if(!return.params & is.na(perChk)) return(1)
  if(return.params) return(list(perChk=perChk, nFill=nFill.tmp, nBlkRows=cand.nBlks.mat[1,c], nBlkCols=cand.nBlks.mat[2,c], nBlks=cand.nBlks[c])) 
}


### Scenario 4.1 function -- user provides only nFieldRows
scenario4.1 <- function(perChk.start, return.params){
  perChk <- NA
  minPlots <- ceiling((1+perChk.start) * nEntries)
  min.nFieldCols <- ceiling(minPlots / nFieldRows)
  
  while(prime(min.nFieldCols)) min.nFieldCols <- min.nFieldCols + 1
  
  ## Set up candidate row dimensions
  cand.nBlkRows <- reduce(nFieldRows)
  cand.nRowsPerBlk <- nFieldRows / cand.nBlkRows
  cands2remove <- (cand.nRowsPerBlk < minRowBlkDim | cand.nRowsPerBlk > maxRowBlkDim)
  cand.nBlkRows <- cand.nBlkRows[!cands2remove]
  cand.nRowsPerBlk <- cand.nRowsPerBlk[!cands2remove]  
  
  ## Set up candidate column dimensions
  cand.nBlkCols <- reduce(min.nFieldCols) ## leave in this order to test the largest blocks possible first
  cand.nColsPerBlk <- min.nFieldCols / cand.nBlkCols
  cands2remove <- (cand.nColsPerBlk < minColBlkDim | cand.nColsPerBlk > maxColBlkDim | cand.nColsPerBlk == min.nFieldCols)
  cand.nBlkCols <- cand.nBlkCols[!cands2remove]
  cand.nColsPerBlk <- cand.nColsPerBlk[!cands2remove]
  
  ## List of candidate block sizes
  cand.nBlks.mat <- rbind(rep(cand.nBlkRows, times=length(cand.nBlkCols)), rep(cand.nBlkCols, each=length(cand.nBlkRows)))
  cand.nBlks <- apply(cand.nBlks.mat, 2, function(X){return(X[1]*X[2])})
  cands2remove <- cand.nBlks < nBlks.min
  cand.nBlks.mat <- as.matrix(cand.nBlks.mat[,!cands2remove])
  cand.nBlks <- cand.nBlks[!cands2remove]  
  
  if(length(cand.nBlks) == 0) return(1) else{
    for(c in 1:length(cand.nBlks)){
      nTotal.tmp <- nEntries + cand.nBlks[c] + nChk2*nChk2.min
      exp.size.tmp <- min.nFieldCols*nFieldRows
      nFill.tmp <- exp.size.tmp - nTotal.tmp ## This whole loop is to calculate the nFill.tmp parameter to make sure that the field is big enough to accomodate all entries and required checks (as defined), plus enough "extra" to satisfy minPerChk requirement
      
      perChk.tmp <- if(nFill.tmp >= 0) (exp.size.tmp - nEntries) / exp.size.tmp else 1
      
      if(minPerChks <= perChk.tmp & perChk.tmp <= maxPerChks){
        perChk <- perChk.tmp
        break
      }
    }
  }
  if(!return.params & !is.na(perChk)) return(perChk)
  if(!return.params & is.na(perChk)) return(1)
  if(return.params) return(list(perChk=perChk, nFill=nFill.tmp, nFieldCols=min.nFieldCols, nBlkRows=cand.nBlks.mat[1,c], nBlkCols=cand.nBlks.mat[2,c], nBlks=cand.nBlks[c])) 
}

### Scenario 4.2 function -- user provides only nFieldCols, find rest
#perChk.start <- perChk.list[1]
scenario4.2 <- function(perChk.start, return.params){
  perChk <- NA
  minPlots <- ceiling((1+perChk.start) * nEntries)
  min.nFieldRows <- ceiling(minPlots / nFieldCols)
  
  while(prime(min.nFieldRows)) min.nFieldRows <- min.nFieldRows + 1
  
  ## Set up candidate column dimensions
  cand.nBlkCols <- reduce(nFieldCols)
  cand.nColsPerBlk <- nFieldCols / cand.nBlkCols
  cands2remove <- cand.nColsPerBlk < minColBlkDim | cand.nColsPerBlk > maxColBlkDim
  cand.nBlkCols <- cand.nBlkCols[!cands2remove]
  cand.nColsPerBlk <- cand.nColsPerBlk[!cands2remove]  
  
  ## Set up candidate column dimensions
  cand.nBlkRows <- reduce(min.nFieldRows) ## leave in this order to test the largest blocks possible first
  cand.nRowsPerBlk <- min.nFieldRows / cand.nBlkRows
  cands2remove <- (cand.nRowsPerBlk < minRowBlkDim | cand.nRowsPerBlk > maxRowBlkDim | cand.nRowsPerBlk == min.nFieldRows)
  cand.nBlkRows <- cand.nBlkRows[!cands2remove]
  cand.nRowsPerBlk <- cand.nRowsPerBlk[!cands2remove]
  
  ## List of candidate block sizes
  cand.nBlks.mat <- rbind(rep(cand.nBlkRows, times=length(cand.nBlkCols)), rep(cand.nBlkCols, each=length(cand.nBlkRows)))
  cand.nBlks <- apply(cand.nBlks.mat, 2, function(X) return(X[1]*X[2]))
  cands2remove <- cand.nBlks < nBlks.min
  cand.nBlks.mat <- as.matrix(cand.nBlks.mat[,!cands2remove])
  cand.nBlks <- cand.nBlks[!cands2remove]  
  
  if(length(cand.nBlks) == 0) return(1) else{
    for(c in 1:length(cand.nBlks)){
      nTotal.tmp <- nEntries + cand.nBlks[c] + nChk2*nChk2.min
      exp.size.tmp <- min.nFieldRows*nFieldCols
      nFill.tmp <- exp.size.tmp - nTotal.tmp ## This whole loop is to calculate the nFill.tmp parameter to make sure that the field is big enough to accomodate all entries and required checks (as defined), plus enough "extra" to satisfy minPerChk requirement
      
      perChk.tmp <- if(nFill.tmp >= 0) (exp.size.tmp - nEntries) / exp.size.tmp else 1
      
      if(minPerChks <= perChk.tmp & perChk.tmp <= maxPerChks){
        perChk <- perChk.tmp
        break
      }
    }
  }
  if(!return.params & !is.na(perChk)) return(perChk)
  if(!return.params & is.na(perChk)) return(1)
  if(return.params) return(list(perChk=perChk, nFill=nFill.tmp, nFieldRows=min.nFieldRows, nBlkRows=cand.nBlks.mat[1,c], nBlkCols=cand.nBlks.mat[2,c], nBlks=cand.nBlks[c])) 
}



