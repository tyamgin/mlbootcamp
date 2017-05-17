jgc <- function() 
  .jcall("java/lang/System", method = "gc")

unnameMatrix = function (XX) 
  as.matrix(unname(data.matrix(XX)))

extendCols = function (XX, idxes=NULL, pairs=F, angles=F, x11=F, x11bin=F) {
  X11 = XX$X11
  
  if (x11bin) {
    XXB = foreach(col=intCols, .combine=cbind) %do% {
      x = XX[, col]
      a = ifelse(X11 == 0, x, NA)
      b = ifelse(X11 != 0, x, NA)
      r = matrix(c(a, b), ncol=2)
      cname = colnames(XX)[col]
      colnames(r) = c(paste0(cname, '_11_0'), paste0(cname, '_11_1'))
      r
    }
  } else {
    XXB = matrix(NA, nrow=nrow(XX), ncol=0)  
  }
  
  XXA = matrix(NA, nrow=nrow(XX), ncol=0)
  if (is.logical(angles) && angles) {
    for (i in 1:nrow(ang.result[order(ang.result$cost),])) {
      r = ang.result[i, ]
      a = XX[, r$col1] - r$x
      b = XX[, r$col2] - r$y
      Z = matrix(atan2(b, a))
      colnames(Z) = paste0('atan2(', colnames(XX)[r$col2], ', ', colnames(XX)[r$col1], ')')
      XXA = cbind(XXA, Z)
    }
  }
  
  if (is.vector(idxes)) {
    XX = XX[, which(1 == idxes)]
  }
  
  if (is.logical(pairs) && pairs || length(pairs) > 1) {
    cnames = colnames(XX)
    sz = ncol(XX)
    for(i in 1:sz) {
      for(j in 1:sz) {
        if (i == j)
          next
        if (i > j) {
          Z = matrix(XX[, i] * XX[, j])
          colnames(Z) = paste0(cnames[i], '*', cnames[j])
          XX = cbind(XX, Z)
        }
        Z = matrix(XX[, i] / (XX[, j] - min(XX[, j] + 1)))
        colnames(Z) = paste0(cnames[i], '/', cnames[j])
        XX = cbind(XX, Z)
      }
    }
    #XX = cbind(XX, atan2(XX$X80 + 1, XX$X97 + 0.05))
  }
  
  XX = cbind(XX, XXA)
  
  if (length(pairs) > 1) {
    XX = XX[, which(1 == pairs)]
  }
  
  if (x11) {
    XX = cbind(XX, ifelse(X11 == 0, 0, 1))
  }
  
  XX = cbind(XX, XXB)
  
  XX
}

extendXYCols = function (XL, ...) {
  X = XL[, -ncol(XL), drop=F]
  Y = XL[, ncol(XL), drop=F]
  X = extendCols(X, ...)
  cbind(X, Y)
}

my.extendedColsTrain = function (XL, trainFunc, ..., newdata=NULL) {
  featuresNumber = ncol(XL) - 1

  XL = extendXYCols(XL, ...)
  
  proc = function (X) {
    if (is.null(X))
      return(X)
    if (is.list(X) && !is.matrix(X) && !is.data.frame(X)) {
      return(foreach(x=X) %do% {
        proc(x)
      })
    }
    
    if (ncol(X) != featuresNumber)
      stop('invalid number of columns')
    
    X = extendCols(X, ...)
    X
  }
  model = trainFunc(XL, newdata=proc(newdata))
  
  function (X) model(proc(X))
}

my.normalizedTrain = function (XL, trainFunc, newdata=NULL) {
  m = ncol(XL) - 1
  means = rep(NA, m)
  sds = rep(NA, m)
  for (j in 1:m) {
    idxes = which(!is.na(XL[, j]))
    means[j] = mean(XL[idxes, j])
    sds[j] = sd(XL[idxes, j])
    XL[idxes, j] = (XL[idxes, j] - means[j]) / sds[j]
  }
  
  proc = function (X) {
    if (is.null(X))
      return( X )
    if (is.list(X) && !is.matrix(X) && !is.data.frame(X))
      return( foreach(x=X) %do% proc(x) )
    
    for (j in 1:m) {
      idxes = which(!is.na(X[, j]))
      X[idxes, j] = (X[idxes, j] - means[j]) / sds[j]
    }
    X
  }
  
  model = trainFunc(XL, newdata=proc(newdata))
  function (X) model(proc(X))
}

my.log = function(x, base=exp(1)) {
  for(i in 1:length(x)) {
    if (x[i] > 0)
      x[i] = log(x[i], base)
    else if (x[i] < 0)
      x[i] = -log(-x[i], base)
  }
  x
}

my.roundAns = function (X, ans) {
  if (is.vector(ans)) {
    nrows = length(ans) / nrow(X)
    mat = matrix(ans, nrow=nrows, byrow=F)
  } else {
    nrows = ncol(ans)
    mat = matrix(c(as.matrix(ans)), nrow=nrows, byrow=T)
  }
  
  if (length(ans) == nrow(X)) {
    print(c(min(ans), max(ans)))
    ans = pmax(0, pmin(nrows - 1, round(ans)))
    return( ans )
  }
  
  foreach(x=mat, .combine=c) %do% { 
    which.max(x) - 1 
  }
}

my.roundedTrain = function (XL, trainFunc, newdata=NULL) {
  model = trainFunc(XL, newdata=newdata)
  function (X) {
    ans = model(X)
    my.roundAns(X, ans)
  }
}

my.checkedRangeTrain = function (XL, trainFunc, newdata=NULL) {
  model = trainFunc(XL, newdata=newdata)
  function (X) {
    ans = model(X)
    if (is.vector(ans))
      stop('unsupported')
    
    for(col in 1:ncol(X)) {
      mn = min(XL[, col])
      mx = max(XL[, col])
      len = mx - mn
      alpha = 0.0
      mx = mx + len * alpha
      mn = mn - len * alpha
      ans[which(X[, col] < mn | X[, col] > mx), col] = 0
    }
    ans
  }
}