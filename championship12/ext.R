jgc <- function() 
  .jcall("java/lang/System", method = "gc")

unnameMatrix = function (X) 
  as.matrix(unname(data.matrix(X)))

extendCols = function (X) {
  w = X$weight
  h = X$height / 100 # в метры
  #X$bmi = w / h^2 # https://ru.wikipedia.org/wiki/%D0%98%D0%BD%D0%B4%D0%B5%D0%BA%D1%81_%D0%BC%D0%B0%D1%81%D1%81%D1%8B_%D1%82%D0%B5%D0%BB%D0%B0
  #X$al_diff = X$ap_hi - X$ap_lo
  X
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