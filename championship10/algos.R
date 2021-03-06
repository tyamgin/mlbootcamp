# see https://www.kaggle.com/wiki/LogarithmicLoss
error.logloss = function (act, pred) {
  if (length(act) != length(pred)) {
    stop("length's must be equal")
  }
  eps = 1e-15
  pred = pmin(pmax(pred, eps), 1 - eps)
  sum(act * log(pred) + (1 - act) * log(1 - pred)) * -1/NROW(act)
}

error.mean = function (act, pred) {
  mean(abs(act - pred))
}

validation.tqfold.enumerate = function (callback, XLL, folds=5, iters=10) {
  resamples = unname(foreach(it=1:iters, .combine=rbind) %do% sample(nrow(XLL)))
  for (it in 1:iters) {
    perm = resamples[it, ]
    for (fold in 1:folds) {
      foldLength = floor(nrow(XLL) / folds)
      foldStart = (fold - 1) * foldLength
      foldEnd = foldStart + foldLength - 1
      
      controlIdxes = perm[foldStart:foldEnd]
      XK = XLL[controlIdxes, ]
      XL = XLL[-controlIdxes, ]  
      
      callback(XL, XK, it, fold)
    }
  }
}

validation.tqfold = function (XLL, teachFunc, folds=5, iters=10, verbose=F) {
  XKerr = c()
  
  validation.tqfold.enumerate(function (XL, XK, it, fold) {
    algo = teachFunc(XL)
    e = error.logloss(XK[, ncol(XL)], algo(XK[, -ncol(XL)]))

    XKerr <<- c(XKerr, e)
    
    if (verbose)
      print(paste0('tqfold ', it, '-', fold, '/', iters, '-', folds, ' cur=', e, ' mean=', mean(XKerr), ' sd=', sd(XKerr)))
    
  }, XLL, folds=folds, iters=iters)
  XKerr
}

meanAggregator = function (baseAlgos, w=NULL) {
  l = length(baseAlgos)
  if (is.null(w))
    w = rep(1/l, l)
  function(x) {
    s = 0
    for (i in 1:l) {
      if (is.function(baseAlgos[[i]])) {
        s = s + w[i] * baseAlgos[[i]](x)
      } else {
        s = s + w[i] * predict(baseAlgos[[i]], x)
      }
    }
    s
  }
}

gmeanAggregator = function (baseAlgos) {
  l = length(baseAlgos)
  function(x) {
    s = 1
    for (algo in baseAlgos) {
      if (is.function(algo)) {
        s = s * algo(x);
      } else {
        s = s * predict(algo, x);
      }
    }
    s ^ (1/l)
  }
}

