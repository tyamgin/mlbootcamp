require(pROC)


my.set.seed = function(seed) {
  my.tmp.nextSeed <<- sample(1:10^5, 1)
  set.seed(seed)
}

my.restore.seed = function() {
  set.seed(my.tmp.nextSeed) 
}

validation.tqfold.enumerate = function (callback, XLL, folds=5, iters=10) {
  resamples = matrix(NA, nrow=iters, ncol=nrow(XLL))
  for (i in 1:iters) {
    resamples[i, ] = sample(nrow(XLL))
  }
  
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


validation.tqfold = function (XLL, teachFunc, folds=5, iters=10, verbose=F, use.newdata=F, seed=0) {
  XKerr = c()
  
  nrows = length(unique(XLL[, ncol(XLL), drop=T]))

  validation.tqfold.enumerate(function (XL, XK, it, fold) {
    if (seed > 0) {
      set.seed(seed)
    }
    
    act = XK[, ncol(XL), drop=T]
    if (use.newdata) {
      algo = teachFunc(XL, newdata=XK[, -ncol(XL)])
      pred = algo(XK[, -ncol(XL)])
    } else {
      algo = teachFunc(XL)
      pred = algo(XK[, -ncol(XL)])
    }
    
    e = roc(act, pred)$auc
    
    XKerr <<- c(XKerr, e)
    
    if (verbose)
      print(paste0('tqfold ', it, '-', fold, '/', iters, '-', folds, ' cur=', e, ' mean=', mean(XKerr), ' sd=', sd(XKerr)))
    
  }, XLL, folds=folds, iters=iters)
  
  XKerr
}