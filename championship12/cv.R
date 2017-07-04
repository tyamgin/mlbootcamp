my.set.seed = function(seed) {
  my.tmp.nextSeed <<- sample(1:10^5, 1)
  set.seed(seed)
}

my.restore.seed = function() {
  set.seed(my.tmp.nextSeed) 
}

my.boot = function (XLL, train, aggregator, iters=10, rowsFactor=0.3, replace=F, nthread=1) {
  n = nrow(XLL)
  
  if (nthread > 1) {
    cl <- makeCluster(nthread)
    registerDoParallel(cl)
  }
  
  algos = foreach(it=1:iters, .export=my.dopar.exports, .packages=my.dopar.packages) %do% {
    sampleIdxes = sample(n, rowsFactor*n, replace=replace)
    
    XK = XLL[-sampleIdxes, ]
    XL = XLL[sampleIdxes, ]  
    
    if (it %% 20 == 0)
      gc()
    
    train(XL, XK)
  }
  
  if (nthread > 1) {
    stopCluster(cl)
  }
  
  if (is.character(aggregator) || is.factor(aggregator)) {
    do.call(as.character(aggregator), list(algos))
  } else if (is.function(aggregator)) {
    aggregator(algos)
  } else {
    stop('invalid aggregator type')
  }
}

error.accuracy = function (act, pred) {
  mean(act != pred)
}

# see https://www.kaggle.com/wiki/LogarithmicLoss
error.logloss = function (act, pred) {
  if (length(act) != length(pred)) {
    stop("length's must be equal")
  }
  eps = 1e-15
  pred = pmin(pmax(pred, eps), 1 - eps)
  sum(act * log(pred) + (1 - act) * log(1 - pred)) * -1/NROW(act)
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

makeHoles = function (X) {
  if ('smoke' %in% colnames(X))
    X$smoke[sample(1:nrow(X), nrow(X) * 0.101)] = NA
  if ('alco' %in% colnames(X))
    X$alco[sample(1:nrow(X), nrow(X) * 0.1010333)] = NA
  if ('active' %in% colnames(X))
    X$active[sample(1:nrow(X), nrow(X) * 0.09656667)] = NA
  X
}

validation.tqfold = function (XLL, teachFunc, folds=5, iters=10, verbose=F, use.newdata=F, seed=0) {
  XKerr = c()
  
  nrows = length(unique(XLL[, ncol(XLL)]))

  validation.tqfold.enumerate(function (XL, XK, it, fold) {
    #XK = makeHoles(XK)
    if (seed > 0) {
      set.seed(seed)
      #seed <<- 0
    }
    
    act = XK[, ncol(XL)]
    if (use.newdata) {
      algo = teachFunc(XL, newdata=XK[, -ncol(XL)])
      pred = algo(XK[, -ncol(XL)])
    } else {
      algo = teachFunc(XL)
      pred = algo(XK[, -ncol(XL)])
    }
    
    e = error.logloss(act, pred)
    
    XKerr <<- c(XKerr, e)
    
    if (verbose)
      print(paste0('tqfold ', it, '-', fold, '/', iters, '-', folds, ' cur=', e, ' mean=', mean(XKerr), ' sd=', sd(XKerr)))
    
  }, XLL, folds=folds, iters=iters)
  
  XKerr
}

my.gridSearch = function (XLL, teach, grid, folds=7, iters=6, verbose=F, use.newdata=F, folds.seed=777, train.seed=2) {
  minE = 1e10
  for (i in 1:nrow(grid)) {
    params = grid[i, ]
    
    my.set.seed(folds.seed)
    val = validation.tqfold(XLL, teach(params), folds=folds, iters=iters, verbose=verbose, use.newdata=use.newdata, seed=train.seed)
    my.restore.seed()
    e = mean(val)
    params$LOGLOSS = e
    params$SD = sd(val)
    
    print(params)
    
    if (e < minE) {
      minE = e
      selParams = params
    }
    gc()
  }
  print('-------------------------------')
  print(selParams)
}