require(pROC)

my.set.seed = function(seed) {
  my.tmp.nextSeed <<- sample(1:10^5, 1)
  set.seed(seed)
}

my.restore.seed = function() {
  set.seed(my.tmp.nextSeed) 
}

is.sorted = function (x) {
  !is.unsorted(x)
}

meanAggregator = function (baseAlgos, w=NULL) {
  l = length(baseAlgos)
  if (is.null(w))
    w = rep(1/l, l)
  else if (sum(w) != 1)
    stop('sum of weight\'s must be 1')
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

gmeanAggregator = function (baseAlgos, w=NULL) {
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

my.boot = function (XLL, train, aggregator, iters=10, rowsFactor=0.3, replace=F, nthread=1) {
  n = nrow(XLL)
  
  if (nthread > 1) {
    cl <- makeCluster(nthread)
    registerDoParallel(cl)
  }
  
  algos = foreach(it=1:iters) %do% {
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
      cat('tqfold ', it, '-', fold, '/', iters, '-', folds, ' ', round(sum(act == 1) / length(act) * 100),  '% cur=', e, ' mean=', mean(XKerr), ' sd=', sd(XKerr), '\n', sep='')
    
  }, XLL, folds=folds, iters=iters)
  
  XKerr
}

validation.tqfold.parallel = function (XLL, teachFunc, folds=5, iters=10, resample.seed = 0, algo.seed=0, params=NULL) {
  nrows = length(unique(XLL[, ncol(XLL), drop=T]))
  
  if (resample.seed > 0) {
    set.seed(resample.seed)
  }
  
  resamples = matrix(NA, nrow=iters, ncol=nrow(XLL))
  for (i in 1:iters) {
    resamples[i, ] = sample(nrow(XLL))
  }
  
  mean(foreach(it=1:iters, .combine=c,
               .export=c('my.extendedColsTrain', 'my.fillNasTrain', 'my.train.lgb', 'my.train.lmr', 'my.train.lm', 'my.train.lm2', 'extendXYCols', 'feats_lmr', 'feats_lm', 'feats', 'my.boot', 'lgbParams', 'meanAggregator'),
               .packages=c('foreach', 'lightgbm', 'pROC', 'MASS')
               ) %dopar% {
    perm = resamples[it, ]
    mean(foreach(fold=1:folds, .combine=c) %do% {
      foldLength = floor(nrow(XLL) / folds)
      foldStart = (fold - 1) * foldLength
      foldEnd = foldStart + foldLength - 1
      
      controlIdxes = perm[foldStart:foldEnd]
      XK = XLL[controlIdxes, ]
      XL = XLL[-controlIdxes, ]  
      
      if (algo.seed > 0) {
        set.seed(algo.seed)
      }
      
      act = XK[, ncol(XL), drop=T]

      algo = teachFunc(XL, params)
      pred = algo(XK[, -ncol(XL)])
      
      e = roc(act, pred)$auc
      
      cat('tqfold ', it, '-', fold, '/', iters, '-', folds, ' ', round(sum(act == 1) / length(act) * 100),  '% cur=', e, '\n', sep='')
      
      e
    })
  })
}
