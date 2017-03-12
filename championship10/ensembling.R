eachPartition = function (n, func, s=1) {
  idx = 1
  if (n == 2) {
    parts = 20
    for (i in 0:parts) {
      func(c(1/parts*i, 1-1/parts*i), idx)
      idx = idx + 1
    }
  } else if (n == 3) {
    parts = 30
    for (i in 0:parts) {
      for (j in i:parts) {
        func(c(1/parts*i, 1/parts*(j-i), 1/parts*(parts-j)), idx)
        idx = idx + 1
      }
    }
  } else {
    stop("not implemented")
  }
}

load("annetmagic.RData")

my.ensemble.enumerate = function (XL) {
  
  folds = 7
  iters = 10
  
  set.seed(2708)
  
  XKerr = list()
  validation.tqfold.enumerate(function (XL, XK, it, fold) {
    cached = function (name) {
      filename = paste0('cache/', name, it, '-', fold, '.rds')
      if (file.exists(filename)) {
        ans = readRDS(filename)
        return( function(X) { 
          for(v in ans) 
            if (length(v) == nrow(X)) 
              return(v) 
          stop('err')
        } )
      }
      if (name == 'magic')
        return( annetmagic )
      if (name == 'nnet') {
        model = nnetBootTrainAlgo(XL)
        saveRDS(list(model(XL[, -ncol(XL)]), model(XK[, -ncol(XK)])), filename)
        return( model )
      }
      if (name == 'lgb') {
        model = lgbTrainAlgo(XL)
        saveRDS(list(model(XL[, -ncol(XL)]), model(XK[, -ncol(XK)])), filename)
        return( model )
      }
      stop("unknown model type")
    }
    models = c(cached('lgb'), cached('nnet'), cached('magic'))
    
    print('-----------------------------------------------------------------------')
    
    minMean = 1e10
    outStr = ''
    
    eachPartition(length(models), function (w, partitionIdx) {
      model = meanAggregator(models, w)
      e = error.logloss(XK[, ncol(XL)], model(XK[, -ncol(XL)]))
      if (length(XKerr) < partitionIdx) {
        XKerr[[partitionIdx]] <<- c(e)
      } else {
        XKerr[[partitionIdx]] <<- c(XKerr[[partitionIdx]], e)
      }
      curMean = mean(XKerr[[partitionIdx]])
      curSd = sd(XKerr[[partitionIdx]])
      str = paste0('tqfold ', it, '-', fold, '/', iters, '-', folds, ' ', w[1], '+', w[2], ' mean=', curMean, ' sd=', curSd)
      if (curMean < minMean) {
        minMean <<- curMean
        outStr <<- str
      }
      if (it == iters && fold == folds) {
        print(str)
      }
    })
    
    if (it == iters && fold == folds) {
      print('+++++++++++++++++++')
      print(outStr)
    }
  }, XL, folds=folds, iters=iters)
}

my.logit = function (p) {
  -log(1/p-1)
}

logitTrainAlgo = function (XL, models) {
  XL2 = unname(cbind(foreach(model=models, .combine=cbind) %do% {
    y = model(XL[, -ncol(XL)])
    y = my.logit(y)
  }, XL[, ncol(XL)]))
  
  glmModel = glmTrainAlgo(XL2)
  
  function (X) {
    X2 = unname(foreach(model=models, .combine=cbind) %do% {
      y = model(X)
      y = my.logit(y)
    })
    glmModel(X2)
  }
}

my.ensemble.stacking = function (XL) {
  
  folds = 7
  iters = 2
  
  set.seed(2708)
  
  XKerr = c()
  validation.tqfold.enumerate(function (XL, XK, it, fold) {
    cached = function (name) {
      #filename = paste0('cache/', name, it, '-', fold, '.rds')
      #if (file.exists(filename)) {
      #  ans = readRDS(filename)
      #  return( function(X) {ans} )
      #}
      if (name == 'magic')
        return( annetmagic )
      if (name == 'lgb') {
        model = lgbTrainAlgo(XL)
        #saveRDS(model(XK[, -ncol(XK)]), filename)
        return( model )
      }
      stop("unknown model type")
    }
    lgbModel = cached('lgb')
    magicModel = cached('magic')
  
    
    model = logitTrainAlgo(XL, c(lgbModel, magicModel))
    e = error.logloss(XK[, ncol(XL)], model(XK[, -ncol(XL)]))

    XKerr <<- c(XKerr, e)
    
    print(paste0('tqfold ', it, '-', fold, '/', iters, '-', folds, ' mean=', mean(XKerr), ' sd=', sd(XKerr)))
      
  }, XL, folds=folds, iters=iters)
}