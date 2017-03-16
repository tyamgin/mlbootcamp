logitTrainAlgo = function (XL, models) {
  Y2 = unname(foreach(model=models, .combine=cbind) %do% {
    model(XL[, -ncol(XL)])
  })
  XL2 = XL
  for (i in 1:nrow(XL2))
    XL2[i, ncol(XL2)] = which.min(abs(Y2[i, ] - XL[i, ncol(XL)])) - 1
  
  finalModel = glmTrainAlgo(XL2)
  #finalModel = svmTrainAlgo(XL2)
  #finalModel = lgb2TrainAlgo(XL2)
  #finalModel = lgbTrainAlgo(XL2)
  #finalModel = nnetBootTrainAlgo(XL2)
  
  function (X) {
    pp = finalModel(X)
    models[[1]](X) * (1 - pp) + models[[2]](X) * pp
  }
}

my.ensemble.stacking = function (XL) {
  
  folds = 7
  iters = 10
  
  set.seed(2708)
  
  XKerr = c()
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
    lgbModel = cached('lgb')
    magicModel = cached('magic')
  
    
    model = logitTrainAlgo(XL, c(lgbModel, magicModel))
    e = error.logloss(XK[, ncol(XL)], model(XK[, -ncol(XL)]))

    XKerr <<- c(XKerr, e)
    
    print(paste0('tqfold ', it, '-', fold, '/', iters, '-', folds, ' mean=', mean(XKerr), ' sd=', sd(XKerr)))
      
  }, XL, folds=folds, iters=iters)
}