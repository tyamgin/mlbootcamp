my.train.xgb = function (XLL, iters=10, rowsFactor=0.3, aggregator=meanAggregator) {
  algos = list()
  n = nrow(XLL)
  for (it in 1:iters) {
    sampleIdxes = sample(n, rowsFactor*n)
    
    XK = XLL[-sampleIdxes, ]
    XL = XLL[sampleIdxes, ]  
    
    dtrain = xgb.DMatrix(data=XL[, -ncol(XL)], label=XL[, ncol(XL)])
    dtest = xgb.DMatrix(data=XK[, -ncol(XK)], label=XK[, ncol(XK)])
    watchlist = list(train=dtrain, test=dtest)
    
    algos[[it]] = xgb.train(
      data=dtrain, watchlist=watchlist, max_depth=3, gamma=5, eta=0.06, 
      nthread=8, nrounds=3000, eval_metric='logloss', objective='binary:logistic',
      early_stopping_rounds=50, verbose=0
    )
  }
  aggregator(algos)
}

xgbTrainAlgo = function (XL) {
  my.extendedColsTrain(XL, function(XL) {
    my.normalizedTrain(XL, function (XL) {
      my.train.xgb(XL, rowsFactor=0.9, iters=15)
    })
  })
}