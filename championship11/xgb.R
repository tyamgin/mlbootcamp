my.train.xgb = function (XLL, iters=10, rowsFactor=0.3, aggregator=meanAggregator) {
  my.boot(XLL, function (XL, XK) {
    dtrain = xgb.DMatrix(data=XL[, -ncol(XL)], label=XL[, ncol(XL)])
    dtest = xgb.DMatrix(data=XK[, -ncol(XK)], label=XK[, ncol(XK)])
    watchlist = list(train=dtrain, test=dtest)
    
    xgb.train(
      data=dtrain, 
      #watchlist=watchlist, 
      max_depth=3, gamma=5, eta=0.06, 
      colsample_bytree=0.5,
      #early_stopping_rounds=200,
      num_parallel_tree=500,
      nthread=4, nrounds=2, eval_metric='merror', objective='multi:softprob', num_class=5, verbose=0
    )
  }, aggregator, iters=iters, rowsFactor=rowsFactor, replace=T, nthread=1)
}

xgbTrainAlgo = function (XL) {
  my.roundedTrain(XL, function (XL) {
    my.extendedColsTrain(XL, function(XL) {
      my.normalizedTrain(XL, function (XL) {
        my.train.xgb(XL, rowsFactor=1, iters=1)
      })
    }, c(0,0,0,0,1,0,
         0,0,0,1,0,1,
         1,0,0,0,0,0,
         0,1,1,1,0,0,
         0,0,0,1,0,0,
         1,0,0,1,1,1,
         1,0,1,0,0,0,
         1,0,0,0,0,1,
         0,1,0,1,0,0,
         0,1,0,0,1,1,
         1,0,0,1,1,0,
         0,0,1,0,0,0,
         1,0,0,1,1,1,
         1,1,0,0,0,0,
         0,0,0,1,0,0,
         0,0,1,0,1,1,
         1,0,0,1,1,0,
         0,0,1,1,0,0,
         1,0,1,0,0,0,
         0,0,0,0,0,0,
         0,1,0,1,0,1,
         0,1,0,0,0,0,
         1,0,0,1,0,1,
         1,0,0,1,0,0,
         0,0,0,0,0,0,
         1,0,0,0,0,1,
         1,0,1,1,1,0,
         0,1,1,0,0,0,
         0,0,0,0,1,1,
         1,1,0,0,1,1,
         1,0,0,1,1,1,
         1,0,1,1,0,1,
         1,0,0,1,1,1,
         0,0,0,0,0,1,
         1,1,1,1,0,1,
         0,1,1,0,0,0,
         1,0,1,1,0,0,
         1))
  })
}