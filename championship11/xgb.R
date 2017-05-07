my.train.xgb = function (XLL, params) {
  my.boot(XLL, function (XL, XK) {
    dtrain = xgb.DMatrix(data=XL[, -ncol(XL)], label=XL[, ncol(XL)])
    
    if (params$early_stopping_rounds <= 0) {
      early_stopping_rounds = NULL
      watchlist = list()
    } else {
      dtest = xgb.DMatrix(data=XK[, -ncol(XK)], label=XK[, ncol(XK)])
      watchlist = list(train=dtrain, test=dtest)
      early_stopping_rounds = params$early_stopping_rounds
    }
    
    tmp.xgb.model <<- xgb.train(
      data=dtrain, 
      watchlist=watchlist, 
      early_stopping_rounds=early_stopping_rounds,
      max_depth=params$max_depth, 
      gamma=params$gamma, 
      lambda=params$lambda,
      alpha=params$alpha,
      eta=params$eta, 
      tree_method='exact',
      colsample_bytree=params$colsample_bytree,
      min_child_weight=params$min_child_weight,
      subsample=params$subsample,
      nthread=params$nthread, 
      nrounds=params$nrounds, 
      eval_metric='merror', objective='multi:softprob', num_class=5, verbose=0
    )
    tmp.xgb.model
  }, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
}

xeee=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,1)

neee=rep(0, 223)
for (i in c(139,  80,  12, 201, 183,  77, 132, 157,  97, 116,  98)) {
  neee[i] = 1
}

xgbTrainAlgo = function (XL, params, newdata) {
  my.roundedTrain(XL, function (XL, newdata) {
    my.extendedColsTrain(XL, function(XL, newdata) {
      my.normalizedTrain(XL, function (XL, newdata) {
        my.train.xgb(XL, params)
      })
    }, xeee)
  })
}