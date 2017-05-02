my.train.xgb = function (XLL, iters=10, rowsFactor=0.3, aggregator=meanAggregator,
                         max_depth=4, gamma=5, lambda=1, alpha=0.2, eta=0.06, tree_method='exact',
                         colsample_bytree=0.635, min_child_weight=2, subsample=1, nthread=4, nrounds=350
                         ) {
  my.boot(XLL, function (XL, XK) {
    dtrain = xgb.DMatrix(data=XL[, -ncol(XL)], label=XL[, ncol(XL)])
    dtest = xgb.DMatrix(data=XK[, -ncol(XK)], label=XK[, ncol(XK)])
    watchlist = list(train=dtrain, test=dtest)
    
    tmp.xgb.model <<- xgb.train(
      data=dtrain, 
      #watchlist=watchlist, 
      max_depth=max_depth, 
      gamma=gamma, 
      lambda=lambda,
      alpha=alpha,
      eta=eta, 
      tree_method=tree_method,
      colsample_bytree=colsample_bytree,
      min_child_weight=min_child_weight,
      subsample=subsample,
      #early_stopping_rounds=200,
      nthread=nthread, 
      nrounds=nrounds, 
      eval_metric='merror', objective='multi:softprob', num_class=5, verbose=0
    )
    tmp.xgb.model
  }, aggregator, iters=iters, rowsFactor=rowsFactor, replace=T, nthread=1)
}

xeee = c(0,1,0,0,0,1,0,1,0,0,1,1,0,1,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,
         0,0,0,0,0,1,1,0,0,0,1,1,1,0,1,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,1,0,0,0,1,0,0,1,0,1,1,0,1,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,
         0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,
         0,1,0,1)

#xeee = rep(1, 223)

xgbTrainAlgo = function (XL) {
  my.roundedTrain(XL, function (XL) {
    my.extendedColsTrain(XL, function(XL) {
      my.normalizedTrain(XL, function (XL) {
        my.train.xgb(XL, rowsFactor=1, iters=1)
      })
    }, xeee)
  })
}