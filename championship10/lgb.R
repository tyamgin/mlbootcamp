my.boot = function (XLL, train, aggregator, iters=10, rowsFactor=0.3) {
  algos = list()
  n = nrow(XLL)
  for (it in 1:iters) {
    sampleIdxes = sample(n, rowsFactor*n)
    
    XK = XLL[-sampleIdxes, ]
    XL = XLL[sampleIdxes, ]  

    algos[[it]] = train(XL, XK)
  }
  aggregator(algos)
}

my.train.lgb = function (XLL, iters=10, rowsFactor=0.3, aggregator=meanAggregator) {
  my.boot(XLL, function (XL, XK) {
    dtrain = lgb.Dataset(data=XL[, -ncol(XL)], label=XL[, ncol(XL)], free_raw_data=FALSE)
    dtest = lgb.Dataset(data=XK[, -ncol(XK)], label=XK[, ncol(XK)], free_raw_data=FALSE)
    valids = list(train=dtrain, test=dtest)
    
    lgb.train(
      data=dtrain, num_leaves=9, max_depth=4, learning_rate=0.06,
      nrounds=2000, valids=valids, 
      eval=c('binary_logloss'), objective = 'binary',
      nthread=4, verbose=0, early_stopping_rounds=100,
      min_data_in_leaf=100, lambda_l2=5
    )
  }, aggregator, iters=iters, rowsFactor=rowsFactor)
}

lgbTrainAlgo = function (XL) {
  my.extendedColsTrain(XL, function(XL) {
    my.normalizedTrain(XL, function (XL) {
      my.train.lgb(XL, rowsFactor=0.95, iters=200)
    })
  }, c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1))
}

lgbNnetmagicTrainAlgo = function (XL) {
  lgbModel = lgbTrainAlgo(XL)
  meanAggregator(c(lgbModel, annetmagic))
}