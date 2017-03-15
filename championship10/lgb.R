my.boot = function (XLL, train, aggregator, iters=10, rowsFactor=0.3, replace=F, nthread=1) {
  n = nrow(XLL)
  
  if (nthread > 1) {
    cl <- makeCluster(nthread)
    registerDoParallel(cl)
  }
  
  #ls(envir=globalenv())
  algos = foreach(it=1:iters, .export=my.dopar.exports, .packages=my.dopar.packages) %dopar% {
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
  aggregator(algos)
}

my.coputed(model, newdata) {
  ansvec = predict(model, newdata)
  function (X) {
    if (nrow(X) != length(ansvec))
      stop("this is computed algo")
    ansvec
  }
}

my.train.lgb = function (XLL, iters=10, rowsFactor=0.3, aggregator=meanAggregator, newdata=NULL) {
  my.boot(XLL, function (XL, XK) {
    dtrain = lgb.Dataset(data=XL[, -ncol(XL)], label=XL[, ncol(XL)], free_raw_data=FALSE)
    dtest = lgb.Dataset(data=XK[, -ncol(XK)], label=XK[, ncol(XK)], free_raw_data=FALSE)
    valids = list(train=dtrain, test=dtest)
    
    model = lgb.train(
      data=dtrain, num_leaves=9, max_depth=4, learning_rate=0.06,
      nrounds=195, 
      #valids=valids, 
      eval=c('binary_logloss'), objective = 'binary',
      nthread=4, verbose=0, 
      #early_stopping_rounds=200,
      min_data_in_leaf=100, lambda_l2=5
    )
    
    if (!is.null(newdata)) {
      my.coputed(model, newdata)
    }
    model
  }, aggregator, iters=iters, rowsFactor=rowsFactor, replace=T, nthread=1)
}


lgbTrainAlgo = function (XL, newdata=NULL) {
  my.extendedColsTrain(XL, function(XL) {
    my.normalizedTrain(XL, function (XL) {
      my.train.lgb(XL, rowsFactor=1, iters=200, newdata=newdata)
    })
  }, c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1))
}

lgb2TrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    my.train.lgb(XL, rowsFactor=0.95, iters=25)
  })
}

lgbNnetmagicTrainAlgo = function (XL) {
  lgbModel = lgbTrainAlgo(XL)
  meanAggregator(c(lgbModel, annetmagic))
}