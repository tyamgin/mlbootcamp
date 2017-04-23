my.dopar.exports = c()
my.dopar.packages = c()

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
  aggregator(algos)
}

my.train.lgb = function (XLL, iters=10, rowsFactor=0.3, aggregator=meanAggregator) {
  my.boot(XLL, function (XL, XK) {
    dtrain = lgb.Dataset(data=XL[, -ncol(XL)], label=XL[, ncol(XL)], free_raw_data=FALSE)
    dtest = lgb.Dataset(data=XK[, -ncol(XK)], label=XK[, ncol(XK)], free_raw_data=FALSE)
    valids = list(train=dtrain, test=dtest)
    
    tmp.lgb.model <<- lgb.train(
      data=dtrain, num_leaves=16, max_depth=5, learning_rate=0.06,
      nrounds=2000, 
      min_data_in_leaf=100, lambda_l2=5,
      valids=valids, early_stopping_rounds=200,
      eval=c('multi_error'), objective='multiclass',num_classes=5, verbose=0, nthread=4
    )
    tmp.lgb.model
  }, aggregator, iters=iters, rowsFactor=rowsFactor, replace=T, nthread=1)
}


lgbTrainAlgo = function (XL) {
  my.roundedTrain(XL, function (XL) {
    my.extendedColsTrain(XL, function (XL) {
      my.normalizedTrain(XL, function (XL) {
        my.train.lgb(XL, rowsFactor=0.9, iters=200)
      })
    }, c(0,0,0,1,0,0,                
         0,0,0,1,1,1,                
         1,0,1,1,0,1,                
         1,1,1,1,0,0,                
         0,0,0,0,0,1,                
         0,0,1,0,0,0,                
         0,0,0,0,0,0,                
         0,1,0,0,0,0,                
         0,1,1,1,1,0,                
         0,0,0,0,0,0,                
         1,0,0,1,1,0,                
         0,0,1,0,0,0,                
         1,0,1,1,1,1,                
         0,0,0,0,0,1,                
         0,1,0,1,0,1,                
         0,0,1,1,0,0,                
         0,0,0,0,0,1,                
         0,1,0,0,1,0,                
         0,1,0,0,0,0,                
         0,0,0,0,0,0,                
         1,1,0,0,1,0,                
         1,0,1,0,0,0,                
         0,0,1,0,0,0,                
         1,1,0,0,0,0,                
         0,1,0,0,0,0,                
         0,0,1,0,0,1,                
         0,0,0,1,1,1,                
         0,1,0,0,0,0,                
         0,0,0,0,1,0,                
         0,0,0,0,0,0,                
         0,0,1,0,1,1,                
         1,0,1,0,0,0,                
         0,0,0,1,0,0,                
         0,0,0,1,0,1,                
         1,0,1,1,1,0,                
         0,0,0,1,0,1,                
         0,0,1,1,0,0,                
         0))
  })
}