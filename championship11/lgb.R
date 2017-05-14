my.train.lgb = function (XLL, params, newdata=NULL) {
  XLL = unnameMatrix(XLL)
  my.boot(XLL, function (XL, XK) {
    dtrain = lgb.Dataset(data=XL[, -ncol(XL)], label=XL[, ncol(XL)], free_raw_data=FALSE)
    
    if (params$early_stopping_rounds <= 0) {
      early_stopping_rounds = NULL
      valids = list()
    } else {
      dtest = lgb.Dataset(data=XK[, -ncol(XK)], label=XK[, ncol(XK)], free_raw_data=FALSE)
      valids = list(train=dtrain, test=dtest)
      early_stopping_rounds = params$early_stopping_rounds
    }
    
    num_class = length(unique(XL[, ncol(XL)]))
    
    model = lgb.train(
      data=dtrain, 
      num_leaves=params$num_leaves, 
      max_depth=params$max_depth, 
      learning_rate=params$learning_rate,
      nrounds=params$nrounds,
      min_data_in_leaf=params$min_data_in_leaf, 
      lambda_l2=params$lambda_l2,
      feature_fraction=params$feature_fraction,
      bagging_fraction=params$bagging_fraction,
      valids=valids, 
      early_stopping_rounds=early_stopping_rounds,
      eval='multi_error', 
      objective='multiclass',
      num_classes=num_class, 
      verbose=0, 
      nthread=params$nthread
    )
    ret = function (X) {
      r = predict(model, unnameMatrix(X))
      matrix(r, byrow=T, ncol=num_class)
    }
    
    if (!is.null(newdata)) {
      ret = ret(newdata)
      rm(model)
      return( function (X) ret )
    }
    
    ret
  }, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
}

lgbTrainAlgo = function (XL, params, newdata=NULL) {
  my.extendedColsTrain(XL, function(XL, newdata=NULL) {
    my.normalizedTrain(XL, function (XL, newdata=NULL) {
      my.train.lgb(XL, params, newdata)
    }, newdata=newdata)
  }, idxes=xeee, pairs=xppp, angles=F, newdata=newdata)
}

lgbParams = expand.grid(
  iters=1,
  rowsFactor=1,
  
  max_depth=9,
  num_leaves=15,
  learning_rate=0.03,
  nrounds=c(600),
  min_data_in_leaf=100,
  lambda_l2=0.2,
  feature_fraction=0.5,
  bagging_fraction=c(0.8),
  nthread=4, 
  
  early_stopping_rounds=0
)