
my.train.lgb = function (XLL, params) {
  if (is.data.frame(XLL))
    XLL = as.matrix(XLL)
  
  ret = my.boot(XLL, function (XL, XK) {
    dtrain = lgb.Dataset(data=XL[, -ncol(XL), drop=F], label=XL[, ncol(XL), drop=T], free_raw_data=FALSE)
    
    if (params$early_stopping_rounds <= 0) {
      early_stopping_rounds = NULL
      valids = list()
    } else {
      dtest = lgb.Dataset(data=XK[, -ncol(XK)], label=XK[, ncol(XK)], free_raw_data=FALSE)
      valids = list(train=dtrain, test=dtest)
      early_stopping_rounds = params$early_stopping_rounds
    }
    
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
      bagging_freq=params$bagging_freq,
      valids=valids, 
      early_stopping_rounds=early_stopping_rounds,
      objective='binary', 
      metric='auc',
      verbose=-1, 
      feature_fraction_seed=sample(1:1000, 1),
      bagging_seed=sample(1:1000, 1),
      #device="gpu",
      #max_bin=127,
      #bin_construct_sample_cnt=2000000,
      nthread=params$nthread
    )
    g.model <<- model
    function (X) {
      if (is.data.frame(X))
        X = as.matrix(X)
      predict(model, X)
    }
  }, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
  
  ret
}

lgbParams = list(
  iters=1,
  rowsFactor=1,
  
  num_leaves=c(18),
  nrounds=c(1400),
  learning_rate=c(0.04),
  
  max_depth=9,
  #lambda_l1=c(5),
  lambda_l2=c(100),
  feature_fraction=c(0.04),
  min_data_in_leaf=20,#382,
  bagging_fraction=c(0.85),
  bagging_freq=2,
  early_stopping_rounds=0,
  nthread=4
)
