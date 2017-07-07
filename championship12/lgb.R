my.train.lgb = function (XLL, params, newdata=NULL) {
  XLL = unnameMatrix(XLL)  
  hash = my.matrixHash(XLL)
  
  cache_filename = paste0('cache2/lgb_', hash)
  if ((my.enableCache == T || my.enableCache == 'readOnly') && file.exists(cache_filename)) {
    #print('[lgb from cache]')
    my.boot(XLL, function (XL, XK) {}, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
    return(readRDS(cache_filename))
  }
  if (my.enableCache == 'readOnly') stop('my.train.lgb cache is read only')
  
  ret = my.boot(XLL, function (XL, XK) {
    dtrain = lgb.Dataset(data=XL[, -ncol(XL)], label=XL[, ncol(XL)], free_raw_data=FALSE)
    
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
      valids=valids, 
      early_stopping_rounds=early_stopping_rounds,
      objective='binary', 
      metric='binary_logloss',
      verbose=0, 
      feature_fraction_seed=sample(1:1000, 1),
      bagging_seed=sample(1:1000, 1),
      nthread=params$nthread
    )
    ret = function (X) {
      predict(model, unnameMatrix(X))
    }
    
    if (!is.null(newdata)) {
      if (is.matrix(newdata) || is.data.frame(newdata))
        newdata = list(newdata)
      results = list()
      for (i in 1:length(newdata))
        results[[i]] = ret(newdata[[i]])
      
      rm(model)
      return( function (X) {
        for (i in 1:length(newdata))
          if (my.matrixEquals(newdata[[i]], X))
            return( results[[i]] )
        
        stop('[lgb] newdata is not available')
      } )
    }
    
    ret
  }, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
  
  if (my.enableCache == T) {
    saveRDS(ret, cache_filename)
  }
  
  ret
}

lgb.features = c('age', 'gender', 'weight', 'ap_hi', 'ap_lo', 'cholesterol', 'gluc', 'smoke', 'alco', 'active', 
                 'cholesterol_le1_and_gluc_le1', 'lol2', 'lol3', 'fat',
                 "smoke_le0_and_alco_le0", "gender_le1_and_cholesterol_le2", 'log_height_div_log_weight', 
                 'log_age_mul_pow2_height')

lgbTrainAlgo = function (XL, params, newdata=NULL) {
  my.fixedDataTrain(XL, function (XL, newdata=NULL) {
    my.filledHolesTrain(XL, function (XL, newdata=NULL) {
      my.extendedColsTrain(XL, function (XL, newdata=NULL) {
        my.normalizedTrain(XL, function (XL, newdata=NULL) {
          my.train.lgb(XL, params, newdata)
        }, newdata=newdata)
      }, features=lgb.features, newdata=newdata)
    }, newdata=newdata)
  }, newdata=newdata)
}