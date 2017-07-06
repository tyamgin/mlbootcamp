my.train.xgb = function (XLL, params, newdata=NULL) {
  XLL = unnameMatrix(XLL)
  hash = my.matrixHash(XLL)
  
  cache_filename = paste0('cache2/xgb_', hash)
  if ((my.enableCache == T || my.enableCache == 'readOnly') && file.exists(cache_filename)) {
    #print('[xgb from cache]')
    my.boot(XLL, function (XL, XK) {}, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
    return(readRDS(cache_filename))
  }
  if (my.enableCache == 'readOnly') stop('my.train.xgb cache is read only')
  
  ret = my.boot(XLL, function (XL, XK) {
    dtrain = xgb.DMatrix(data=XL[, -ncol(XL)], label=XL[, ncol(XL)])
    
    if (params$early_stopping_rounds <= 0) {
      early_stopping_rounds = NULL
      watchlist = list()
    } else {
      dtest = xgb.DMatrix(data=XK[, -ncol(XK)], label=XK[, ncol(XK)])
      watchlist = list(train=dtrain, test=dtest)
      early_stopping_rounds = params$early_stopping_rounds
    }

    model = xgb.train(
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
      num_parallel_tree=params$num_parallel_tree,
      eval_metric='logloss', objective='binary:logistic', verbose=0
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
        
        stop('[xgb] newdata is not available')
      } )
    }
    
    ret
  }, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
  
  if (my.enableCache == T) {
    saveRDS(ret, cache_filename)
  }
  
  ret
}

xgb.features = c('age', 'gender', 'weight', 'ap_hi', 'ap_lo', 'cholesterol', 'gluc', 'smoke', 'alco', 'active', 
                 'cholesterol_le1_and_gluc_le1', 'lol2', 'lol3', 'fat',
                 "smoke_le0_and_alco_le0", "gender_le1_and_cholesterol_le2", 'log_height_div_log_weight', 
                 'log_age_mul_pow2_height')
#"cholesterol_le1_and_active_le0"

xgbTrainAlgo = function (XL, params, newdata=NULL) {
  my.fixedDataTrain(XL, function (XL, newdata=NULL) {
    my.filledHolesTrain(XL, function (XL, newdata=NULL) {
      my.extendedColsTrain(XL, function (XL, newdata=NULL) {
        my.removedNasColumnsTrain(XL, function (XL, newdata=NULL) {
          my.normalizedTrain(XL, function (XL, newdata=NULL) {
            my.train.xgb(XL, params, newdata)
          }, newdata=newdata)
        }, newdata=newdata)
      }, features=xgb.features, newdata=newdata)
    }, newdata=newdata)
  }, newdata=newdata)
}