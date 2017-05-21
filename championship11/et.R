my.train.et = function (XL, params, newdata=NULL) {
  XL = unnameMatrix(XL)
  
  hash = my.matrixHash(XL)
  
  if (my.enableCache == T && is.null(newdata)) {
    stop('cache without newdata is not working')
  }
  
  cache_filename = paste0('cache2/et_', hash)
  if ((my.enableCache == T || my.enableCache == 'readOnly') && file.exists(cache_filename)) {
    #print('[et from cache]')
    my.boot(XLL, function (XL, XK) {}, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
    return(readRDS(cache_filename))
  }
  if (my.enableCache == 'readOnly') stop('my.train.et cache is read only')
  
  ret = my.boot(XL, function (XL, XK) {
    X = XL[, -ncol(XL)]
    colnames(X) <- paste0('X', 1:ncol(X))
    Y = factor(XL[, ncol(XL)], labels=c('a', 'b', 'c', 'd', 'e')[1:length(unique(XL[, ncol(XL)]))])
    
    trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
    
    tuneGrid = expand.grid(
      numRandomCuts=params$numRandomCuts,
      mtry=params$mtry
    )
    
    model <- train(X, Y, method='extraTrees', metric='Accuracy',
                   maximize=F, trControl=trControl,
                   ntree=params$ntree,
                   nodesize=params$nodesize,
                   numThreads=4,
                   na.action='fuse',
                   tuneGrid=tuneGrid)
    
    ret = function (X) {
      X = unnameMatrix(X)
      colnames(X) <- paste0('X', 1:ncol(X))
      predict(model, X, type='prob')
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
        
        stop('[et] newdata is not available')
      } )
    }
    
    ret
  }, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
  
  if (my.enableCache == T) {
    saveRDS(ret, cache_filename)
  }
  
  ret
}


intCols = c(139,  80,  12, 201, 183,  77, 132, 157,  97, 116,  98)

neee=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

nppp=c(1,1,1,1,0,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, #1
       0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)


etTrainAlgo = function (XL, params, newdata=NULL) {
  my.extendedColsTrain(XL, function(XL, newdata=NULL) {
    my.normalizedTrain(XL, function (XL, newdata=NULL) {
      my.train.et(XL, params, newdata=newdata)
    }, newdata=newdata)
  }, idxes=neee, pairs=nppp, angles=F, newdata=newdata)
}

etGlmTrainAlgo = function (XL, params) {
  my.extendedColsTrain(XL, function(XL, newdata=NULL) {
    X = XL[, -ncol(XL)]
    Y = XL[, ncol(XL)]
    m = my.normalizedTrain(XL, function (XL, newdata=NULL) my.train.glm(XL, NULL) )
    Z = m(X)
    XL = cbind(X, Z, Y)
    
    model = my.normalizedTrain(XL, function (XL, newdata=NULL) my.train.et(XL, params))
    
    function (X) {
      model(cbind(X, m(X)))
    }
  }, neee)
}

etWithBin123TrainAlgo = function (XL, params, newdata=NULL) {
  bin123TrainAlgo(XL, params, newdata=newdata, trainAlgo=function (XL, params, newdata=NULL) {
    my.extendedColsTrain(XL, function(XL, newdata=NULL) {
      my.normalizedTrain(XL, function (XL, newdata=NULL) {
        my.train.et(XL, params, newdata=newdata)
      }, newdata=newdata)
    }, idxes=neee, pairs=nppp, x11bin=c(5,10), newdata=newdata)
  })
}

bin123TrainAlgo = function (XL, params, newdata=NULL, trainAlgo=NULL, use12=T, use23=T) {
  XL2 = XL
  XL2[, ncol(XL2)] = ifelse(XL2[, ncol(XL2)] <= 1, 0, 1)
  XL3 = XL
  XL3[, ncol(XL3)] = ifelse(XL3[, ncol(XL3)] <= 2, 0, 1)
  
  aa = trainAlgo(XL, params, newdata=newdata)
  
  if (use12) bb = trainAlgo(XL2, params, newdata=newdata)
  if (use23) cc = trainAlgo(XL3, params, newdata=newdata)
  
  function (X) {
    A = aa(X)
    
    if (use23) {
      C = cc(X)
      s2 = A[,3] + A[,4]
      A[,3] = C[,1] * s2
      A[,4] = C[,2] * s2
    }
    
    if (use12) {
      B = bb(X)
      s1 = A[,2] + A[,3]
      A[,2] = B[,1] * s1
      A[,3] = B[,2] * s1
    }
    
    A
  }
}