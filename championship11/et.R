my.train.et = function (XL, params, newdata=NULL) {
  XL = unnameMatrix(XL)
  my.boot(XL, function (XL, XK) {
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
                   tuneGrid=tuneGrid)
    
    ret = function (X) {
      X = unnameMatrix(X)
      colnames(X) <- paste0('X', 1:ncol(X))
      predict(model, X, type='prob')
    }
    
    if (!is.null(newdata)) {
      ret = ret(newdata)
      rm(model)
      return( function (X) ret )
    }
    
    ret
  }, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
}


intCols = c(139,  80,  12, 201, 183,  77, 132, 157,  97, 116,  98)

neee=rep(0, 223)
for (i in intCols) {
  neee[i] = 1
}
neee=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

nppp=c(1,1,1,1,0,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
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
  }, neee, params$extra, newdata=newdata)
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


etWithBin12TrainAlgo = function (XL, params, newdata=NULL) {
  XL2 = XL
  XL2[, ncol(XL2)] = ifelse(XL2[, ncol(XL2)] <= 1, 0, 1)
  XL3 = XL
  XL3[, ncol(XL3)] = ifelse(XL3[, ncol(XL3)] <= 2, 0, 1)
  
  f = function (XL) {
    my.extendedColsTrain(XL, function(XL, newdata=NULL) {
      my.normalizedTrain(XL, function (XL, newdata=NULL) {
        my.train.et(XL, params, newdata=newdata)
      }, newdata=newdata)
    }, idxes=neee, pairs=nppp, extra=params$extra, newdata=newdata)
  }
  
  aa = f(XL)
  bb = f(XL2)
  cc = f(XL3)
  
  function (X) {
    A = aa(X)
    B = bb(X)
    C = cc(X)
    
    s2 = A[,3] + A[,4]
    A[,3] = C[,1] * s2
    A[,4] = C[,2] * s2
    
    s1 = A[,2] + A[,3]
    A[,2] = B[,1] * s1
    A[,3] = B[,2] * s1
    
    A
  }
}