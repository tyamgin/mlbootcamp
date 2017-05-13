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

nppp=c(1,1,1,1,0,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, #1
       0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)

my.train.knn = function (XL, params, newdata=NULL) {
  XL = unnameMatrix(XL)

  X = XL[, -ncol(XL)]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b', 'c', 'd', 'e')[1:length(unique(XL[, ncol(XL)]))])
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    k=params$k
  )
  
  model <- train(X, Y, method='knn', metric='Accuracy',
                 maximize=F, trControl=trControl,
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
}

knnTrainAlgo = function (XL, params, newdata=NULL) {
  idxes = rep(0, 223)
  for(i in intCols)
    idxes[i] = 1
  
  my.extendedColsTrain(XL, function(XL, newdata=NULL) {
    my.normalizedTrain(XL, function (XL, newdata=NULL) {
      my.train.knn(XL, params, newdata=newdata)
    }, newdata=newdata)
  }, idxes=idxes, pairs=F, angles=F, params$extra, newdata=newdata)
}

knnTrainRoundAlgo = function (XL, params, newdata=NULL) {
  knn = knnTrainAlgo(XL, params, newdata)
  function (X) {
    r = knn(X)
    ret = rep(-1, nrow(r))
    for (i in 1:length(ret)) {
      if (1 %in% r[i, ]) {
        ret[i] = which.max(r[i, ]) - 1
      }
    }
    ret
  }
}

knnEtTrainAlgo = function (XL, params, newdata=NULL) {
  knn = knnTrainAlgo(XL, params, newdata=newdata)
  et = etTrainAlgo(XL, params, newdata=newdata)
  
  function (X) {
    r = et(X)
    kr = knn(X)
    return(r)
    for (i in 1:nrow(r)) {
      if (1 %in% kr[i, ]) {
        r[i, ] = kr[i, ]
      }
    }
    r
  }
}

etTrainAlgo = function (XL, params, newdata=NULL) {
  my.extendedColsTrain(XL, function(XL, newdata=NULL) {
    my.normalizedTrain(XL, function (XL, newdata=NULL) {
      my.train.et(XL, params, newdata=newdata)
    }, newdata=newdata)
  }, idxes=neee, pairs=nppp, angles=F, params$extra, newdata=newdata)
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
  bin123TrainAlgo(XL, params, newdata=newdata,   trainAlgo=function (XL, params, newdata=NULL) {
    my.extendedColsTrain(XL, function(XL, newdata=NULL) {
      my.normalizedTrain(XL, function (XL, newdata=NULL) {
        my.train.et(XL, params, newdata=newdata)
      }, newdata=newdata)
    }, idxes=neee, pairs=nppp, extra=params$extra, newdata=newdata)
  })
}

bin123TrainAlgo = function (XL, params, newdata=NULL, trainAlgo=NULL) {
  XL2 = XL
  XL2[, ncol(XL2)] = ifelse(XL2[, ncol(XL2)] <= 1, 0, 1)
  XL3 = XL
  XL3[, ncol(XL3)] = ifelse(XL3[, ncol(XL3)] <= 2, 0, 1)
  
  aa = trainAlgo(XL, params, newdata=newdata)
  bb = trainAlgo(XL2, params, newdata=newdata)
  cc = trainAlgo(XL3, params, newdata=newdata)
  
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