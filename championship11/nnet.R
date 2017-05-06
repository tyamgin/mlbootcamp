my.train.et = function (XL, params, newdata=NULL) {
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
                   numThreads=4,
                   tuneGrid=tuneGrid)

    ret = function (X) {
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

my.train.nnet = function (XL, params) {
  X = XL[, -ncol(XL)]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b', 'c', 'd', 'e'))
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    size=params$size,
    decay=params$decay
  )
  
  capture.output(
    model <- train(X, Y, method='nnet', metric='Accuracy',
                   maximize=T, trControl=trControl, MaxNWts=100000, maxit=params$maxit,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    colnames(X) <- paste0('X', 1:ncol(X))
    predict(model, X, type='prob')
  }
}

"
my.train.nnet = function (XL, XK=NULL) {
  X = XL[, -ncol(XL)]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b', 'c', 'd', 'e'))
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    nrounds = 200,
    max_depth = 3,
    eta = 0.06,
    gamma = 5,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 0.9
  )
  
  capture.output(
    model <- train(X, Y, method='xgbTree', metric='Accuracy',
                   maximize=F, trControl=trControl,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    colnames(X) <- paste0('X', 1:ncol(X))
    predict(model, X, type='prob')
  }
}
"


my.train.glm = function (XL, params, newdata=NULL) {
  X = XL[, -ncol(XL)]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = XL[, ncol(XL)]
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = NULL
  
  capture.output(
    model <- train(X, Y, method='glm',
                   maximize=F, trControl=trControl,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    colnames(X) <- paste0('X', 1:ncol(X))
    a = predict(model, X)
    foreach(x=a, .combine=c) %do% {
      #foreach(y=0:4, .combine=c) %do% {
      #  max(0, min(1, 1 - abs(y - x)))
      #}
      r = ((0:4) - x)^2
      1 - r / sum(r)
    }
  }
}

my.train.svm = function (XL, XK=NULL) {
  X = XL[, -ncol(XL)]
  Y = XL[, ncol(XL)]
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    sigma = 1,
    C = 10
  )
  
  capture.output(
    model <- train(X, Y, method='svmRadial',
                   maximize=F, trControl=trControl,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    a = predict(model, X)
    foreach(x=a, .combine=c) %do% {
      r = ((0:4) - x)^2
      1 - r / sum(r)
    }
  }
}

neee=rep(0, 223)
for (i in c(139,  80,  12, 201, 183,  77, 132, 157,  97, 116,  98)) {
  neee[i] = 1
}
neee=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

etTrainAlgo = function (XL, params, newdata=NULL) {
  my.roundedTrain(XL, function (XL, newdata=NULL) {
    #my.checkedRangeTrain(XL, function (XL, newdata=NULL) {
      my.extendedColsTrain(XL, function(XL, newdata=NULL) {
        my.normalizedTrain(XL, function (XL, newdata=NULL) {
          my.train.et(XL, params, newdata=newdata)
        }, newdata=newdata)
      }, neee, params$extra, newdata=newdata)
    #}, newdata=newdata)
  }, newdata=newdata)
}

nnetTrainAlgo = function (XL, params) {
  my.roundedTrain(XL, function (XL) {
    my.extendedColsTrain(XL, function(XL) {
      my.normalizedTrain(XL, function (XL) {
        my.train.nnet(XL, params)
      })
    }, neee)
  })
}

glmTrainAlgo = function (XL, params, newdata=NULL) {
  my.roundedTrain(XL, function (XL, newdata=NULL) {
    my.extendedColsTrain(XL, function(XL, newdata=NULL) {
      my.normalizedTrain(XL, function (XL, newdata=NULL) {
        my.train.glm(XL, params, newdata=newdata)
      }, newdata=newdata)
    }, newdata=newdata)
  }, newdata=newdata)
}

etGlmTrainAlgo = function (XL, params) {
  my.roundedTrain(XL, function (XL, newdata=NULL) {
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
  })
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
    }, neee, extra=params$extra, newdata=newdata)
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
    
    my.roundAns(X, A)
  }
}

"
etWithBin12TrainAlgo = function (XL, params, newdata=NULL) {
  a = etTrainAlgo(XL, params, newdata)
  XL2 = XL
  XL2[, ncol(XL2)] = ifelse(XL2[, ncol(XL2)] <= 1, 0, 1)
  b = etTrainAlgo(XL2, params, newdata)
  function (X) {
    A = a(X)
    B = b(X)
    ifelse(A == 1 | A == 2, B + 1, A)
  }
}
"