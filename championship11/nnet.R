my.train.et = function (XL, params) {
  X = XL[, -ncol(XL)]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b', 'c', 'd', 'e')[1:length(unique(XL[, ncol(XL)]))])
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    numRandomCuts=params$numRandomCuts,
    mtry=params$mtry
  )
  
  capture.output(
    model <- train(X, Y, method='extraTrees', metric='Accuracy',
                   maximize=F, trControl=trControl,
                   ntree=params$ntree,
                   numThreads=4,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    colnames(X) <- paste0('X', 1:ncol(X))
    predict(model, X, type='prob')
  }
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


my.train.glm = function (XL, XK=NULL) {
  X = XL[, -ncol(XL)]
  Y = XL[, ncol(XL)]
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = NULL
  
  capture.output(
    model <- train(X, Y, method='glm',
                   maximize=F, trControl=trControl,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
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

neee=c(0,1,1,0,0,1,0,1,0,0,1,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
       0,0,0,0,0,1,1,0,0,0,1,1,0,0,1,0,1,0,0,0,0,1,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,1,1,1,0,1,0,0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,
       0,0,0,0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,1,1,0,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,0,1,
       0,1,0,0)

neee=c(0,1,1,0,0,1,0,1,0,0,1,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
       0,0,0,0,0,1,1,0,0,0,1,1,0,0,1,0,1,0,0,0,0,1,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,1,1,1,0,1,0,0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,
       0,0,0,0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,1,1,0,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,0,1,
       0,1,0,0)

neee=c(0,1,0,0,0,1,0,1,0,0,1,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
       0,0,0,0,0,1,1,0,0,0,1,1,0,0,1,0,1,0,0,0,0,1,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,1,1,1,0,1,0,0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,1,1,1,0,1,0,0,0,0,0,0,0,
       0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,0,1,
       0,1,0,0)

neee=c(0,1,0,0,0,1,0,1,0,0,1,1,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,
       0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,
       0,0,0,0,0,0,1,1,0,0,0,1,1,1,0,1,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,1,0,0,0,1,
       0,0,1,0,1,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,
       0,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,
       0,0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
       0,0,1,0,1,0,0)

neee=rep(0, 223)
for (i in c(139,  80,  12, 201, 183,  77, 132, 157,  97, 116,  98)) {
  neee[i] = 1
}

etTrainAlgo = function (XL, params) {
  my.roundedTrain(XL, function (XL) {
    my.extendedColsTrain(XL, function(XL) {
      my.normalizedTrain(XL, function (XL) {
        my.train.et(XL, params)
      })
    }, neee)
  })
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

glmTrainAlgo = function (XL) {
  my.roundedTrain(XL, function (XL) {
    my.normalizedTrain(XL, function (XL) {
      my.train.glm(XL)
    })
  })
}

etGlmTrainAlgo = function (XL) {
  my.roundedTrain(XL, function (XL) {
    my.extendedColsTrain(XL, function(XL) {
      #X = XL[, -ncol(XL)]
      #Y = XL[, ncol(XL)]
      #m = my.normalizedTrain(XL, my.train.glm)
      #Z = m(X)
      #XL = cbind(X, Z, Y)
      
      model = my.normalizedTrain(XL, my.train.nnet)
      
      function (X) {
        #model(cbind(X, m(X)))
        model(X)
      }
      
      #meanAggregator(c(
      #  my.normalizedTrain(XL, my.train.nnet)
        #my.normalizedTrain(XL, my.train.glm)
        #my.normalizedTrain(XL, my.train.svm)
      #))
    }, neee)
  })
}

etBtTrainAlgo = function (XL) {
  my.roundedTrain(XL, function (XL) {
    my.extendedColsTrain(XL, function(XL) {
      my.normalizedTrain(XL, function (XL) {
        my.boot(XL, function (XL, XK) {
          my.train.nnet(XL)
        }, meanAggregator, iters=5, rowsFactor=1, replace=F, nthread=1)
      })
    }, neee)
  })
}

nnetWithBin12TrainAlgo = function (XL) {
  a = nnetTrainAlgo(XL)
  XL2 = XL
  XL2[, ncol(XL2)] = ifelse(XL2[, ncol(XL2)] <= 1, 0, 1)
  b = nnetTrainAlgo(XL2)
  function (X) {
    A = a(X)
    B = b(X)
    ifelse(A == 1 | A == 2, B + 1, A)
  }
}

