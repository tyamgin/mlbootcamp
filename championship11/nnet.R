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

