my.train.nnet = function (XL, params, newdata=NULL) {
  X = XL[, -ncol(XL)]
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    size=params$size,
    decay=params$decay
  )
  
  capture.output(
    model <- train(X, Y, method='nnet', metric='logLoss',
                   maximize=F, trControl=trControl, maxit=params$maxit,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    predict(model, X, type='prob')$b
  }
}

nnetTrainAlgo = function (XL, params, newdata=NULL) {
  my.extendedColsTrain(XL, function (XL, newdata=NULL) {
    my.normalizedTrain(XL, function (XL, newdata=NULL) {
      my.train.nnet(XL, params, newdata)
    }, newdata=newdata)
  }, newdata=newdata)
}

nnetXgbTrainAlgo = function (XL, params, newdata=NULL) {
  meanAggregator(c(
    xgbTrainAlgo(XL, params, newdata),
    nnetTrainAlgo(XL, params, newdata)
  ))
}

my.train.et = function (XL, params, newdata=NULL) {
  X = XL[, -ncol(XL)]
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    mtry=params$mtry
    #numRandomCuts=params$numRandomCuts
  )
  
  capture.output(
    model <- train(X, Y, method='rf', metric='logLoss',
                   maximize=F, trControl=trControl, numThreads=4, 
                   ntree=params$ntree, nodesize=params$nodesize,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    predict(model, X, type='prob')$b
  }
}

etTrainAlgo = function (XL, params, newdata=NULL) {
  my.extendedColsTrain(XL, function (XL, newdata=NULL) {
    my.normalizedTrain(XL, function (XL, newdata=NULL) {
      my.train.et(XL, params, newdata)
    }, newdata=newdata)
  }, newdata=newdata)
}