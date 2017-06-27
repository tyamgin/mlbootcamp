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
                   maximize=T, trControl=trControl, maxit=params$maxit,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    colnames(X) <- paste0('X', 1:ncol(X))
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