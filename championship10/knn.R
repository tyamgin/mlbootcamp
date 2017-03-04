knnTeachAlgo = function (XL) {
  X = XL[, -ncol(XL)]
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
  
  trControl = trainControl(method='none', number=5, repeats=1, classProbs=T, summaryFunction=mnLogLoss)
  
  tuneGrid = expand.grid(
    kmax = 199,
    distance=3,
    kernel=c("optimal")
  )
  
  capture.output(
    model <- train(X, Y, method='kknn', metric='logLoss',
                   maximize=F, trControl=trControl,
                   tuneGrid=tuneGrid)
  )
  
  knnm <<- model
  #print(model)
  
  function (X) {
    predict(model, X, type='prob')$b
  }
}

knnTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    knnTeachAlgo(XL)
  })
}