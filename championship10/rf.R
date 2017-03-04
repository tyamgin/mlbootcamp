rfTeachAlgo = function (XL) {
  X = XL[, -ncol(XL)]
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
  
  
  "
  trControl = trainControl(method='cv', number=5, repeats=1, classProbs=T, summaryFunction=mnLogLoss)
  
  tuneGrid = expand.grid(
  mtry = 4
  )
  
  capture.output(
  model <- train(X, Y, method='rf', metric='logLoss',
  maximize=F, trControl=trControl, maxit=1000, nodesize=1, maxnodes=20, ntree=2000, importance=T,
  tuneGrid=tuneGrid)
  )
  
  rfm <<- model
  print(model)
  
  function (X) {
  predict(model, X, type='prob')$b
  }
  "
  
  model = randomForest(X, Y, nodesize=1, maxnodes=10, ntree=5000, mtry=2)
  #classwt=c(0.7, 0.3)
  
  function (x) {
    matrix(predict(model, x, type='prob'), ncol=2)[,2]
  }
  
}


rfTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    rfTeachAlgo(XL)
  })
}