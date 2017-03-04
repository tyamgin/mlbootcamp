nnetTeachAlgo = function (XL, XK=NULL) {
  X = XL[, -ncol(XL)]
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
  
  number = 5
  trControl = trainControl(method='none', number=number, classProbs=T, summaryFunction=mnLogLoss, seeds=1:(number+2))

  tuneGrid = expand.grid(
    size = 5,
    decay = 0.15
  )
  
  capture.output(
    model <- train(X, Y, method='nnet', metric='logLoss', maxit=1000, 
                   maximize=F, trControl=trControl, verbose=F,
                   tuneGrid=tuneGrid)
  )
  mmm <<- model
  
  function (X) {
    predict(model, X, type='prob')$b
  }
}

nnetExTrainAlgo = function (XL) {
  my.extendedColsTrain(XL, function(XL) {
    my.normalizedTrain(XL, function (XL) {
      nnetTeachAlgo(XL)
    })
  })
}

nnetTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    nnetTeachAlgo(XL)
  })
}

nnetBootTrainAlgo = function (XL) {
  my.boot(XL, nnetTeachAlgo, meanAggregator, iters=200, rowsFactor=0.632)
}