my.train.nnet = function (XL, XK=NULL) {
  X = XL[, -ncol(XL)]
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b', 'c', 'd', 'e'))
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary) #TODO: не понятно как указать метрику
  
  tuneGrid = expand.grid(
    #size = 15,
    #decay = 0.01 
    mtry = floor(ncol(XL)/4),
    numRandomCuts=3
  )
  
  capture.output(
    model <- train(X, Y, method='extraTrees', metric='Accuracy',
                   #maxit=1000, MaxNWts=10000, verbose=F,
                   maximize=F, trControl=trControl,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    predict(model, X, type='prob')
  }
}

nnetTrainAlgo = function (XL) {
  my.roundedTrain(XL, function (XL) {
    my.extendedColsTrain(XL, function(XL) {
      my.normalizedTrain(XL, function (XL) {
        my.train.nnet(XL)
      })
    }, c(1,0,1,1,1,1,
         1,0,1,0,0,1,
         1,0,0,0,0,0,
         1,0,1,0,0,1,
         0,0,0,0,1,1,
         1,1,0,0,1,1,
         0,0,1,0,0,1,
         0,0,1,1,0,1,
         0,1,0,1,0,0,
         0,1,0,0,1,1,
         0,0,0,1,0,0,
         0,0,1,0,1,0,
         0,0,1,1,0,1,
         0,0,1,0,0,0,
         0,0,0,1,1,1,
         0,0,0,0,0,1,
         1,0,0,0,0,0,
         0,1,1,1,0,1,
         0,0,0,1,1,0,
         0,1,1,1,1,0,
         1,1,0,1,0,0,
         0,1,1,1,1,0,
         0,0,1,1,0,1,
         1,0,0,1,0,0,
         0,0,1,0,0,0,
         1,0,1,0,0,0,
         1,0,1,0,1,0,
         0,1,0,0,0,0,
         0,1,0,0,0,0,
         0,1,0,1,0,0,
         0,1,1,1,1,0,
         0,0,1,1,1,0,
         0,0,0,0,0,0,
         1,1,1,1,0,0,
         0,0,0,0,0,1,
         0,0,0,0,1,1,
         1,1,0,0,0,1,
         0))
  })
}

