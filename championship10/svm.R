svmTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    X = XL[, -ncol(XL)]
    Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
    
    tuneGrid = expand.grid(
      weight=4,#c(5, 10), 
      cost=0.5#c(0.5, 1, 2, 5)
    )
    trControl = trainControl(method='none', number=5, classProbs=T, summaryFunction=mnLogLoss)
    capture.output(
      model <- train(X, Y, method='svmLinearWeights', metric='logLoss', maximize=F, 
                     trControl=trControl, tuneGrid=tuneGrid,
                     verbose=F)
    )
    
    svmModel1 <<- model
    #print(model)
    
    function (X) {
      predict(model, X, type='prob')$b
    }
  })
}