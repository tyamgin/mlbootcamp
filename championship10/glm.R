glmTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    X = XL[, -ncol(XL)]
    Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
    
    tuneGrid = NULL # there is no tuning parameters
    trControl = trainControl(method='none', number=5, classProbs=T, summaryFunction=mnLogLoss)
    
    model <- train(X, Y, method='glm', metric='logLoss', maximize=F, 
                   trControl=trControl, tuneGrid=tuneGrid, family=binomial)
    
    function (X) {
      predict(model, X, type='prob')$b
    }
  })
}