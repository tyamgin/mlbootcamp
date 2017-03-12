nnetTeachAlgo = function (XL, XK=NULL) {
  X = XL[, -ncol(XL)]
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
  
  #XX = XK[, -ncol(XK)]
  #YY = factor(XK[, ncol(XK)], labels=c('a', 'b'))
  
  number = 5
  trControl = trainControl(method='none', number=number, classProbs=T, summaryFunction=mnLogLoss)

  tuneGrid = expand.grid(
    size = 5,
    decay = 0.01
  )
  
  capture.output(
    #model <- train(X, Y, method='mlp', metric='logLoss', maxit=500, #abstol=1e-3, softmax=T,
    #               maximize=F, trControl=trControl, verbose=F, learnFuncParams=c(0.2, 0.1),
    #               #inputsTest=XX, targetsTest=YY,
    #               tuneGrid=tuneGrid)
    
    model <- train(X, Y, method='nnet', metric='logLoss', maxit=1000,
                   maximize=F, trControl=trControl, verbose=F,
                   tuneGrid=tuneGrid)
  )
  mmm <<- model
  
  function (X) {
    predict(model, X, type='prob')$b
  }
}

nnetTrainAlgo = function (XL) {
  my.extendedColsTrain(XL, function(XL) {
    my.normalizedTrain(XL, function (XL) {
      nnetTeachAlgo(XL)
    })
  }, NULL)
}

nnetMagicTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    X = XL[, -ncol(XL)]
    Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))

    trControl = trainControl(method='cv', number=5, classProbs=T, summaryFunction=mnLogLoss)
    
    capture.output(
      model <- train(X, Y, method='nnet', metric='logLoss', maxit=1000,
                     maximize=F, trControl=trControl, verbose=F)
    )
    mmm <<- model
    print(model)
    
    function (X) {
      predict(model, X, type='prob')$b
    }
  })
}

nnetBootTrainAlgo = function (XL) {
  my.extendedColsTrain(XL, function(XL) {
    my.normalizedTrain(XL, function (XL) {
      my.boot(XL, nnetTeachAlgo, meanAggregator, iters=25, rowsFactor=0.632)
    })
  #}, c(T,  T, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F))
  })
}