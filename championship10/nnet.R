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

nnetBootEliteTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    my.boot(XL, function (XL, XK) {
      selAlgo = NULL
      minError = 1e10
      for (i in 1:3) {
        
        ##### copy-paste
        asd = function () {
          X = XL[, -ncol(XL)]
          Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
  
          capture.output(
            model <- train(X, Y, method='nnet', metric='logLoss', maxit=1000, maximize=F, verbose=F,
                           trControl=trainControl(method='none', classProbs=T, summaryFunction=mnLogLoss), 
                           tuneGrid=expand.grid(size = 5, decay = 0.01)
                           )
          )
          function (X) {
            predict(model, X, type='prob')$b
          }
        }
        algo = asd()
        
        error.logloss = function (act, pred) {
          if (length(act) != length(pred)) {
            stop("length's must be equal")
          }
          eps = 1e-15
          pred = pmin(pmax(pred, eps), 1 - eps)
          sum(act * log(pred) + (1 - act) * log(1 - pred)) * -1/NROW(act)
        }
        #####
        
        err = error.logloss(XK[, ncol(XK)], algo(XK[, -ncol(XK)]))
        
        if (err < minError) {
          minError = err
          selAlgo = algo
        }
      }
      selAlgo
    }, meanAggregator, iters=200, rowsFactor=0.8, nthread=4)
  })
}

nnetBootTrainAlgo = function (XL) {
  #my.extendedColsTrain(XL, function(XL) {
    my.normalizedTrain(XL, function (XL) {
      my.boot(XL, nnetTeachAlgo, meanAggregator, iters=200, rowsFactor=0.8)
    })
  #}, c(T,  T, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F))
  #})
}