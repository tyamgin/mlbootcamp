nnetTeachAlgo = function (XL, XK=NULL) {
  X = XL[, -ncol(XL)]
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=mnLogLoss)

  tuneGrid = expand.grid(
    size = 5,
    decay = 0.01
  )
  
  capture.output(
    model <- train(X, Y, method='nnet', metric='logLoss', maxit=1000,
                   maximize=F, trControl=trControl, verbose=F,
                   tuneGrid=tuneGrid)
  )
  
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

load("res/annetmagic.RData")

nnetMagicTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    X = XL[, -ncol(XL)]
    Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))

    trControl = trainControl(method='cv', number=5, classProbs=T, summaryFunction=mnLogLoss)
    
    capture.output(
      model <- train(X, Y, method='nnet', metric='logLoss', maxit=1000,
                     maximize=F, trControl=trControl, verbose=F)
    )
    
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
      for (i in 1:20) {
        
        ##### копипаст, почему-то не работает с dopar
        getAlgo = function () {
          X = XL[, -ncol(XL)]
          Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
  
          capture.output(
            model <- train(X, Y, method='nnet', metric='logLoss', maxit=1000, maximize=F, verbose=F,
                           trControl=trainControl(method='none', classProbs=T, summaryFunction=mnLogLoss), 
                           tuneGrid=expand.grid(size=5, decay=0.01)
                           )
          )
          function (X) {
            predict(model, X, type='prob')$b
          }
        }
        algo = getAlgo()
        
        error.logloss = function (act, pred) {
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
    }, meanAggregator, iters=200, rowsFactor=0.9, nthread=4)
  })
}

nnetBootTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    my.boot(XL, nnetTeachAlgo, meanAggregator, iters=200, rowsFactor=0.8)
  })
}