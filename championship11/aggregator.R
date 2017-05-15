glmTrainAlgo = function (XL, newdata) {
  XL = unnameMatrix(XL)
  my.normalizedTrain(XL, function (XL, newdata) {
    if (!is.null(newdata))
      stop('unsupported newdata')
    
    X = XL[, -ncol(XL)]
    colnames(X) <- paste0('X', 1:ncol(X))
    Y = factor(XL[, ncol(XL)], labels=c('a', 'b', 'c', 'd', 'e')[1:length(unique(XL[, ncol(XL)]))])
    
    tuneGrid = NULL # there is no tuning parameters
    trControl = trainControl(method='none', number=5, classProbs=T, summaryFunction=mnLogLoss)
    
    model <- train(X, Y, method='glm', metric='Accuracy', maximize=F, 
                   trControl=trControl, tuneGrid=tuneGrid, family=binomial)
    
    function (X) {
      X = unnameMatrix(X)
      colnames(X) <- paste0('X', 1:ncol(X))
      predict(model, X, type='prob')
    }
  })
}

colmat = function (X, idxes) {
  res = rep(NA, nrow(X))
  for (i in 1:nrow(X)) {
    res[i] = X[i, idxes[i]]
  }
  res
}

logitAggregator = function (XL, teachers, newdata=NULL) {
  if (length(teachers) != 2)
    stop('2 teachers required')
  
  models = list()
  
  validation.tqfold.enumerate(function (XL, XK, it, fold) {
    Y = XL[, ncol(XL)]
    Y2 = unname(foreach(teach=teachers, .combine=cbind) %do% {
      model = teach(XL, newdata=newdata)
      r = model(XK[, -ncol(XK)])
      colmat(r, Y + 1)
    })
    XL2 = XK
    for (i in 1:nrow(XL2))
      XL2[i, ncol(XL2)] = which.max(Y2[i, ]) - 1
    
    finalModel = glmTrainAlgo(XL2)
    
    models[it] = function (X) {
      pp = finalModel(X)
      models[[1]](X) * (1 - pp) + models[[2]](X) * pp
    }
  }, XL, folds=3, iters=1)
  
  meanAggregator(models)
}

etXgbTrainAlgo = function (XL, params.unused, newdata) {
  logitAggregator(XL, c(
    function (XL, newdata=NULL) {
      etWithBin123TrainAlgo(XL, expand.grid(numRandomCuts=1, mtry=2, ntree=2000, nodesize=1, iters=1, rowsFactor=1, extra=F), newdata=newdata)
    },
    function (XL, newdata=NULL) {
      xgbWithBin123TrainAlgo(XL, xgbParams, newdata=newdata)
    }
  ), newdata=newdata)
}
"
etXgbTrainAlgo = function (XL, params.unused, newdata) {
  logitAggregator(XL, c(
    etTrainAlgo(XL, expand.grid(numRandomCuts=1, mtry=2, ntree=2000, nodesize=1, iters=1, rowsFactor=1, extra=F), newdata=newdata),
    xgbTrainAlgo(XL, expand.grid(  iters=1,
                                   rowsFactor=1,
                                   
                                   max_depth=7, 
                                   gamma=0, 
                                   lambda=0.129457, 
                                   alpha=0.812294, 
                                   eta=0.03,
                                   colsample_bytree=0.630299,
                                   min_child_weight=3,
                                   subsample=0.8,
                                   nthread=4, 
                                   nrounds=c(800),
                                   early_stopping_rounds=0,
                                   num_parallel_tree=1), newdata=newdata)
  ))
}
"


meanAggregator = function (baseAlgos, w=NULL) {
  l = length(baseAlgos)
  if (is.null(w))
    w = rep(1/l, l)
  function(x) {
    s = 0
    for (i in 1:l) {
      if (is.function(baseAlgos[[i]])) {
        s = s + w[i] * baseAlgos[[i]](x)
      } else {
        s = s + w[i] * predict(baseAlgos[[i]], x)
      }
    }
    s
  }
}

gmeanAggregator = function (baseAlgos) {
  l = length(baseAlgos)
  function(x) {
    s = 1
    for (algo in baseAlgos) {
      if (is.function(algo)) {
        s = s * algo(x);
      } else {
        s = s * predict(algo, x);
      }
    }
    s ^ (1/l)
  }
}