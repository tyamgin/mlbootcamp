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

logitAggregator = function (XL, models) {
  if (length(models) != 2)
    stop('2 models required')
  
  Y = XL[, ncol(XL)]
  Y2 = unname(foreach(model=models, .combine=cbind) %do% {
    r = model(XL[, -ncol(XL)])
    r[, Y + 1]
  })
  XL2 = XL
  for (i in 1:nrow(XL2))
    XL2[i, ncol(XL2)] = which.max(Y2[i, ]) - 1
  
  finalModel = glmTrainAlgo(XL2)

  function (X) {
    pp = finalModel(X)
    models[[1]](X) * (1 - pp) + models[[2]](X) * pp
  }
}

etXgbTrainAlgo = function (XL, params.unused, newdata) {
  meanAggregator(c(
    etWithBin123TrainAlgo(XL, expand.grid(numRandomCuts=1, mtry=2, ntree=2000, nodesize=1, iters=1, rowsFactor=1, extra=F), newdata=newdata),
    xgbWithBin123TrainAlgo(XL, expand.grid(  iters=1,
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
  ), w=c(3/4,1/4))
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