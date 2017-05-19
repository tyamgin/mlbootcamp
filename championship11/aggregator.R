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

is.sorted = function (x) {
  !is.unsorted(x)
}

logitAggregator = function (XL, teachers, params, newdata=NULL) {
  if (length(teachers) != 2)
    stop('2 teachers required')
  
  lst = list()
  
  validation.tqfold.enumerate(function (XL, XK, it, fold) {
    Y = XL[, ncol(XL)]
    models = list()
    Y2 = unname(foreach(i=1:length(teachers), .combine=cbind) %do% {
      models[[i]] = teachers[[i]](XL, newdata=list(newdata, XK[, -ncol(XK)]))
      r = models[[i]](XK[, -ncol(XK)])
      colmat(r, Y + 1)
    })
    XL2 = XK
    for (i in 1:nrow(XL2)) {
      XL2[i, ncol(XL2)] = which.max(Y2[i, ]) - 1
    }
    
    idxes = rep(0, ncol(XL))
    for (i in intCols) idxes[i] = 1
    finalModel = my.extendedColsTrain(XL2, function (XL, newdata=NULL) { 
      glmTrainAlgo(XL) 
    }, idxes=idxes)
    
    
    lst[[it]] <<- function (X) {
      pp = finalModel(X)
      models[[1]](X) * pp[, 1] + models[[2]](X) * pp[, 2]
    }
  }, XL, folds=12, iters=params$iters)
  
  meanAggregator(lst)
}

etXgbTrainAlgo = function (XL, params, newdata) {
  logitAggregator(XL, c(
    function (XL, newdata=NULL) {
      etWithBin123TrainAlgo(XL, expand.grid(numRandomCuts=1, mtry=2, ntree=2000, nodesize=1, iters=1, rowsFactor=1, extra=F), newdata=newdata)
    },
    function (XL, newdata=NULL) {
      xgbWithBin123TrainAlgo(XL, xgbParams, newdata=newdata)
    }
  ), params, newdata=newdata)
}

etXgbMeanTrainAlgo = function (XL, params, newdata) {
  meanAggregator04(c(
    etWithBin123TrainAlgo(XL, expand.grid(numRandomCuts=1, mtry=3, ntree=2000, nodesize=1, iters=1, rowsFactor=1, extra=F), newdata=newdata),
    xgbWithBin123TrainAlgo(XL, xgbParams, newdata=newdata)
  ), c(params$p1, 1 - params$p1))
}



meanAggregator = function (baseAlgos, w=NULL) {
  l = length(baseAlgos)
  if (is.null(w))
    w = rep(1/l, l)
  else if (sum(w) != 1)
    stop('sum of weight\'s must be 1')
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

meanAggregator04 = function (baseAlgos, w=NULL) {
  l = length(baseAlgos)
  if (is.null(w))
    w = rep(1/l, l)
  else if (sum(w) != 1)
    stop('sum of weight\'s must be 1')
  
  if (length(baseAlgos) != 2)
    stop('baseAlgos length must be 2')
  
  function(X) {
    A = unnameMatrix(baseAlgos[[1]](X))
    B = unnameMatrix(baseAlgos[[2]](X))
    C = matrix(NA, nrow=nrow(A), ncol=ncol(A))
    for (i in 1:nrow(A)) {
      if ((which.max(A[i, ]) - 1) %in% c(0, 4)) {
        C[i, ] = A[i, ]
      } else {
        C[i, ] = w[1] * A[i, ] + w[2] * B[i, ]
      }
    }
    C
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