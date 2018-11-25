debugSource('lgb.R')

my.train.lm = function (XL, params) {
  model = glm(Y ~ ., data=XL,
              family=binomial(link='logit'))
  
  function (X) {
    X = as.data.frame(X)
    r2=predict(model, type="response", newdata=X)
    as.vector(r2)
  }
}

my.train.lmr = function (XL, params) {
  model = lm.ridge(Y ~ ., data=XL)
  
  function (X) {
    X = as.data.frame(X)
    as.matrix(cbind(const=1, X)) %*% coef(model)
  }
}


my.train.lmr = function (XL, params) {
  nvars = ncol(XL) - 1
  model = glmnet(as.matrix(XL[,-ncol(XL)]), as.double(XL$Y), family='binomial', alpha=0, standardize=T,
                 nlambda=params$nlambda, lambda.min.ratio=params$lambda.min.ratio, thresh=params$thresh,
                 maxit=100000)
  
  function (X) {
    X = as.matrix(X)
    as.double(predict(model, X, type="response", s=params$s))
  }
}
lmrParams = list(
  s=c(1e-6),
  nlambda=c(2),
  lambda.min.ratio=c(1e-6),
  thresh=c(1e-7)
)



my.train.lm2 = function (XL, params) {
  model = lm(Y~., as.data.frame(XL))
  
  function (X) {
    predict(model, as.data.frame(X))
  }
}

my.train.glm = function (XL, params, newdata=NULL) {
  X = XL[, -ncol(XL), drop=F]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = factor(XL[, ncol(XL), drop=T], labels=c('a', 'b'))
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = NULL
  
  capture.output(
    model <- train(X, Y, method='glm',
                   maximize=F, trControl=trControl,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    colnames(X) <- paste0('X', 1:ncol(X))
    predict(model, X, type='prob')$b
  }
}

my.train.nnet = function (XL, params, newdata=NULL) {
  X = XL[, -ncol(XL), drop=F]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = factor(XL[, ncol(XL), drop=T], labels=c('a', 'b'))
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    size=params$size,
    decay=params$decay
  )
  
  
  capture.output(
    model <- train(X, Y, method='nnet', metric='ROC',
                   maximize=T, trControl=trControl,
                   maxit=params$maxit,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    colnames(X) <- paste0('X', 1:ncol(X))
    predict(model, X, type='prob')$b
  }
}

nnetParams = list(
  size = 7,
  decay = c(70),
  maxit = c(700)
)

my.train.knn = function (XL, params, newdata=NULL) {
  X = XL[, -ncol(XL), drop=F]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = factor(XL[, ncol(XL), drop=T], labels=c('a', 'b'))
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    kmax=7,
    distance=2,
    kernel='cos'
  )
  
  
  capture.output(
    model <- train(X, Y, method='kknn', metric='ROC',
                   maximize=F, trControl=trControl,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    colnames(X) <- paste0('X', 1:ncol(X))
    predict(model, X, type='prob')$b
  }
}

my.train.et = function (XL, params) {
  ret = my.boot(XL, function (XL, XK) {
    X = XL[, -ncol(XL), drop=F]
    colnames(X) <- paste0('X', 1:ncol(X))
    Y = factor(XL[, ncol(XL), drop=T], labels=c('a', 'b'))
    
    trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
    
    tuneGrid = expand.grid(
      numRandomCuts=params$numRandomCuts,
      mtry=params$mtry
    )
    
    model <- train(X, Y, method='extraTrees', metric='ROC',
                   maximize=T, trControl=trControl,
                   ntree=params$ntree,
                   nodesize=params$nodesize,
                   numThreads=4,
                   na.action='zero',
                   tuneGrid=tuneGrid)
    
    function (X) {
      colnames(X) <- paste0('X', 1:ncol(X))
      predict(model, X, type='prob')$b
    }
  }, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
  
  ret
}

etParams = list(
  numRandomCuts=c(1),
  mtry=c(3),
  ntree=c(500),
  nodesize=1,
  iters=1,
  rowsFactor=1
)
