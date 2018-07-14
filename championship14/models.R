debugSource('lgb.R')

my.train.lm = function (XL, params) {
  #model = MatrixModels:::lm.fit.sparse(XL[, -ncol(XL), drop=F], XL[, ncol(XL), drop=T])
  lambda = 0.04709416
  model = glmnet(XL[, -ncol(XL), drop=F], XL[, ncol(XL), drop=T],
                 family='binomial',type.logistic='Newton', type.multinomial='ungrouped', 
                 lambda=lambda, alpha=0)
  
  function (X) {
    #r=predict(model, X, type="response")
    #bestlam <- model$lambda.max
    r2=predict(model, X, type="response", s=lambda)
    r2[,1]
  }
}

my.train.lm2 = function (XL, params) {
  model = lm(target~., as.data.frame(XL))
  
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
    size=5,
    decay=100
  )
  
  
  capture.output(
    model <- train(X, Y, method='nnet', metric='ROC',
                   maximize=F, trControl=trControl,
                   maxit=200,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    colnames(X) <- paste0('X', 1:ncol(X))
    predict(model, X, type='prob')$b
  }
}

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
  mtry=c(2),
  ntree=c(250),
  nodesize=1,
  iters=1,
  rowsFactor=1
)
