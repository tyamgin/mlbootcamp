set.seed(2708)
require(kernlab)
require(scatterplot3d)
require(xgboost)
require(class) 
require(e1071) 
require(RSNNS)
require(caret)
require(lightgbm)
require(nnet)
require(doParallel)
require(randomForest)

debugSource("algos.R")
debugSource("genetic.R")
debugSource("rfe.R")
debugSource("stacking.R")

XX = read.csv(file="x_train.csv", head=T, sep=";", na.strings="?")
YY = read.csv(file="y_train.csv", head=F, sep=";", na.strings="?")

extendCols = function (XX) {
  sz = ncol(XX)
  for (j in 2:sz) {
    for (k in 1:(j-1)) {
      num = XX[, j]
      denum = XX[, k]
      
      XX = cbind(XX, num / (denum + ifelse(min(denum) == 0, 1, 0)))
      XX = cbind(XX, num * denum)
    }
  }
  
  # "tqfold 0 iterations remains, mean=0.379375133577985 sd=0.00884458981072915"
  #XX = XX[, which(1 == c(0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0,1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0,0, 0, 0, 0, 1, 0, 0,1, 1, 1, 0, 1, 1, 1,1, 1, 0, 1, 0, 0, 1,0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0))]
  #XX = XX[, which(1 == c(1, 1, 1, 1, 0, 0, 1, 1, 1,1, 0, 0, 1, 0, 0, 1, 0, 1,1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1))]
  
  #last "tqfold 0 iterations remains, mean=0.379544967115814 sd=0.00888422118808115"
  "XX = XX[, which(1 == c(1,                 1,                 0,                 1,                 1,                
  1,                 1,                 0,                 1,                 1,                
  1,                 1,                 0,                 0,                 0,                
  1,                 0,                 0,                 0,                 0,                
  0,                 1,                 0,                 0,                 1,                
  0,                 0,                 0,                 0,                 0,                
  0,                 0,                 0,                 1,                 1,                
  0,                 0,                 1,                 1,                 0,                
  0,                 1,                 0,                 1,                 0,                
  1,                 1,                 0,                 0,                 0,                
  0,                 1,                 0,                 1,                 1,                
  0,                 1,                 0,                 1,                 1,                
  0,                 0,                 1,                 1,                 0,                
  0,                 1,                 0,                 1,                 0,                
  1,                 0,                 0,                 0,                 1,                
  0,                 1,                 1,                 1,                 1,                
  1,                 0,                 1,                 1,                 1,                
  1,                 0,                 1,                 0,                 1,                
  0,                 0,                 1,                 1,                 1,                
  0,                 1,                 0,                 1,                 0,                
  0,                 1,                 0,                 1,                 0,                
  0,                 1,                 1,                 1,                 0,                
  1,                 1,                 0,                 0,                 0,                
  0,                 0,                 0,                 0,                 0,                
  0,                 0,                 0,                 1,                 1,                
  1,                 0,                 0,                 0,                 0,                
  0,                 0,                 1,                 1,                 1,                
  0,                 1,                 0,                 0,                 1,                
  1,                 1,                 1,                 0))]"
  
  # pre last "tqfold 0 iterations remains, mean=0.379438947268411 sd=0.00877389849010951"
  "
  XX = XX[, which(1 == c(1,                 1,                 0,                 0,                 0,                
  0,                 0,                 0,                 1,                 1,                
  0,                 0,                 0,                 1,                 0,                
  1,                 1,                 1,                 1,                 1,                
  1,                 1,                 1,                 0,                 1,                
  0,                 0,                 1,                 1,                 1,                
  0,                 1,                 1,                 0,                 1,                
  0,                 1,                 0,                 1,                 0,                
  0,                 0,                 1,                 1,                 0,                
  1,                 1,                 0,                 1,                 0,                
  1,                 0,                 1,                 0,                 0,                
  0,                 0,                 0,                 0,                 1,                
  1,                 0,                 0,                 1,                 0,                
  1,                 1,                 0,                 0,                 0,                
  0,                 0,                 0,                 1,                 1,                
  0,                 0,                 0,                 1,                 0,                
  1,                 0,                 1,                 1,                 1,                
  1,                 0,                 0,                 1,                 1,                
  0,                 0,                 1,                 0,                 0,                
  0,                 0,                 0,                 1,                 0,                
  0,                 1,                 1,                 0,                 1,                
  1,                 0,                 0,                 1,                 1,                
  0,                 1,                 1,                 1,                 0,                
  1,                 0,                 0,                 0,                 0,                
  0,                 0,                 0,                 0,                 1,                
  0,                 1,                 0,                 0,                 0,                
  1,                 0,                 1,                 0,                 1,                
  0,                 0,                 0,                 1,                 0,                
  1,                 0,                 0,                 0))]
  "
  
  #super last "tqfold 0 iterations remains, mean=0.379215446151919 sd=0.00882421181974091"
  
  #XX = XX[, which(1 == c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1))]
  
  #lll
  XX = XX[, which(1 == c(0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0))]
  
  XX
}
insertXLCol = function (XL, Z) {
  X = XL[, -ncol(XL), drop=F]
  Y = XL[, ncol(XL), drop=F]
  cbind(X, Z, Y)
}
extendXYCols = function (XL) {
  X = XL[, -ncol(XL), drop=F]
  Y = XL[, ncol(XL), drop=F]
  X = extendCols(X)
  cbind(X, Y)
}

preCols = function (XX) {
  as.matrix(unname(data.matrix(XX)))
}

XX = preCols(XX)

XLL = as.matrix(unname(cbind(data.matrix(XX), YY)))


my.extendedColsTrain = function (XL, trainFunc) {
  XL = extendXYCols(XL)
  model = trainFunc(XL)
  function (X) {
    X = extendCols(X)
    model(X)
  }
}
my.normalizedTrain = function (XL, trainFunc) {
  m = ncol(XL) - 1
  means = rep(NA, m)
  sds = rep(NA, m)
  for (j in 1:m) {
    means[j] = mean(XL[, j])
    sds[j] = sd(XL[, j])
    XL[, j] = (XL[, j] - means[j]) / sds[j]
  }
  model = trainFunc(XL)
  function (X) {
    for (j in 1:m)
      X[, j] = (X[, j] - means[j]) / sds[j]
    model(X)
  }
}
my.train.xgb = function (XLL, iters=10, rowsFactor=0.3, aggregator=meanAggregator) {
  algos = list()
  n = nrow(XLL)
  for (it in 1:iters) {
    sampleIdxes = sample(n, rowsFactor*n)
    
    XK = XLL[-sampleIdxes, ]
    XL = XLL[sampleIdxes, ]  
    
    dtrain = xgb.DMatrix(data=XL[, -ncol(XL)], label=XL[, ncol(XL)])
    dtest = xgb.DMatrix(data=XK[, -ncol(XK)], label=XK[, ncol(XK)])
    watchlist = list(train=dtrain, test=dtest)
    
    algos[[it]] = xgb.train(
      data=dtrain, watchlist=watchlist, max_depth=3, gamma=5, eta=0.06, 
      nthread=8, nrounds=3000, eval_metric='logloss', objective='binary:logistic',
      early_stopping_rounds=50, verbose=0
    )
  }
  aggregator(algos)
}
my.train.lgb = function (XLL, iters=10, rowsFactor=0.3, aggregator=meanAggregator) {
  algos = list()
  n = nrow(XLL)
  for (it in 1:iters) {
    sampleIdxes = sample(n, rowsFactor*n)
    
    XK = XLL[-sampleIdxes, ]
    XL = XLL[sampleIdxes, ]  
    
    dtrain = lgb.Dataset(data=XL[, -ncol(XL)], label=XL[, ncol(XL)], free_raw_data=FALSE)
    dtest = lgb.Dataset(data=XK[, -ncol(XK)], label=XK[, ncol(XK)], free_raw_data=FALSE)
    valids = list(train=dtrain, test=dtest)
    
    algos[[it]] = lgb.train(
      data=dtrain, num_leaves=7, max_depth=3, learning_rate=0.06,
      nrounds=1000, valids=valids, 
      eval=c('binary_logloss'), objective = 'binary',
      nthread=4, verbose=0, early_stopping_rounds=50,
      min_data_in_leaf=100, lambda_l2=5
    )
    if (it %% 100 == 0)
      gc()
  }
  aggregator(algos)
}

mlpTeachAlgo = function (X, Y) {
  Y = factor(Y, labels=c('a', 'b'))
  
  trControl = trainControl(method='cv', number=5, repeats=1, classProbs=T, summaryFunction=mnLogLoss)
  
  tuneGrid = expand.grid(
    size = 5:7
  )
  
  capture.output(
    model <- train(X, Y, method='mlp', metric='logLoss', maxit=2000, 
                   maximize=F, trControl=trControl, verbose=F,
                   tuneGrid=tuneGrid)
  )
  
  mmm <<- model
  print(model)
  
  function (X) {
    predict(model, X, type='prob')$b
  }
}
nnetTeachAlgo = function (X, Y) {
  Y = factor(Y, labels=c('a', 'b'))
  
  number = 5
  trControl = trainControl(method='boot632', number=number, classProbs=T, summaryFunction=mnLogLoss, seeds=1:(number+2))
  
  #for(sz in 4:5) {
  #  for(dec in c(0.05, 0.10, 0.15, 0.20)) {
  sz=5
  dec=0.15
      tuneGrid = expand.grid(
        size = sz,
        decay = dec
      )
      
      capture.output(
        model <- train(X, Y, method='nnet', metric='logLoss', maxit=1000, 
                       maximize=F, trControl=trControl, verbose=F,
                       tuneGrid=tuneGrid)
      )
      
      mmm <<- model
      print(model)
  #  }
  #}
  
  function (X) {
    predict(model, X, type='prob')$b
  }
}
knnTeachAlgo = function (X, Y) {
  Y = factor(Y, labels=c('a', 'b'))
  
  trControl = trainControl(method='cv', number=5, repeats=1, classProbs=T, summaryFunction=mnLogLoss)
  
  tuneGrid = expand.grid(
    kmax = 199,
    distance=3,
    kernel=c("optimal")
  )
  
  capture.output(
    model <- train(X, Y, method='kknn', metric='logLoss',
                   maximize=F, trControl=trControl,
                   tuneGrid=tuneGrid)
  )
  
  knnm <<- model
  print(model)
  
  function (X) {
    predict(model, X, type='prob')$b
  }
}
rfTeachAlgo = function (X, Y) {
  Y = factor(Y, labels=c('a', 'b'))

  
  "
  trControl = trainControl(method='cv', number=5, repeats=1, classProbs=T, summaryFunction=mnLogLoss)
  
  tuneGrid = expand.grid(
    mtry = 4
  )
  
  capture.output(
    model <- train(X, Y, method='rf', metric='logLoss',
                   maximize=F, trControl=trControl, maxit=1000, nodesize=1, maxnodes=20, ntree=2000, importance=T,
                   tuneGrid=tuneGrid)
  )
  
  rfm <<- model
  print(model)

  function (X) {
    predict(model, X, type='prob')$b
  }
"
  
  model = randomForest(X, Y, nodesize=1, maxnodes=10, ntree=5000, mtry=2)
  #classwt=c(0.7, 0.3)
  
  function (x) {
    matrix(predict(model, x, type='prob'), ncol=2)[,2]
  }

}

svmTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    
    X = XL[, -ncol(XL)]
    Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
    
    tuneGrid = expand.grid(
      weight=4,#c(5, 10), 
      cost=0.5#c(0.5, 1, 2, 5)
    )
    trControl = trainControl(method='cv', number=5, classProbs=T, summaryFunction=mnLogLoss)
    capture.output(
      model <- train(X, Y, method='svmLinearWeights', metric='logLoss', maximize=F, 
                     trControl=trControl, tuneGrid=tuneGrid,
                     verbose=F)
    )
    
    svmModel1 <<- model
    print(model)
    
    function (X) {
      predict(model, X, type='prob')$b
    }
  })
}

nnetTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    nnetTeachAlgo(XL[, -ncol(XL)], XL[, ncol(XL)])
  })
}

mlpTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    mlpTeachAlgo(XL[, -ncol(XL)], XL[, ncol(XL)])
  })
}

knnTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    knnTeachAlgo(XL[, -ncol(XL)], XL[, ncol(XL)])
  })
}

rfTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    rfTeachAlgo(XL[, -ncol(XL)], XL[, ncol(XL)])
  })
}


xgbTrainAlgo = function (XL) {
  my.extendedColsTrain(XL, function(XL) {
    my.normalizedTrain(XL, function (XL) {
      my.train.xgb(XL, rowsFactor=0.9, iters=15)
    })
  })
}

lgbTrainAlgo = function (XL) {
  my.extendedColsTrain(XL, function(XL) {
    my.normalizedTrain(XL, function (XL) {
      my.train.lgb(XL, rowsFactor=0.9, iters=200)
    })
  })
}


stackedAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    my.stacking(XL[, -ncol(XL)], XL[, ncol(XL)], c(svmTrainAlgo), lgbTrainAlgo)#nnetTrainAlgo, 
  })
}

#cl <- makeCluster(detectCores())
#registerDoParallel(cl)

#set.seed(2707); print(validation.tqfold(XLL, stackedAlgo, folds=3, iters=10, verbose=T))
#set.seed(2709);print(geneticSelect(iterations=200, XL=extendXYCols(XLL), teach=function (XL) {
#  my.normalizedTrain(XL, function (XL) {
#    my.train.lgb(XL, rowsFactor=0.9, iters=4)
#  })
#}, maxPopulationSize=13, mutationProb=0.1, startOnesProbab=0.15))
#my.rfe(XLL)


set.seed(2708);algb = lgbTrainAlgo(XLL)
#set.seed(2708);a2 = xgbTrainAlgo(XLL)
#set.seed(2707);annet = nnetTrainAlgo(XLL)
#set.seed(2708);asvm = svmTrainAlgo(XLL)
#set.seed(2708);a5 = mlpTrainAlgo(XLL)
#set.seed(2708);aknn = knnTrainAlgo(XLL)
#set.seed(2708);arf = rfTrainAlgo(XLL)

#rs = resamples(list(svm=svmModel1, nnet=mmm, knn=knnm))
#modelCor(rs)

#stopCluster(cl)



alg = meanAggregator(c(algb))
XXX = read.csv(file='x_test.csv', head=T, sep=';', na.strings='?')
XXX = preCols(XXX)
results = alg(XXX)
write(results, file='res.txt', sep='\n')
print('done')
