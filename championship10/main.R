set.seed(2707)
require(kernlab)
require(scatterplot3d)
require(xgboost)
require(class) 
require(e1071) 
require(RSNNS)
require(caret)
require(lightgbm)

debugSource("algos.R")
debugSource("genetic.R")
debugSource("rfe.R")

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
  
  XX = XX[, which(1 == c(0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0,1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0,0, 0, 0, 0, 1, 0, 0,1, 1, 1, 0, 1, 1, 1,1, 1, 0, 1, 0, 0, 1,0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0))]
  XX = XX[, which(1 == c(1, 1, 1, 1, 0, 0, 1, 1, 1,1, 0, 0, 1, 0, 0, 1, 0, 1,1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1))]
  
  #XX = XX[, c(130, 124, 132, 20,  32,  12,  133, 49,  101, 131, 75,  91,  43,  21,  62,  109, 135, 4,   139, 129, 119, 64, 100, 25,  81,  126, 137, 27,  105, 69,  23,  1,   121)]
  
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


"
pc = princomp(XX)
XX = XX %*% solve(t(pc$loadings))
XX = XX[, 1:20]
"

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
    
    dtrain <- xgb.DMatrix(data=XL[, -ncol(XL)], label=XL[, ncol(XL)])
    dtest <- xgb.DMatrix(data=XK[, -ncol(XK)], label=XK[, ncol(XK)])
    watchlist <- list(train=dtrain, test=dtest)
    
    algos[[it]] = local({
      bstSparse <- xgb.train(data=dtrain, watchlist=watchlist, max_depth=3, eta=0.06, nthread=8, nrounds=3000, eval_metric='logloss', objective='binary:logistic', early_stopping_rounds=50, verbose=0)
      function (X) {
        predict(bstSparse, X)
      }
    })
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
    
    dtrain <- lgb.Dataset(data=XL[, -ncol(XL)], label=XL[, ncol(XL)], free_raw_data=FALSE)
    dtest <- lgb.Dataset(data=XK[, -ncol(XK)], label=XK[, ncol(XK)], free_raw_data=FALSE)
    valids <- list(train=dtrain, test=dtest)
    
    algos[[it]] = local({
      bst <- lgb.train(data=dtrain, num_leaves=9, max_depth=3, learning_rate=0.06, nrounds=1000, valids=valids,
                       eval = c('binary_logloss'), objective = 'binary',
                       nthread = 4, verbose=0, early_stopping_rounds=50,
                       min_data_in_leaf=100, lambda_l2=5)
      
      function (X) {
        predict(bst, X)
      }
    })
  }
  aggregator(algos)
}

XLL = as.matrix(unname(cbind(data.matrix(XX), YY)))

#clr = ifelse(XLL[,ncol(XLL)]==0,"red","green")
#pairs(XLL[,1:3], col=clr)


"
xxx = XLL[, -ncol(XLL),drop=F]
aaa = XLL[, ncol(XLL)]
aaa <- factor(aaa, labels=c('a', 'b'))

xxx2 = XLL[10001:15000, -ncol(XLL),drop=F]
aaa2 = XLL[10001:15000, ncol(XLL)]
aaa2 <- factor(aaa2, labels=c('a', 'b'))

trControl=trainControl(method='cv', number=5, classProbs=TRUE, summaryFunction=mnLogLoss)
tuGrid = expand.grid(
  nrounds = 2000, 
  max_depth = c(2, 3, 4, 5), 
  eta = 0.06, 
  gamma = c(0.5, 1, 2, 3), 
  colsample_bytree = c(0.9, 0.95, 1.0), 
  min_child_weight = c(0.5, 1, 1.5, 2, 2.5, 3, 2.2, 0.2),
  subsample = 1
)
model <- train(xxx, aaa, method='xgbTree', metric='logLoss', maximize=F, trControl=trControl, tuneGrid=tuGrid, verbose=T)

pr = predict(model, xxx, type='prob')$b
print( error.logloss(c(aaa)-1, pr) )

pr2 = predict(model, xxx2, type='prob')$b
print( error.logloss(c(aaa2)-1, pr2) )


ctrl <- rfeControl(functions=caretFuncs, method='cv', repeats=5, verbose=F)
colnames(xxx) <- paste('col', 1:12, sep='')
lmProfile <- rfe(xxx, aaa, sizes=c(1:5, 10), rfeControl=ctrl, method='nnet')
"

mlpTeachAlgo = function (X, Y) {
  Y = factor(Y, levels=c("0", "1"))
  model = train(X, Y, method='mlp', maxit=30)
  
  function (X) {
    predict(model, X, type='prob')$`1`
  }
}
nnetTeachAlgo = function (X, Y) {
  Y = factor(Y, labels=c('a', 'b'))

  trControl = trainControl(method='cv', number=5, classProbs=T, summaryFunction=mnLogLoss)
  capture.output(model <- train(X, Y, method='nnet', metric='logLoss', maxit=1000, maximize=F, trControl=trControl, verbose=F))
  
  function (X) {
    predict(model, X, type='prob')$b
  }
}

teachAlgo = function (XL) {
  X = XL[, -ncol(XL)]
  Y = XL[, ncol(XL)]
  mlpAlg = mlpTeachAlgo(X, Y)
  XL = cbind(X, mlpAlg(X), Y)

  model = my.teach(XL, rowsFactor=0.6, iters=15, colsFactor=1)
  
  function (X) {
    X = cbind(X, mlpAlg(X))
    model(X)
  }
}
teachAlgo = function (XL) {
  X = XL[, -ncol(XL)]
  Y = XL[, ncol(XL)]
  svmAlg = svmTeachAlgo(XL)
  XL = cbind(X, svmAlg(X), Y)
  
  model = my.teach(XL, rowsFactor=0.6, iters=2, colsFactor=1)
  
  function (X) {
    X = cbind(X, svmAlg(X))
    model(X)
  }
}

svmTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    
    X = XL[, -ncol(XL)]
    Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
    
    trControl = trainControl(method='cv', number=3, classProbs=T, summaryFunction=mnLogLoss)
    capture.output(model <- train(X, Y, method='svmRadial', metric='logLoss', maximize=F, trControl=trControl, verbose=F))
    
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

xgbTrainAlgo = function (XL) {
  my.extendedColsTrain(XL, function(XL) {
    my.normalizedTrain(XL, function (XL) {
      my.train.xgb(XL, rowsFactor=0.9, iters=200)
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

xgbnNnetAggregatedTrain = function (XL) {
  gmeanAggregator(c(
    nnetTrainAlgo(XL),
    xgbTrainAlgo(XL)
  ))
}

#print(validation.tqfold(XLL, lgbTrainAlgo, folds=5, iters=6, verbose=T))
#print(geneticSelect(iterations=200, XL=XLL, teach=function (XL) {my.teach(XL, rowsFactor=0.6, iters=3, colsFactor=1)}, maxPopulationSize=15, mutationProb=0.2))\
#my.rfe(XLL)


#a3 = lgbTrainAlgo(XLL)
#a2 = xgbTrainAlgo(XLL)
#a1 = nnetTrainAlgo(XLL)
"
alg = gmeanAggregator(c(a1))
XXX = read.csv(file='x_test.csv', head=T, sep=';', na.strings='?')
XXX = preCols(XXX)
results = alg(XXX)
write(results, file='res.txt', sep='\n')
"