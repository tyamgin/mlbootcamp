set.seed(2707)
require(kernlab)
require(scatterplot3d)
require(xgboost)
require(class) 
require(e1071) 
require(RSNNS)
require(caret)

debugSource("algos.R")
debugSource("genetic.R")

XX = read.csv(file="x_train.csv", head=T, sep=";", na.strings="?")
YY = read.csv(file="y_train.csv", head=F, sep=";", na.strings="?")

preCols = function (XX) {
  "
  XX = cbind(XX, XX$totalBonusScore / (1 + XX$totalScore))
  XX = cbind(XX, XX$totalScore / XX$numberOfDaysActuallyPlayed)
  XX = cbind(XX, XX$totalStarsCount / (1 + XX$totalBonusScore))
  XX = cbind(XX, XX$attemptsOnTheHighestLevel / XX$totalNumOfAttempts)
  XX = cbind(XX, XX$numberOfAttemptedLevels / XX$totalNumOfAttempts)
  "
  XX = as.matrix(unname(data.matrix(XX)))
  "
  sz = ncol(XX)
  for (j in 2:sz) {
    for (k in 1:(j-1)) {
      num = XX[, j]
      denum = XX[, k]

      if (min(denum) == 0)
        denum = denum + 1
      XX = cbind(XX, num / denum)
      XX = cbind(XX, num * denum)
    }
  }

  XX = XX[, which(1 == c(0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0,1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0,0, 0, 0, 0, 1, 0, 0,1, 1, 1, 0, 1, 1, 1,1, 1, 0, 1, 0, 0, 1,0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0))]
  XX = XX[, which(1 == c(1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1,1, 0, 0, 1, 0, 1, 0, 1, 1))]
  "
  XX
}

XX = preCols(XX)

means = rep(NA, nrow(XX))
sds = rep(NA, nrow(XX))
for (j in 1:ncol(XX)) {
  means[j] = mean(XX[, j])
  sds[j] = sd(XX[, j])
  XX[, j] = (XX[, j] - means[j]) / sds[j]
}


"
pc = princomp(XX)
XX = XX %*% solve(t(pc$loadings))
XX = XX[, 1:20]
"

my.teach = function (XLL, iters=10, rowsFactor=0.3, colsFactor=1) {
  algos = list(iters)
  n = nrow(XLL)
  m = ncol(XLL) - 1
  for (it in 1:iters) {
    sampleIdxes = sample(1:n, rowsFactor*n)
    cols = sample(1:m, colsFactor*m)
    XK = XLL[-sampleIdxes, c(cols, m + 1)]
    XL = XLL[sampleIdxes, c(cols, m + 1)]  
    
    dtrain <- xgb.DMatrix(data=XL[,-ncol(XL)], label=XL[,ncol(XL)])
    dtest <- xgb.DMatrix(data=XK[,-ncol(XK)], label=XK[,ncol(XK)])
    watchlist <- list(train=dtrain, test=dtest)
    
    algos[[it]] = local({
      cols <- cols
      bstSparse <- xgb.train(data=dtrain, watchlist=watchlist, max_depth=3, eta=0.06, nthread=8, nrounds=3000, eval_metric="logloss", objective="binary:logistic", early_stopping_rounds=50, verbose=0)
      function (X) {
        predict(bstSparse, X[, cols])
      }
    })
  }
  randomForestTreeFloatAggregator(0, algos)
}

XLL = as.matrix(unname(cbind(data.matrix(XX), YY)))
#sampleIdxes = sample(1:nrow(XLL), 15000)
#XKK = XLL[-sampleIdxes, ]
#XLL = XLL[sampleIdxes, ]  

clr = ifelse(XLL[,ncol(XLL)]==0,"red","green")
#plot(XL[,1], XL[,2], col=clr)
#scatterplot3d(XL[,1:3], color=clr)
#pairs(XLL[,1:3], col=clr)


"
xxx = XLL[1:1000, -ncol(XLL),drop=F]
aaa = XLL[1:1000, ncol(XLL)]
xxx2 = XLL[10001:15000, -ncol(XLL),drop=F]
aaa2 = XLL[10001:15000, ncol(XLL)]

aaa <- factor(aaa, labels=c('a', 'b'))
aaa2 <- factor(aaa2, labels=c('a', 'b'))
trControl=trainControl(method='cv', number=5, classProbs=TRUE, summaryFunction=mnLogLoss, verbose=F)
capture.output(model <- train(xxx, aaa, method='nnet', metric='logLoss', maxit=1000, maximize=F, trControl=trControl, verbose=F))

pr = predict(model, xxx, type='prob')$b
print( error.logloss(c(aaa)-1, pr) )

pr2 = predict(model, xxx2, type='prob')$b
print( error.logloss(c(aaa2)-1, pr2) )
"
svmTeachAlgo = function (XL) {
  algos = svm.getBaseAlgos(XL, count=1)
  randomForestTreeFloatAggregator(0, algos)
}
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
  capture.output(model <- train(X, Y, method='nnet', metric='logLoss', maxit=100, maximize=F, trControl=trControl, verbose=F))
  
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
teachAlgo = function (XL) {
  my.teach(XL, rowsFactor=0.6, iters=150, colsFactor=1)
}
teachAlgo = function (XL) {
  nnetTeachAlgo(XL[, -ncol(XL)], XL[, ncol(XL)])
}



"
teachAlgo = function (XL) {
  w = monmlp.fit(XL[, -ncol(XL),drop=F], XL[, ncol(XL),drop=F], hidden1=15, monotone=1, bag=TRUE, n.ensemble=1, silent=T)
  
  algos = c()
  algos = c(algos, function (X) {
    pmax(0.01, pmin(0.99, monmlp.predict(x=X, weights=w)))
  })
  algos = c(algos, my.teach(XL, rowsFactor=0.6, iters=1))
  randomForestTreeFloatAggregator(0, algos)
}
"
validation.tqfold(XLL, teachAlgo, folds=5, iters=6, verbose=T)
#print(geneticSelect(iterations=200, XL=XLL, teach=function (XL) {my.teach(XL, rowsFactor=0.6, iters=3, colsFactor=1)}, maxPopulationSize=15, mutationProb=0.2))


"
teachAlgo = function (XL) {
  y = XL[, ncol(XL)]
  levels(y) <- c('0', '1')
  cl = naiveBayes(XL[, -ncol(XL)], y, type='raw')
  function (X) {
    r = predict(cl, X, type='raw')
    c(r[, 2])
  }
}
print(paste0('tqfold: ', validation.tqfold(XLL, teachAlgo, folds=5, iters=6, verbose=T)))
"


#aggregated = my.teach(XLL, rowsFactor=0.6, iters=20, colsFactor=1)
#print(paste0("learn   average logloss: ", calculateError(XLL, aggregated, multi=T)))
#print(paste0("control average logloss: ", calculateError(XKK, aggregated, multi=T)))


"
alg = teachAlgo(XLL)
XXX = read.csv(file='x_test.csv', head=T, sep=';', na.strings='?')
XXX = preCols(XXX)
results = rep(0, nrow(XXX))
for (j in 1:ncol(XXX)) {
  XXX[, j] = (XXX[, j] - means[j]) / sds[j]
}
results = alg(XXX)
write(results, file='res.txt', sep='\n')
"