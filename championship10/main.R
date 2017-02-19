set.seed(2707)
require(kernlab)
require(scatterplot3d)
require(xgboost)
# install.packages('e1071', dependencies = TRUE)
require(class) 
require(e1071) 

debugSource("algos.R")
debugSource("genetic.R")

XX = read.csv(file="x_train.csv", head=T, sep=";", na.strings="?")
YY = read.csv(file="y_train.csv", head=F, sep=";", na.strings="?")

preCols = function (XX) {
  XX = cbind(XX, XX$totalBonusScore / (1 + XX$totalScore))
  XX = cbind(XX, XX$totalScore / XX$numberOfDaysActuallyPlayed)
  XX = cbind(XX, XX$totalStarsCount / (1 + XX$totalBonusScore))
  XX = cbind(XX, XX$attemptsOnTheHighestLevel / XX$totalNumOfAttempts)
  XX = cbind(XX, XX$numberOfAttemptedLevels / XX$totalNumOfAttempts)

  XX = as.matrix(unname(data.matrix(XX)))

  XX = XX[, -c(5, 8, 10, 11, 13, 14, 15)]
  XX
}

"
for (j in 1:ncol(XX)) {
  for (k in 1:(j-1)) {
    num = XX[, j]
    denum = XX[, k]
    if (min(denum) == 0)
      denum = denum + 1
    XX = cbind(XX, num / denum)
  }
}
"

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
XX = XX[, 1:10]
"

my.teach = function (XLL, iters=10, rowsFactor=0.3, colsFactor=0.3) {
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


teachAlgo = function (XL) {
  my.teach(XL, rowsFactor=0.6, iters=150, colsFactor=1)
}
#print(paste0('tqfold: ', validation.tqfold(XLL, teachAlgo, folds=10, iters=1, verbose=T)))
#print(geneticSelect(iterations=100, XL=XLL, teach=teachAlgo, maxPopulationSize=10))


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
"
teachAlgo = function (XL) {
  algos = svm.getBaseAlgos(XL)
  randomForestTreeFloatAggregator(0, algos)
}
print(paste0('tqfold: ', validation.tqfold(XLL, teachAlgo, folds=2, iters=6, verbose=T)))
"

#aggregated = my.teach(XLL, rowsFactor=0.6, iters=20, colsFactor=1)
#print(paste0("learn   average logloss: ", calculateError(XLL, aggregated, multi=T)))
#print(paste0("control average logloss: ", calculateError(XKK, aggregated, multi=T)))


alg = teachAlgo(XLL)
XXX = read.csv(file='x_test.csv', head=T, sep=';', na.strings='?')
XXX = preCols(XXX)
results = rep(0, nrow(XXX))
for (j in 1:ncol(XXX)) {
  XXX[, j] = (XXX[, j] - means[j]) / sds[j]
}
results = alg(XXX)
write(results, file='res.txt', sep='\n')
