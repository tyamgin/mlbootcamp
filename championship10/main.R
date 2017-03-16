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
require(kknn)

debugSource("algos.R")
debugSource("nnet.R")
debugSource("svm.R")
debugSource("xgb.R")
debugSource("glm.R")
debugSource("lgb.R")
debugSource("genetic.R")
debugSource("ensembling.R")
debugSource("preProcess.R")

my.dopar.exports = c('validation.tqfold', 'my.normalizedTrain', 'nnetTrainAlgo', 'my.extendedColsTrain',
                     'nnetTeachAlgo', 'error.logloss', 'vapnik.logloss', 'nnetBootTrainAlgo',
                     'my.boot', 'meanAggregator', 'extendXYCols', 'extendCols', 'my.train.lgb')
my.dopar.packages = c('caret', 'lightgbm')

XX = read.csv(file="x_train.csv", head=T, sep=";", na.strings="?")
YY = read.csv(file="y_train.csv", head=F, sep=";", na.strings="?")

extendCols = function (XX, idxes=NULL) {
  if (!is.null(idxes)) {
    sz = ncol(XX)
    for (j in 2:sz) {
      for (k in 1:(j-1)) {
        num = XX[, j]
        denum = XX[, k]
        
        XX = cbind(XX, num / (denum + ifelse(min(denum) == 0, 1, 0)))
        XX = cbind(XX, num * denum)
      }
    }
    if (is.logical(idxes) && idxes) {
      #все
    } else {
      XX = XX[, which(1 == idxes)]
    }
  }
  
  XX
}
insertXLCol = function (XL, Z, idxes=NULL) {
  X = XL[, -ncol(XL), drop=F]
  Y = XL[, ncol(XL), drop=F]
  cbind(X, Z, Y)
}
extendXYCols = function (XL, idxes) {
  X = XL[, -ncol(XL), drop=F]
  Y = XL[, ncol(XL), drop=F]
  X = extendCols(X, idxes)
  cbind(X, Y)
}

unnameMatrix = function (XX) {
  as.matrix(unname(data.matrix(XX)))
}

XX = unnameMatrix(XX)
XLL = unnameMatrix(cbind(data.matrix(XX), YY))


my.extendedColsTrain = function (XL, trainFunc, idxes=NULL) {
  featuresNumber = ncol(XL) - 1
  XL = extendXYCols(XL, idxes)
  model = trainFunc(XL)
  function (X) {
    if (ncol(X) != featuresNumber)
      stop('invalid number of columns')
    X = extendCols(X, idxes)
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
my.fromFileAlgo = function (path) {
  ansvec = as.matrix(read.csv(file=path, head=F))[, 1]
  function (X) {
    if (nrow(X) != length(ansvec))
      stop('this is computed algo')
    ansvec
  }
}


#cl <- makeCluster(2)
#registerDoParallel(cl)

#my.ensemble.stacking(XLL)

#set.seed(2707); print(validation.tqfold(XLL, nnetBootEliteTrainAlgo, folds=7, iters=10, verbose=T))


#set.seed(2701);print(geneticSelect(iterations=200, XL=extendXYCols(XLL), teach=function (XL) {
#  my.normalizedTrain(XL, function (XL) {
#    #my.train.lgb(XL, iters=1, rowsFactor=0.9)
#    nnetTrainAlgo(XL) #dec=0.01
#  })
#}, maxPopulationSize=13, mutationProb=0.2, startOnesProbab=0.35))

#set.seed(2708);print(addRemoveSelect(iterations=10000, XL=extendXYCols(XLL, T), teach=function (XL) {
#  my.normalizedTrain(XL, function (XL) {
#    #nnetBootTrainAlgo(XL)
#    my.train.lgb(XL, iters=10, rowsFactor=0.9)
#  })
#}, startVec=1==c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1)))
#}, startVec=c(T,  T, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,  T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F)))

set.seed(2708);algb = lgbTrainAlgo(XLL)
#set.seed(2707);annet = nnetTrainAlgo(XLL)
#set.seed(2707);annetmagic = nnetMagicTrainAlgo(XLL) # saved to annetmagic.RData
#set.seed(2707);annetbt = nnetBootTrainAlgo(XLL)
set.seed(2707);annetbt2 = nnetBootEliteTrainAlgo(XLL)
#set.seed(2708);asvm = svmTrainAlgo(XLL)


#stopCluster(cl)

print('computed')
alg = gmeanAggregator(c(algb, annetbt2))
#alg = meanAggregator(c(my.fromFileAlgo('lgb_old_features.txt'), my.fromFileAlgo('nnet200_09_20.txt')))
#alg = logitTrainAlgo(XLL, c(my.fromFileAlgo('algb200.txt'), my.fromFileAlgo('nnet200_09_20.txt')))
#alg = algb

XXX = read.csv(file='x_test.csv', head=T, sep=';', na.strings='?')
XXX = unnameMatrix(XXX)
results1 = alg(XXX)
results = correctPreProcessedAnswers(XLL, XXX, results1)
write(results, file='res.txt', sep='\n')
print('done')

