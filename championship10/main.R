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
debugSource("lgb.R")
debugSource("knn.R")
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
  
  #super last "tqfold 0 iterations remains, mean=0.379215446151919 sd=0.00882421181974091"
  
  XX = XX[, which(1 == c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1))]
  
  #lll
  #XX = XX[, which(1 == c(0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0))]
  
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


#cl <- makeCluster(detectCores())
#registerDoParallel(cl)

#set.seed(2707); print(validation.tqfold(XLL, lgbTrainAlgo, folds=7, iters=10, verbose=T))
#set.seed(2702);print(geneticSelect(iterations=200, XL=extendXYCols(XLL), teach=function (XL) {
#  my.normalizedTrain(XL, function (XL) {
#    #my.train.lgb(XL, iters=4, rowsFactor=0.9)
#    nnetTrainAlgo(XL) #dec=0.01
#  })
#}, maxPopulationSize=13, mutationProb=0.15, startOnesProbab=0.15))


#set.seed(2708);algb = lgbTrainAlgo(XLL)
#set.seed(2707);annet = nnetTrainAlgo(XLL)
#set.seed(2707);annetmagic = nnetMagicTrainAlgo(XLL)
#set.seed(2707);annetbt = nnetBootTrainAlgo(XLL)
#set.seed(2708);asvm = svmTrainAlgo(XLL)
#set.seed(2708);aknn = knnTrainAlgo(XLL)
#set.seed(2708);arf = rfTrainAlgo(XLL)

#rs = resamples(list(svm=svmModel1, nnet=mmm, knn=knnm))
#modelCor(rs)

#stopCluster(cl)



alg = meanAggregator(c(algb, algb, annetbt, annetmagic))
XXX = read.csv(file='x_test.csv', head=T, sep=';', na.strings='?')
XXX = preCols(XXX)
results = alg(XXX)
write(results, file='res.txt', sep='\n')
print('done')
