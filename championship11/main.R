options( java.parameters = "-Xmx16000m" )

set.seed(2707)
require(ggplot2)
require(GGally)
require(corrgram)
require(lightgbm)
require(foreach)
require(doParallel)
require(caret)
require(randomForest)
require(lars)
require(xgboost)
require(e1071)
require(rJava)
require(extraTrees)

debugSource("ext.R")
debugSource("algos.R")
debugSource("lgb.R")
debugSource("xgb.R")
debugSource("nnet.R")
debugSource("genetic.R")
debugSource("preprocess.R")

my.dopar.exports = c('validation.tqfold', 'validation.tqfold.enumerate', 'my.normalizedTrain', 'nnetTrainAlgo', 
                     'my.extendedColsTrain', 'my.roundedTrain', 'error.accuracy', 'my.train.nnet',
                     'my.boot', 'meanAggregator', 'extendXYCols', 'extendCols', 'my.train.lgb',
                     'my.dopar.exports', 'my.dopar.packages')
my.dopar.packages = c('caret', 'lightgbm', 'foreach', 'rJava', 'extraTrees')

XX = read.csv(file="data/x_train.csv", head=F, sep=";", na.strings="?")
YY = read.csv(file="data/y_train.csv", head=F, sep=";", na.strings="?")



XX = unnameMatrix(XX)
XX = my.data.transformFeatures(XX)
XLL = unnameMatrix(cbind(data.matrix(XX), YY))

"
my.gridSearch(XLL, function (params) {
  function (XL, newdata=NULL) {
    etWithBin12TrainAlgo(XL, params, newdata=newdata)
    #etTrainAlgo(XL, params, newdata=newdata)
    #etGlmTrainAlgo(XL, params)
  }
}, expand.grid(numRandomCuts=c(1), mtry=c(2), ntree=c(2000), iters=1, rowsFactor=1, extra=F), verbose=T, iters=6, use.newdata=F)
exit()
"


"
my.gridSearch(XLL, function (params) {
  function (XL, newdata=NULL) {
    glmTrainAlgo(XL, params, newdata=newdata)
  }
}, expand.grid(), verbose=T, iters=6)
exit()
"

"
my.gridSearch(XLL, function (params) {
  function (XL) {
    nnetTrainAlgo(XL, params)
  }
}, expand.grid(size=3, maxit=c(100), decay=c(0)))
"


my.gridSearch(XLL, function (params) {
  function (XL, newdata) {
    xgbTrainAlgo(XL, params)
  }
}, expand.grid(
  iters=1,
  rowsFactor=1,

  max_depth=7, 
  gamma=0, 
  lambda=0.129457, 
  alpha=0.812294, 
  eta=c(0.024637), 
  colsample_bytree=0.630299,
  min_child_weight=3,
  subsample=0.996574,
  nthread=4, 
  nrounds=c(1192),
  early_stopping_rounds=0
), verbose=T)
exit()          

"
set.seed(2707);aXgb = xgbTrainAlgo(XLL, expand.grid(  
  iters=10,
  rowsFactor=0.95,
  max_depth=7, 
  gamma=0, 
  lambda=0.129457, 
  alpha=0.812294, 
  eta=0.024637, 
  colsample_bytree=0.630299,
  min_child_weight=3,
  subsample=0.996574,
  nthread=4, 
  nrounds=1192))"

XXX = read.csv(file='data/x_test.csv', head=F, sep=';', na.strings='?')
XXX = unnameMatrix(XXX)
XXX = my.data.transformFeatures(XXX, T)

set.seed(2707);aEtwb = etWithBin12TrainAlgo(XLL, expand.grid(numRandomCuts=1, mtry=2, ntree=2000, iters=100, rowsFactor=0.95, extra=F), newdata=XXX); print('trained')
#set.seed(2707);aEt = etTrainAlgo(XLL, expand.grid(numRandomCuts=1, mtry=2, ntree=2000, iters=1, rowsFactor=1)); print('trained')
alg=aEtwb


#XLLbin12 = XLL
#XLLbin12[, ncol(XLLbin12)] = ifelse(XLLbin12[, ncol(XLLbin12)] <= 1, 0, 1)
#set.seed(2707);print(validation.tqfold(XLLbin12, nnetTrainAlgo, folds=7, iters=4, verbose=T))

"
set.seed(3233)
addRemoveSelect(iterations=10000, XL=cbind(XLL[, -ncol(XLL)], eext(XLL), XLL[, ncol(XLL)]), teach=function (XL) {
  my.roundedTrain(XL, function (XL) {
    my.normalizedTrain(XL, function (XL) {
      my.train.et(XL, expand.grid(numRandomCuts=1, mtry=2, ntree=2000, iters=1, rowsFactor=1))
    })
  })
}, startVec=neee)
"
"
set.seed(3233)
addRemoveSelect(iterations=10000, XL=extendXYCols(XLL, idxes=neee, pairs=T), teach=function (XL) {
  my.roundedTrain(XL, function (XL, newdata=NULL) {
    my.normalizedTrain(XL, function (XL, newdata=NULL) {
      my.train.et(XL, expand.grid(numRandomCuts=1, mtry=2, ntree=2000, iters=1, rowsFactor=1))
    })
  })
}, startVec=c(1,1,1,1,0,1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,1,0,0,0,0,0,0,0,0))
"

"
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(2709);print(geneticSelect(iterations=200, XL=XLL, teach=function (XL) {
  my.roundedTrain(XL, function (XL) {
    my.normalizedTrain(XL, function (XL) {
      #my.train.lgb(XL, rowsFactor=0.9, iters=3, lgb.nthread=1)
      my.train.nnet(XL)
    })
  })
}, config='genetic.config'))
stopCluster(cl)
"

# https://www.r-bloggers.com/7-visualizations-you-should-learn-in-r/


#ggplot(train, aes(Item_Visibility, Item_MRP)) + geom_point() + scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+ scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))+ theme_bw() 

#X_X = data.frame(XX)
#plt = ggplot(data=X_X, aes(X_X[,18]))+geom_histogram(binwidth=0.01)
#print(plt)
  #scale_x_continuous("Item MRP", breaks = seq(0,270,by = 30)) +
  #scale_y_continuous("Count", breaks = seq(0,200,by = 20)) +
  #labs(title = "Histogram")


#plot(density((X_X[,77]-mean(X_X[,77])/sd(X_X[,77]))))
#lines(density((X_X[,103]-mean(X_X[,103])/sd(X_X[,103]))), col='red')


results = alg(XXX)
write(results, file='res/res.txt', sep='\n')
print('done')
