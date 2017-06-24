require(ggplot2)
require(GGally)
require(foreach)
require(xgboost)
require(lightgbm)

debugSource('cv.R')
debugSource('ext.R')
debugSource('clear.R')
debugSource('cache.R')
debugSource('aggregators.R')
debugSource('xgb.R')
debugSource('lgb.R')

my.dopar.exports = c()
my.dopar.packages = c()

XLL = read.csv(file="data/train.csv", head=T, sep=";", na.strings="None")
XXX = read.csv(file="data/test.csv", head=T, sep=";", na.strings="None")

XLL = my.fixData(XLL)
XXX = my.fixData(XXX)

#ggpairs(XLL[1:1000,], aes(colour='red', alpha=0.4))
#XLL2 = XLL[which(XLL$ap_lo < 300 & XLL$ap_hi < 300), ]
#ggplot(XLL2, aes(x=ap_hi, y=ap_lo, colour=as.factor(XLL2$cardio))) + geom_point(alpha=.3) + scale_color_manual(values=1:2)
#ggplot(XLL, aes(XLL$age/365)) + geom_histogram(binwidth=0.1)

xgbParams = expand.grid(
  iters=100,
  rowsFactor=1,
  
  max_depth=c(4), 
  gamma=c(0),#0.7
  lambda=c(1),
  alpha=c(9), #10
  eta=c(0.1),
  colsample_bytree=c(0.7),
  min_child_weight=c(2),
  subsample=c(0.9),
  nthread=4, 
  nrounds=c(140),
  early_stopping_rounds=0,
  num_parallel_tree=1
)

lgbParams = expand.grid(
  iters=5,
  rowsFactor=1,
  
  num_leaves=c(8, 7, 6),#
  max_depth=c(4), #
  lambda_l2=0,#
  learning_rate=c(0.1),#
  feature_fraction=c(0.7),#
  min_data_in_leaf=c(20),#
  bagging_fraction=c(0.9),#
  nrounds=c(140),#
  early_stopping_rounds=0,#
  nthread=4 #
)

my.gridSearch(XLL, function (params) {
  function (XL, newdata) {
    lgbTrainAlgo(XL, params)
  }
}, lgbParams, verbose=F, iters=15, use.newdata=T)
lol()
my.gridSearch(XLL, function (params) {
  function (XL, newdata) {
    xgbTrainAlgo(XL, params)
  }
}, xgbParams, verbose=F, iters=15, use.newdata=T)
lol()



postProcess = function (X) {
  X$smoke[which(is.na(X$smoke))] = predict(knn.model.smoke, sel.col(X[which(is.na(X$smoke)),]))
  X$alco[which(is.na(X$alco))] = predict(knn.model.smoke, sel.col(X[which(is.na(X$alco)),]))
  X$active[which(is.na(X$active))] = predict(knn.model.smoke, sel.col(X[which(is.na(X$active)),]))
  X
}

alg = xgbTrainAlgo(XLL, xgbParams)

XXX1 = postProcess(XXX)
results = alg(XXX1)
write(results, file='res/res.txt', sep='\n')
print('done')