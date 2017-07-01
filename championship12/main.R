options( java.parameters = "-Xmx16000m" )

set.seed(2707)
require(ggplot2)
require(GGally)
require(foreach)
require(xgboost)
require(lightgbm)
require(caret)

debugSource('cv.R')
debugSource('ext.R')
debugSource('clear.R')
debugSource('cache.R')
debugSource('aggregators.R')
debugSource('xgb.R')
debugSource('lgb.R')
debugSource('nnet.R')
debugSource('score.R')
debugSource('FRS.R')
debugSource('feat-select.R')

my.dopar.exports = c()
my.dopar.packages = c()

XLL = read.csv(file="data/train.csv", head=T, sep=";", na.strings="None")
XXX = read.csv(file="data/test.csv", head=T, sep=";", na.strings="None")

#ggpairs(XLL[1:1000,], aes(colour='red', alpha=0.4))
#XLL2 = XLL[which(XLL$ap_lo < 300 & XLL$ap_hi < 300), ]
#ggplot(XLL2, aes(x=ap_hi, y=ap_lo, colour=as.factor(XLL2$cardio))) + geom_point(alpha=.3) + scale_color_manual(values=1:2)
#ggplot(XLL, aes(XLL$age/365)) + geom_histogram(binwidth=0.1)

nnetXgbParams = expand.grid(
  iters=100,
  rowsFactor=1,
  
  max_depth=c(4), 
  gamma=c(0.7),
  lambda=c(1),
  alpha=c(10),
  eta=c(0.075),
  subsample=c(0.9),
  colsample_bytree=c(0.7),
  min_child_weight=c(10),
  nthread=4, 
  nrounds=c(175),
  early_stopping_rounds=0,
  num_parallel_tree=1
  
 # decay=c(0.8), size=c(5), maxit=c(200)
)

lgbParams = expand.grid(
  iters=5,
  rowsFactor=1,
  
  num_leaves=c(13),
  max_depth=c(4),
  lambda_l2=c(0),
  learning_rate=c(0.1),
  feature_fraction=c(0.75),
  min_data_in_leaf=c(55),
  bagging_fraction=c(0.8),
  nrounds=c(130),
  early_stopping_rounds=0,
  nthread=4 
)

"
my.gridSearch(XLL, function (params) {
  function (XL, newdata) {
    lgbTrainAlgo(XL, params)
  }
}, lgbParams, verbose=T, iters=15, use.newdata=T)
lol()
"
"
my.gridSearch(XLL, function (params) {
  function (XL, newdata) {
    xgbTrainAlgo(XL, params)
    #nnetXgbTrainAlgo(XL, params)
  }
}, nnetXgbParams, verbose=F, iters=15, use.newdata=T)
lol()
"
"
my.gridSearch(XLL, function (params) {
  function (XL, newdata) {
    etTrainAlgo(XL, params)
  }
}, expand.grid(ntree=c(1000), mtry=c(4), numRandomCuts=c(1), nodesize=c(1)), verbose=T, iters=3, use.newdata=F)
lol()

my.gridSearch(XLL, function (params) {
  function (XL, newdata) {
    nnetTrainAlgo(XL, params)
  }
}, expand.grid(decay=c(0.8), size=c(5), maxit=c(200)), verbose=F, iters=3, use.newdata=F)
lol()
"

postProcess = function (X) {
  X$smoke[which(is.na(X$smoke))] = 0# predict(knn.model.smoke, sel.col(X[which(is.na(X$smoke)),]))
  X$alco[which(is.na(X$alco))] = 0#predict(knn.model.smoke, sel.col(X[which(is.na(X$alco)),]))
  X$active[which(is.na(X$active))] = 1#predict(knn.model.smoke, sel.col(X[which(is.na(X$active)),]))
  X
}

result = rep(0, nrow(XXX))

my.applyMask = function (X, smoke, alco, active) {
  if (smoke == 0) X = subset(X, select=-c(smoke))
  if (alco == 0) X = subset(X, select=-c(alco))
  if (active == 0) X = subset(X, select=-c(active))
  X
}

"
for (smoke in 0:1) {
  for (alco in 0:1) {
    for (active in 0:1) {
      alg = xgbTrainAlgo(my.applyMask(XLL, smoke, alco, active), xgbParams)
      
      idx = which((!is.na(XXX$smoke) == smoke) & (!is.na(XXX$alco) == alco) & (!is.na(XXX$active) == active))
      result[idx] = alg(my.applyMask(XXX[idx, ], smoke, alco, active))
    }
  } 
}
"

#alg = lgbTrainAlgo(XLL, lgbParams)
alg = xgbTrainAlgo(XLL, nnetXgbParams)
XXX1 = postProcess(XXX)
results = alg(XXX1)
write(results, file='res/res.txt', sep='\n')
print('done')