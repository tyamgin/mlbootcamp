options( java.parameters = "-Xmx16000m" )

set.seed(2707)
require(ggplot2)
require(GGally)
require(foreach)
require(xgboost)
require(lightgbm)
require(caret)
require(mxnet)
require(nnet)

if (!exists('prev.set.seed')) {
  prev.set.seed = set.seed
  set.seed = function (x) {
    prev.set.seed(x)
    mx.set.seed(x)
  }
}

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
debugSource('tune.R')

my.dopar.exports = c()
my.dopar.packages = c()

XLL = read.csv(file="data/train.csv", head=T, sep=";", na.strings="None")
XXX = read.csv(file="data/test.csv", head=T, sep=";", na.strings="None")

#ggpairs(XLL[1:1000,], aes(colour='red', alpha=0.4))
#XLL2 = XLL[which(XLL$ap_lo < 300 & XLL$ap_hi < 300), ]
#ggplot(XLL2, aes(x=ap_hi, y=ap_lo, colour=as.factor(XLL2$cardio))) + geom_point(alpha=.3) + scale_color_manual(values=1:2)
#ggplot(XLL, aes(XLL$age/365)) + geom_histogram(binwidth=0.1)

xgbParams = expand.grid(
  iters=5,
  rowsFactor=1,
  
  max_depth=c(4), 
  gamma=7.99583567306399,#c(0.7),
  lambda=7.87930519785732,#c(1),
  alpha=8.40669463388622,#c(10),
  eta=0.369333719557617,#c(0.075),
  subsample=0.85628359192051,#c(0.9),
  colsample_bytree=0.691344426204916,#c(0.7),
  min_child_weight=97.8118330226557,#c(10),
  nthread=4, 
  nrounds=374,#c(175),
  early_stopping_rounds=0,
  num_parallel_tree=1
)
xgbParams = expand.grid(
  iters=5,
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
)

lgbParams = list(
  iters=5,
  rowsFactor=1,
  
  num_leaves=c(10),#c(13),
  nrounds=c(65),#c(175),
  learning_rate=c(0.223558),#c(0.09),
  
  max_depth=c(4),
  lambda_l2=13.369908,#c(0),
  feature_fraction=0.746088,#c(0.65),
  min_data_in_leaf=382,#c(55),
  bagging_fraction=0.910187,#c(0.8),
  early_stopping_rounds=0,
  nthread=4 
)

mxnetParams = expand.grid(
  hidden_node=5, 
  out_activation='logistic',
  activation='tanh',
  num.round=c(100), 
  array.batch.size=c(100),  
  learning.rate=c(0.1), 
  momentum=c(0.1), 
  dropout=c(0)
)

lgbXgbTrainAlgo = function (XL, params, newdata=NULL) {
  gmeanAggregator(c(
    xgbTrainAlgo(XL, xgbParams, newdata),
    lgbTrainAlgo(XL, lgbParams, newdata)
  ))
}

#debugSource('diffevol.R'); lol();

"
my.tuneSequential(XLL, function (params) {
  function (XL, newdata) {
    lgbTrainAlgo(XL, params)
    #lgbXgbTrainAlgo(XL, NULL)
  }
}, lgbParams, verbose=T, loops=1, iters=15, use.newdata=T)
lol()
"
#"
my.tuneSequential(XLL, function (params) {
  function (XL, newdata) {
    xgbTrainAlgo(XL, params)
  }
}, xgbParams, verbose=T, iters=15, folds=7, use.newdata=T)
lol()
#"
"
my.gridSearch(XLL, function (params) {
  function (XL, newdata) {
    nnetTrainAlgo(XL, params)
  }
}, mxnetParams, verbose=T, iters=1, folds=4, use.newdata=F)
lol()
"

'
set.seed(37234)
fff = T#c("age", "gender", "height", "weight", "ap_hi", "ap_lo", "cholesterol", "gluc", "smoke", "alco", "active", "bmi", "bmi4", "al_diff", "map", "lol2", "lol3", "fat", "FRSq", "smoke_le0_and_alco_le0", "gender_le1_and_cholesterol_le2", "cholesterol_le1_and_active_le0", "cholesterol_le1_and_gluc_le1",  "log_age_mul_log_height", "log_age_div_log_height", "log_age_mul_sqrt_height", "log_age_div_sqrt_height", "log_age_mul_pow2_height", "log_age_div_pow2_height", "log_age_mul_height", "log_age_div_height", "sqrt_age_mul_log_height", "sqrt_age_div_log_height", "sqrt_age_mul_sqrt_height", "sqrt_age_div_sqrt_height", "sqrt_age_mul_pow2_height", "sqrt_age_div_pow2_height", "sqrt_age_mul_height", "sqrt_age_div_height", "pow2_age_mul_log_height", "pow2_age_div_log_height", "pow2_age_mul_sqrt_height", "pow2_age_div_sqrt_height", "pow2_age_mul_pow2_height", "pow2_age_div_pow2_height", "pow2_age_mul_height", "pow2_age_div_height", "age_mul_log_height", "age_div_log_height", "age_mul_sqrt_height", "age_div_sqrt_height", "age_mul_pow2_height", "age_div_pow2_height", "age_mul_height", "age_div_height", "log_age_mul_log_weight", "log_age_div_log_weight", "log_age_mul_sqrt_weight", "log_age_div_sqrt_weight", "log_age_mul_pow2_weight", "log_age_div_pow2_weight", "log_age_mul_weight", "log_age_div_weight", "sqrt_age_mul_log_weight", "sqrt_age_div_log_weight", "sqrt_age_mul_sqrt_weight", "sqrt_age_div_sqrt_weight", "sqrt_age_mul_pow2_weight", "sqrt_age_div_pow2_weight", "sqrt_age_mul_weight", "sqrt_age_div_weight", "pow2_age_mul_log_weight", "pow2_age_div_log_weight", "pow2_age_mul_sqrt_weight", "pow2_age_div_sqrt_weight", "pow2_age_mul_pow2_weight", "pow2_age_div_pow2_weight", "pow2_age_mul_weight", "pow2_age_div_weight", "age_mul_log_weight", "age_div_log_weight", "age_mul_sqrt_weight", "age_div_sqrt_weight", "age_mul_pow2_weight", "age_div_pow2_weight", "age_mul_weight", "age_div_weight", "log_height_mul_log_weight", "log_height_div_log_weight", "log_height_mul_sqrt_weight", "log_height_div_sqrt_weight", "log_height_mul_pow2_weight", "log_height_div_pow2_weight", "log_height_mul_weight", "log_height_div_weight", "sqrt_height_mul_log_weight", "sqrt_height_div_log_weight", "sqrt_height_mul_sqrt_weight", "sqrt_height_div_sqrt_weight", "sqrt_height_mul_pow2_weight", "sqrt_height_div_pow2_weight", "sqrt_height_mul_weight", "sqrt_height_div_weight", "pow2_height_mul_log_weight", "pow2_height_div_log_weight", "pow2_height_mul_sqrt_weight", "pow2_height_div_sqrt_weight", "pow2_height_mul_pow2_weight", "pow2_height_div_pow2_weight", "pow2_height_mul_weight", "pow2_height_div_weight", "height_mul_log_weight", "height_div_log_weight", "height_mul_sqrt_weight", "height_div_sqrt_weight", "height_mul_pow2_weight", "height_div_pow2_weight", "height_mul_weight", "height_div_weight", "log_age_mul_log_ap_hi", "log_age_div_log_ap_hi", "log_age_mul_sqrt_ap_hi", "log_age_div_sqrt_ap_hi", "log_age_mul_pow2_ap_hi", "log_age_div_pow2_ap_hi", "log_age_mul_ap_hi", "log_age_div_ap_hi", "sqrt_age_mul_log_ap_hi", "sqrt_age_div_log_ap_hi", "sqrt_age_mul_sqrt_ap_hi", "sqrt_age_div_sqrt_ap_hi", "sqrt_age_mul_pow2_ap_hi", "sqrt_age_div_pow2_ap_hi", "sqrt_age_mul_ap_hi", "sqrt_age_div_ap_hi", "pow2_age_mul_log_ap_hi", "pow2_age_div_log_ap_hi", "pow2_age_mul_sqrt_ap_hi", "pow2_age_div_sqrt_ap_hi", "pow2_age_mul_pow2_ap_hi", "pow2_age_div_pow2_ap_hi", "pow2_age_mul_ap_hi", "pow2_age_div_ap_hi", "age_mul_log_ap_hi", "age_div_log_ap_hi", "age_mul_sqrt_ap_hi", "age_div_sqrt_ap_hi", "age_mul_pow2_ap_hi", "age_div_pow2_ap_hi", "age_mul_ap_hi", "age_div_ap_hi", "log_height_mul_log_ap_hi", "log_height_div_log_ap_hi", "log_height_mul_sqrt_ap_hi", "log_height_div_sqrt_ap_hi", "log_height_mul_pow2_ap_hi", "log_height_div_pow2_ap_hi", "log_height_mul_ap_hi", "log_height_div_ap_hi", "sqrt_height_mul_log_ap_hi", "sqrt_height_div_log_ap_hi", "sqrt_height_mul_sqrt_ap_hi", "sqrt_height_div_sqrt_ap_hi", "sqrt_height_mul_pow2_ap_hi", "sqrt_height_div_pow2_ap_hi", "sqrt_height_mul_ap_hi", "sqrt_height_div_ap_hi", "pow2_height_mul_log_ap_hi", "pow2_height_div_log_ap_hi", "pow2_height_mul_sqrt_ap_hi", "pow2_height_div_sqrt_ap_hi", "pow2_height_mul_pow2_ap_hi", "pow2_height_div_pow2_ap_hi", "pow2_height_mul_ap_hi", "pow2_height_div_ap_hi", "height_mul_log_ap_hi", "height_div_log_ap_hi", "height_mul_sqrt_ap_hi", "height_div_sqrt_ap_hi", "height_mul_pow2_ap_hi", "height_div_pow2_ap_hi", "height_mul_ap_hi", "height_div_ap_hi", "log_weight_mul_log_ap_hi", "log_weight_div_log_ap_hi", "log_weight_mul_sqrt_ap_hi", "log_weight_div_sqrt_ap_hi", "log_weight_mul_pow2_ap_hi", "log_weight_div_pow2_ap_hi", "log_weight_mul_ap_hi", "log_weight_div_ap_hi", "sqrt_weight_mul_log_ap_hi", "sqrt_weight_div_log_ap_hi", "sqrt_weight_mul_sqrt_ap_hi", "sqrt_weight_div_sqrt_ap_hi", "sqrt_weight_mul_pow2_ap_hi", "sqrt_weight_div_pow2_ap_hi", "sqrt_weight_mul_ap_hi", "sqrt_weight_div_ap_hi", "pow2_weight_mul_log_ap_hi", "pow2_weight_div_log_ap_hi", "pow2_weight_mul_sqrt_ap_hi", "pow2_weight_div_sqrt_ap_hi", "pow2_weight_mul_pow2_ap_hi", "pow2_weight_div_pow2_ap_hi", "pow2_weight_mul_ap_hi", "pow2_weight_div_ap_hi", "weight_mul_log_ap_hi", "weight_div_log_ap_hi", "weight_mul_sqrt_ap_hi", "weight_div_sqrt_ap_hi", "weight_mul_pow2_ap_hi", "weight_div_pow2_ap_hi", "weight_mul_ap_hi", "weight_div_ap_hi", "log_age_mul_log_ap_lo", "log_age_div_log_ap_lo", "log_age_mul_sqrt_ap_lo", "log_age_div_sqrt_ap_lo", "log_age_mul_pow2_ap_lo", "log_age_div_pow2_ap_lo", "log_age_mul_ap_lo", "log_age_div_ap_lo", "sqrt_age_mul_log_ap_lo", "sqrt_age_div_log_ap_lo", "sqrt_age_mul_sqrt_ap_lo", "sqrt_age_div_sqrt_ap_lo", "sqrt_age_mul_pow2_ap_lo", "sqrt_age_div_pow2_ap_lo", "sqrt_age_mul_ap_lo", "sqrt_age_div_ap_lo", "pow2_age_mul_log_ap_lo", "pow2_age_div_log_ap_lo", "pow2_age_mul_sqrt_ap_lo", "pow2_age_div_sqrt_ap_lo", "pow2_age_mul_pow2_ap_lo", "pow2_age_div_pow2_ap_lo", "pow2_age_mul_ap_lo", "pow2_age_div_ap_lo", "age_mul_log_ap_lo", "age_div_log_ap_lo", "age_mul_sqrt_ap_lo", "age_div_sqrt_ap_lo", "age_mul_pow2_ap_lo", "age_div_pow2_ap_lo", "age_mul_ap_lo", "age_div_ap_lo", "log_height_mul_log_ap_lo", "log_height_div_log_ap_lo", "log_height_mul_sqrt_ap_lo", "log_height_div_sqrt_ap_lo", "log_height_mul_pow2_ap_lo", "log_height_div_pow2_ap_lo", "log_height_mul_ap_lo", "log_height_div_ap_lo", "sqrt_height_mul_log_ap_lo", "sqrt_height_div_log_ap_lo", "sqrt_height_mul_sqrt_ap_lo", "sqrt_height_div_sqrt_ap_lo", "sqrt_height_mul_pow2_ap_lo", "sqrt_height_div_pow2_ap_lo", "sqrt_height_mul_ap_lo", "sqrt_height_div_ap_lo", "pow2_height_mul_log_ap_lo", "pow2_height_div_log_ap_lo", "pow2_height_mul_sqrt_ap_lo", "pow2_height_div_sqrt_ap_lo", "pow2_height_mul_pow2_ap_lo", "pow2_height_div_pow2_ap_lo", "pow2_height_mul_ap_lo", "pow2_height_div_ap_lo", "height_mul_log_ap_lo", "height_div_log_ap_lo", "height_mul_sqrt_ap_lo", "height_div_sqrt_ap_lo", "height_mul_pow2_ap_lo", "height_div_pow2_ap_lo", "height_mul_ap_lo", "height_div_ap_lo", "log_weight_mul_log_ap_lo", "log_weight_div_log_ap_lo", "log_weight_mul_sqrt_ap_lo", "log_weight_div_sqrt_ap_lo", "log_weight_mul_pow2_ap_lo", "log_weight_div_pow2_ap_lo", "log_weight_mul_ap_lo", "log_weight_div_ap_lo", "sqrt_weight_mul_log_ap_lo", "sqrt_weight_div_log_ap_lo", "sqrt_weight_mul_sqrt_ap_lo", "sqrt_weight_div_sqrt_ap_lo", "sqrt_weight_mul_pow2_ap_lo", "sqrt_weight_div_pow2_ap_lo", "sqrt_weight_mul_ap_lo", "sqrt_weight_div_ap_lo", "pow2_weight_mul_log_ap_lo", "pow2_weight_div_log_ap_lo", "pow2_weight_mul_sqrt_ap_lo", "pow2_weight_div_sqrt_ap_lo", "pow2_weight_mul_pow2_ap_lo", "pow2_weight_div_pow2_ap_lo", "pow2_weight_mul_ap_lo", "pow2_weight_div_ap_lo", "weight_mul_log_ap_lo", "weight_div_log_ap_lo", "weight_mul_sqrt_ap_lo", "weight_div_sqrt_ap_lo", "weight_mul_pow2_ap_lo", "weight_div_pow2_ap_lo", "weight_mul_ap_lo", "weight_div_ap_lo", "log_ap_hi_mul_log_ap_lo", "log_ap_hi_div_log_ap_lo", "log_ap_hi_mul_sqrt_ap_lo", "log_ap_hi_div_sqrt_ap_lo", "log_ap_hi_mul_pow2_ap_lo", "log_ap_hi_div_pow2_ap_lo", "log_ap_hi_mul_ap_lo", "log_ap_hi_div_ap_lo", "sqrt_ap_hi_mul_log_ap_lo", "sqrt_ap_hi_div_log_ap_lo", "sqrt_ap_hi_mul_sqrt_ap_lo", "sqrt_ap_hi_div_sqrt_ap_lo", "sqrt_ap_hi_mul_pow2_ap_lo", "sqrt_ap_hi_div_pow2_ap_lo", "sqrt_ap_hi_mul_ap_lo", "sqrt_ap_hi_div_ap_lo", "pow2_ap_hi_mul_log_ap_lo", "pow2_ap_hi_div_log_ap_lo", "pow2_ap_hi_mul_sqrt_ap_lo", "pow2_ap_hi_div_sqrt_ap_lo", "pow2_ap_hi_mul_pow2_ap_lo", "pow2_ap_hi_div_pow2_ap_lo", "pow2_ap_hi_mul_ap_lo", "pow2_ap_hi_div_ap_lo", "ap_hi_mul_log_ap_lo", "ap_hi_div_log_ap_lo", "ap_hi_mul_sqrt_ap_lo", "ap_hi_div_sqrt_ap_lo", "ap_hi_mul_pow2_ap_lo", "ap_hi_div_pow2_ap_lo", "ap_hi_mul_ap_lo", "ap_hi_div_ap_lo")
addRemoveSelect(iterations=10000, XL=extendXYCols(my.fixData(XLL), features=fff), teach=function (XL, newdata=NULL) {
  my.normalizedTrain(XL, function (XL, newdata=NULL) {
    params = xgbParams
    params$iters = 3
    my.train.xgb(XL, params, newdata)
  }, newdata=newdata)
}, startFeatures=xgb.features)
'

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
lgbParams$iters = 100
xgbParams$iters = 100

#lgbAlg = lgbTrainAlgo(XLL, lgbParams)
xgbAlg = xgbTrainAlgo(XLL, xgbParams)

alg = xgbAlg
results = alg(XXX)
write(results, file='res/res.txt', sep='\n')
print('done')