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
debugSource('score.R')
debugSource('FRS.R')
debugSource('feat-select.R')
debugSource('tune.R')

my.dopar.exports = c()
my.dopar.packages = c()

XLL = read.csv(file="data/train.csv", head=T, sep=";", na.strings="None")
XXX = read.csv(file="data/test.csv", head=T, sep=";", na.strings="None")

xgbParams = list(
  iters=5,
  rowsFactor=1,
  
  max_depth=c(4), 
  gamma=c(0.9),
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
  
  num_leaves=c(15),
  nrounds=c(55),
  learning_rate=c(0.223558),
  
  max_depth=c(4),
  lambda_l2=c(10),
  feature_fraction=c(0.746088),
  min_data_in_leaf=c(382),
  bagging_fraction=c(0.910187),
  early_stopping_rounds=0,
  nthread=4 
)

lgbXgbTrainAlgo = function (XL, params, newdata=NULL) {
  meanAggregator(c(
    xgbTrainAlgo(XL, xgbParams, newdata),
    lgbTrainAlgo(XL, lgbParams, newdata)
  ), c(1/2,1/2))
}

"
my.tuneSequential(XLL, function (params) {
  function (XL, newdata) {
    #lgbTrainAlgo(XL, params)
    lgbXgbTrainAlgo(XL, NULL)
  }
}, lgbParams, verbose=T, loops=1, iters=15, use.newdata=T)
lol()
"
"
my.tuneSequential(XLL, function (params) {
  function (XL, newdata) {
    xgbTrainAlgo(XL, params)
  }
}, xgbParams, verbose=T, iters=15, folds=7, use.newdata=T)
lol()
"


'
set.seed(727934)
fff = T#c("age", "gender", "height", "weight", "ap_hi", "ap_lo", "cholesterol", "gluc", "smoke", "alco", "active", "bmi", "bmi4", "al_diff", "map", "lol2", "lol3", "fat", "FRSq", "smoke_le0_and_alco_le0", "gender_le1_and_cholesterol_le2", "cholesterol_le1_and_active_le0", "cholesterol_le1_and_gluc_le1",  "log_age_mul_log_height", "log_age_div_log_height", "log_age_mul_sqrt_height", "log_age_div_sqrt_height", "log_age_mul_pow2_height", "log_age_div_pow2_height", "log_age_mul_height", "log_age_div_height", "sqrt_age_mul_log_height", "sqrt_age_div_log_height", "sqrt_age_mul_sqrt_height", "sqrt_age_div_sqrt_height", "sqrt_age_mul_pow2_height", "sqrt_age_div_pow2_height", "sqrt_age_mul_height", "sqrt_age_div_height", "pow2_age_mul_log_height", "pow2_age_div_log_height", "pow2_age_mul_sqrt_height", "pow2_age_div_sqrt_height", "pow2_age_mul_pow2_height", "pow2_age_div_pow2_height", "pow2_age_mul_height", "pow2_age_div_height", "age_mul_log_height", "age_div_log_height", "age_mul_sqrt_height", "age_div_sqrt_height", "age_mul_pow2_height", "age_div_pow2_height", "age_mul_height", "age_div_height", "log_age_mul_log_weight", "log_age_div_log_weight", "log_age_mul_sqrt_weight", "log_age_div_sqrt_weight", "log_age_mul_pow2_weight", "log_age_div_pow2_weight", "log_age_mul_weight", "log_age_div_weight", "sqrt_age_mul_log_weight", "sqrt_age_div_log_weight", "sqrt_age_mul_sqrt_weight", "sqrt_age_div_sqrt_weight", "sqrt_age_mul_pow2_weight", "sqrt_age_div_pow2_weight", "sqrt_age_mul_weight", "sqrt_age_div_weight", "pow2_age_mul_log_weight", "pow2_age_div_log_weight", "pow2_age_mul_sqrt_weight", "pow2_age_div_sqrt_weight", "pow2_age_mul_pow2_weight", "pow2_age_div_pow2_weight", "pow2_age_mul_weight", "pow2_age_div_weight", "age_mul_log_weight", "age_div_log_weight", "age_mul_sqrt_weight", "age_div_sqrt_weight", "age_mul_pow2_weight", "age_div_pow2_weight", "age_mul_weight", "age_div_weight", "log_height_mul_log_weight", "log_height_div_log_weight", "log_height_mul_sqrt_weight", "log_height_div_sqrt_weight", "log_height_mul_pow2_weight", "log_height_div_pow2_weight", "log_height_mul_weight", "log_height_div_weight", "sqrt_height_mul_log_weight", "sqrt_height_div_log_weight", "sqrt_height_mul_sqrt_weight", "sqrt_height_div_sqrt_weight", "sqrt_height_mul_pow2_weight", "sqrt_height_div_pow2_weight", "sqrt_height_mul_weight", "sqrt_height_div_weight", "pow2_height_mul_log_weight", "pow2_height_div_log_weight", "pow2_height_mul_sqrt_weight", "pow2_height_div_sqrt_weight", "pow2_height_mul_pow2_weight", "pow2_height_div_pow2_weight", "pow2_height_mul_weight", "pow2_height_div_weight", "height_mul_log_weight", "height_div_log_weight", "height_mul_sqrt_weight", "height_div_sqrt_weight", "height_mul_pow2_weight", "height_div_pow2_weight", "height_mul_weight", "height_div_weight", "log_age_mul_log_ap_hi", "log_age_div_log_ap_hi", "log_age_mul_sqrt_ap_hi", "log_age_div_sqrt_ap_hi", "log_age_mul_pow2_ap_hi", "log_age_div_pow2_ap_hi", "log_age_mul_ap_hi", "log_age_div_ap_hi", "sqrt_age_mul_log_ap_hi", "sqrt_age_div_log_ap_hi", "sqrt_age_mul_sqrt_ap_hi", "sqrt_age_div_sqrt_ap_hi", "sqrt_age_mul_pow2_ap_hi", "sqrt_age_div_pow2_ap_hi", "sqrt_age_mul_ap_hi", "sqrt_age_div_ap_hi", "pow2_age_mul_log_ap_hi", "pow2_age_div_log_ap_hi", "pow2_age_mul_sqrt_ap_hi", "pow2_age_div_sqrt_ap_hi", "pow2_age_mul_pow2_ap_hi", "pow2_age_div_pow2_ap_hi", "pow2_age_mul_ap_hi", "pow2_age_div_ap_hi", "age_mul_log_ap_hi", "age_div_log_ap_hi", "age_mul_sqrt_ap_hi", "age_div_sqrt_ap_hi", "age_mul_pow2_ap_hi", "age_div_pow2_ap_hi", "age_mul_ap_hi", "age_div_ap_hi", "log_height_mul_log_ap_hi", "log_height_div_log_ap_hi", "log_height_mul_sqrt_ap_hi", "log_height_div_sqrt_ap_hi", "log_height_mul_pow2_ap_hi", "log_height_div_pow2_ap_hi", "log_height_mul_ap_hi", "log_height_div_ap_hi", "sqrt_height_mul_log_ap_hi", "sqrt_height_div_log_ap_hi", "sqrt_height_mul_sqrt_ap_hi", "sqrt_height_div_sqrt_ap_hi", "sqrt_height_mul_pow2_ap_hi", "sqrt_height_div_pow2_ap_hi", "sqrt_height_mul_ap_hi", "sqrt_height_div_ap_hi", "pow2_height_mul_log_ap_hi", "pow2_height_div_log_ap_hi", "pow2_height_mul_sqrt_ap_hi", "pow2_height_div_sqrt_ap_hi", "pow2_height_mul_pow2_ap_hi", "pow2_height_div_pow2_ap_hi", "pow2_height_mul_ap_hi", "pow2_height_div_ap_hi", "height_mul_log_ap_hi", "height_div_log_ap_hi", "height_mul_sqrt_ap_hi", "height_div_sqrt_ap_hi", "height_mul_pow2_ap_hi", "height_div_pow2_ap_hi", "height_mul_ap_hi", "height_div_ap_hi", "log_weight_mul_log_ap_hi", "log_weight_div_log_ap_hi", "log_weight_mul_sqrt_ap_hi", "log_weight_div_sqrt_ap_hi", "log_weight_mul_pow2_ap_hi", "log_weight_div_pow2_ap_hi", "log_weight_mul_ap_hi", "log_weight_div_ap_hi", "sqrt_weight_mul_log_ap_hi", "sqrt_weight_div_log_ap_hi", "sqrt_weight_mul_sqrt_ap_hi", "sqrt_weight_div_sqrt_ap_hi", "sqrt_weight_mul_pow2_ap_hi", "sqrt_weight_div_pow2_ap_hi", "sqrt_weight_mul_ap_hi", "sqrt_weight_div_ap_hi", "pow2_weight_mul_log_ap_hi", "pow2_weight_div_log_ap_hi", "pow2_weight_mul_sqrt_ap_hi", "pow2_weight_div_sqrt_ap_hi", "pow2_weight_mul_pow2_ap_hi", "pow2_weight_div_pow2_ap_hi", "pow2_weight_mul_ap_hi", "pow2_weight_div_ap_hi", "weight_mul_log_ap_hi", "weight_div_log_ap_hi", "weight_mul_sqrt_ap_hi", "weight_div_sqrt_ap_hi", "weight_mul_pow2_ap_hi", "weight_div_pow2_ap_hi", "weight_mul_ap_hi", "weight_div_ap_hi", "log_age_mul_log_ap_lo", "log_age_div_log_ap_lo", "log_age_mul_sqrt_ap_lo", "log_age_div_sqrt_ap_lo", "log_age_mul_pow2_ap_lo", "log_age_div_pow2_ap_lo", "log_age_mul_ap_lo", "log_age_div_ap_lo", "sqrt_age_mul_log_ap_lo", "sqrt_age_div_log_ap_lo", "sqrt_age_mul_sqrt_ap_lo", "sqrt_age_div_sqrt_ap_lo", "sqrt_age_mul_pow2_ap_lo", "sqrt_age_div_pow2_ap_lo", "sqrt_age_mul_ap_lo", "sqrt_age_div_ap_lo", "pow2_age_mul_log_ap_lo", "pow2_age_div_log_ap_lo", "pow2_age_mul_sqrt_ap_lo", "pow2_age_div_sqrt_ap_lo", "pow2_age_mul_pow2_ap_lo", "pow2_age_div_pow2_ap_lo", "pow2_age_mul_ap_lo", "pow2_age_div_ap_lo", "age_mul_log_ap_lo", "age_div_log_ap_lo", "age_mul_sqrt_ap_lo", "age_div_sqrt_ap_lo", "age_mul_pow2_ap_lo", "age_div_pow2_ap_lo", "age_mul_ap_lo", "age_div_ap_lo", "log_height_mul_log_ap_lo", "log_height_div_log_ap_lo", "log_height_mul_sqrt_ap_lo", "log_height_div_sqrt_ap_lo", "log_height_mul_pow2_ap_lo", "log_height_div_pow2_ap_lo", "log_height_mul_ap_lo", "log_height_div_ap_lo", "sqrt_height_mul_log_ap_lo", "sqrt_height_div_log_ap_lo", "sqrt_height_mul_sqrt_ap_lo", "sqrt_height_div_sqrt_ap_lo", "sqrt_height_mul_pow2_ap_lo", "sqrt_height_div_pow2_ap_lo", "sqrt_height_mul_ap_lo", "sqrt_height_div_ap_lo", "pow2_height_mul_log_ap_lo", "pow2_height_div_log_ap_lo", "pow2_height_mul_sqrt_ap_lo", "pow2_height_div_sqrt_ap_lo", "pow2_height_mul_pow2_ap_lo", "pow2_height_div_pow2_ap_lo", "pow2_height_mul_ap_lo", "pow2_height_div_ap_lo", "height_mul_log_ap_lo", "height_div_log_ap_lo", "height_mul_sqrt_ap_lo", "height_div_sqrt_ap_lo", "height_mul_pow2_ap_lo", "height_div_pow2_ap_lo", "height_mul_ap_lo", "height_div_ap_lo", "log_weight_mul_log_ap_lo", "log_weight_div_log_ap_lo", "log_weight_mul_sqrt_ap_lo", "log_weight_div_sqrt_ap_lo", "log_weight_mul_pow2_ap_lo", "log_weight_div_pow2_ap_lo", "log_weight_mul_ap_lo", "log_weight_div_ap_lo", "sqrt_weight_mul_log_ap_lo", "sqrt_weight_div_log_ap_lo", "sqrt_weight_mul_sqrt_ap_lo", "sqrt_weight_div_sqrt_ap_lo", "sqrt_weight_mul_pow2_ap_lo", "sqrt_weight_div_pow2_ap_lo", "sqrt_weight_mul_ap_lo", "sqrt_weight_div_ap_lo", "pow2_weight_mul_log_ap_lo", "pow2_weight_div_log_ap_lo", "pow2_weight_mul_sqrt_ap_lo", "pow2_weight_div_sqrt_ap_lo", "pow2_weight_mul_pow2_ap_lo", "pow2_weight_div_pow2_ap_lo", "pow2_weight_mul_ap_lo", "pow2_weight_div_ap_lo", "weight_mul_log_ap_lo", "weight_div_log_ap_lo", "weight_mul_sqrt_ap_lo", "weight_div_sqrt_ap_lo", "weight_mul_pow2_ap_lo", "weight_div_pow2_ap_lo", "weight_mul_ap_lo", "weight_div_ap_lo", "log_ap_hi_mul_log_ap_lo", "log_ap_hi_div_log_ap_lo", "log_ap_hi_mul_sqrt_ap_lo", "log_ap_hi_div_sqrt_ap_lo", "log_ap_hi_mul_pow2_ap_lo", "log_ap_hi_div_pow2_ap_lo", "log_ap_hi_mul_ap_lo", "log_ap_hi_div_ap_lo", "sqrt_ap_hi_mul_log_ap_lo", "sqrt_ap_hi_div_log_ap_lo", "sqrt_ap_hi_mul_sqrt_ap_lo", "sqrt_ap_hi_div_sqrt_ap_lo", "sqrt_ap_hi_mul_pow2_ap_lo", "sqrt_ap_hi_div_pow2_ap_lo", "sqrt_ap_hi_mul_ap_lo", "sqrt_ap_hi_div_ap_lo", "pow2_ap_hi_mul_log_ap_lo", "pow2_ap_hi_div_log_ap_lo", "pow2_ap_hi_mul_sqrt_ap_lo", "pow2_ap_hi_div_sqrt_ap_lo", "pow2_ap_hi_mul_pow2_ap_lo", "pow2_ap_hi_div_pow2_ap_lo", "pow2_ap_hi_mul_ap_lo", "pow2_ap_hi_div_ap_lo", "ap_hi_mul_log_ap_lo", "ap_hi_div_log_ap_lo", "ap_hi_mul_sqrt_ap_lo", "ap_hi_div_sqrt_ap_lo", "ap_hi_mul_pow2_ap_lo", "ap_hi_div_pow2_ap_lo", "ap_hi_mul_ap_lo", "ap_hi_div_ap_lo")
addRemoveSelect(iterations=10000, XL=extendXYCols(my.fixData(XLL), features=fff), teach=function (XL, newdata=NULL) {
  params = lgbParams
  params$iters = 3
  my.train.lgb(XL, params, newdata)
}, startFeatures=lgb.features)
'

lgbParams$iters = 200
xgbParams$iters = 200

lgbAlg = lgbTrainAlgo(XLL, lgbParams)
xgbAlg = xgbTrainAlgo(XLL, xgbParams)

alg = meanAggregator(c(xgbAlg, lgbAlg), c(0.5, 0.5))
results = alg(XXX)
write(results, file='res/res.txt', sep='\n')
print('done')