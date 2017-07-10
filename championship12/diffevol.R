require(DEoptim)

my.diffevol.run = function (XL, trainFunc, params) {

  lower = sapply(params, function (x) {
    x[1]
  })
  upper = sapply(params, function (x) {
    if (length(x) <= 1)
      return( x[1] )
    x[2]
  })

  fnmap_f = function(x) {
    for (i in 1:length(params)) {
      if (length(params[[i]]) >= 3 && params[[i]][3] == 'int') {
        x[i] = as.integer(round(x[i]))   
      }
    }
    x
  }

  func = function(x) {
  
    nextSeed = sample(1:10^5, 1)
    set.seed(666)
    r = mean(validation.tqfold(XLL, function (XL) {
      p = params
      for (i in 1:length(p)) {
        p[[i]] = x[i]
      }
      
      trainFunc(XL, as.data.frame(p))
    }, folds=3, iters=1, verbose=F))
    set.seed(nextSeed)
    
    print(paste(c('call ', x, '(', r, ')'), collapse=' '))
    r
  }
  outDEoptim <- DEoptim(func, unlist(lower), unlist(upper), DEoptim.control(NP = 50, itermax = 10000, F = 0.01, CR = 0.9), fnMap = fnmap_f)
}


my.diffevol.xgb.params = list(
  iters=3,
  rowsFactor=1,
  
  max_depth=c(4), 
  gamma=c(0, 20),
  lambda=c(0, 20),
  alpha=c(0, 40),
  eta=c(0.005, 1),
  subsample=c(0.4, 1),
  colsample_bytree=c(0.4, 1),
  min_child_weight=c(0.1, 100),
  nthread=4, 
  nrounds=list(40, 500, 'int'),
  early_stopping_rounds=0,
  num_parallel_tree=1
)
set.seed(123456)
my.diffevol.run(XL, xgbTrainAlgo, my.diffevol.xgb.params)