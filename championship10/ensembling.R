eachPartition = function (n, func, s=1) {
  if (n == 2) {
    parts = 2
    for (i in 0:parts) {
      func(c(1/parts*i, 1-1/parts*i))
    }
  } else {
    stop("not implemented")
  }
}

my.ensemble.enumerate = function (XL) {
  
  ecache.models <<- list()
  
  set.seed(2708)
  validation.tqfold(XL, function (XL) {
    model1 = lgbTrainAlgo(XL)
    model2 = annetmagic
    
    ecache.models[[length(ecache.models) + 1]] <<- c(model1, model2)
    meanAggregator(c(model1, model2)) # что-то вернуть
  }, folds=2, iters=1, verbose=T)
  
  eachPartition(2, function (w) {
    ecache.models.it <<- 1
    set.seed(2708)
    print(c(w, mean(validation.tqfold(XL, function (XL) {
      res = meanAggregator(ecache.models[[ecache.models.it]], w)
      ecache.models.it <<- ecache.models.it + 1
      res
    }, folds=2, iters=1, verbose=F))))
  })
}