require(DEoptim)


#num_leaves=c(13),
#max_depth=c(4),
#lambda_l2=c(0),
#learning_rate=c(0.09),
#feature_fraction=c(0.65),
#min_data_in_leaf=c(55),
#bagging_fraction=c(0.8),
#nrounds=c(175),

lower = c(5,  3,  0,  0.005,  0.4, 1,   0.4, 30)
upper = c(30, 7, 20,  1,      1,   400, 1,   500)

fnmap_f <- function(x) {
  for (i in c(1,2,6,8))
    x[i] = as.integer(round(x[i]))
  
  x
}

func <- function(x) {
  
  if (x[[6]] != round(x[[6]])) {
    x = x
  }
  
  my.set.seed(666)
  nextSeed = sample(1:10^5, 1)
  set.seed(666)
  r = mean(validation.tqfold(XLL, function (XL) {
    lgbTrainAlgo(XL, expand.grid(
      iters=3,
      rowsFactor=1,
      
      num_leaves=x[[1]],
      max_depth=x[[2]],
      lambda_l2=x[[3]],
      learning_rate=x[[4]],
      feature_fraction=x[[5]],
      min_data_in_leaf=x[[6]],
      bagging_fraction=x[[7]],
      nrounds=x[[8]],
      early_stopping_rounds=0,
      nthread=4 
    ))
  }, folds=5, iters=3, verbose=F))
  set.seed(nextSeed)
  
  print(paste(c('call ', x, '(', r, ')'), collapse=' '))
  r
}

set.seed(1234)
outDEoptim <- DEoptim(func, lower, upper, DEoptim.control(NP = 35, itermax = 10000, F = 0.8, CR = 0.7), fnMap = fnmap_f)
## plot the output
plot(outDEoptim)
print(gl.cnt)
