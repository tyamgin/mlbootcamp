require(DEoptim)

#max_depth=4, 
#nrounds=350, 
#gamma=5, 
#lambda=1,
#alpha=0.2,
#eta=0.06, 
#colsample_bytree=0.635,
#min_child_weight=2,
#subsample=1,

lower = c(2, 120,  0,   0,   0, 0.005, 0.3,   0, 0.3)
upper = c(5, 1200, 20,  10,  5,   0.4,   1,   15,  1)

fnmap_f <- function(x) {
  x[1] = round(x[1])
  x[2] = round(x[2])
  x
}

func <- function(x) {
  
  nextSeed = sample(1:10^5, 1)
  set.seed(666)
  r = mean(validation.tqfold(XLL, function (XL) {
    my.roundedTrain(XL, function (XL) {
      my.extendedColsTrain(XL, function(XL) {
        my.normalizedTrain(XL, function (XL) {
          my.train.xgb(XL, expand.grid(rowsFactor=1, iters=1, max_depth=x[1], nrounds=x[2], gamma=x[3], lambda=x[4], alpha=x[5], eta=x[6], 
                       colsample_bytree=x[7], min_child_weight=x[8], subsample=x[9], nthread=4, tree_method='exact'))
        })
      }, xeee)
    })
  }, folds=4, iters=4, verbose=F))
  set.seed(nextSeed)
  
  print(paste(c('call ', x, '(', r, ')'), collapse=' '))
  r
}

set.seed(1234)
outDEoptim <- DEoptim(func, lower, upper, DEoptim.control(NP = 30, itermax = 10000, F = 0.8, CR = 0.7), fnMap = fnmap_f)
## plot the output
plot(outDEoptim)
print(gl.cnt)
