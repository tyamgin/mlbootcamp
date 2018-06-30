my.gridSearch = function (XLL, teach, grid, folds=7, iters=6, verbose=F, use.newdata=F, folds.seed=777, train.seed=2) {
  minE = -1e10 ##TODO maxE actually
  for (i in 1:nrow(grid)) {
    params = grid[i, ]
    
    my.set.seed(folds.seed)
    val = validation.tqfold(XLL, teach(params), folds=folds, iters=iters, verbose=verbose, use.newdata=use.newdata, seed=train.seed)
    my.restore.seed()
    e = mean(val)
    params$SCORE_MEAN = e
    params$SCORE_SD = sd(val)
    
    print(params)
    
    if (e > minE) {
      minE = e
      selParams = params
    }
    gc()
  }
  print('-------------------------------')
  print(selParams)
  list(e=minE, params=selParams)
}

my.tuneSequential = function (XLL, func, tuneGrid, loops=1, ...) {
  selParams = tuneGrid
  minE = Inf
  for (i in 1:length(selParams)) {
    if (length(selParams[[i]]) > 1) {
      selParams[[i]] = selParams[[i]][1]
    }
  }
  for (lp in 1:loops) {
    for (i in 1:length(tuneGrid)) {
      if (nrow(expand.grid(tuneGrid)) > 1 && length(tuneGrid[[i]]) == 1) {
        next
      }
      
      curParams = selParams
      curParams[[i]] = tuneGrid[[i]]
      curParams = expand.grid(curParams)
      
      r = my.gridSearch(XLL, func, curParams, ...)
      if (r$e < minE) {
        minE = r$e
        for (j in 1:length(selParams))
          selParams[[j]] = r$params[[j]]
      }
      
      print(sprintf('Current best result: %f:', minE))
      print(selParams)
      
      if (nrow(expand.grid(tuneGrid)) == 1) {
        break
      }
    }
  }
}