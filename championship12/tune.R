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
    }
  }
}