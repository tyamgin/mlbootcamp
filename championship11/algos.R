my.boot = function (XLL, train, aggregator, iters=10, rowsFactor=0.3, replace=F, nthread=1) {
  n = nrow(XLL)
  
  if (nthread > 1) {
    cl <- makeCluster(nthread)
    registerDoParallel(cl)
  }
  
  algos = foreach(it=1:iters, .export=my.dopar.exports, .packages=my.dopar.packages) %do% {
    sampleIdxes = sample(n, rowsFactor*n, replace=replace)
    
    XK = XLL[-sampleIdxes, ]
    XL = XLL[sampleIdxes, ]  
    
    if (it %% 20 == 0)
      gc()
    
    train(XL, XK)
  }
  
  if (nthread > 1) {
    stopCluster(cl)
  }
  
  if (is.character(aggregator) || is.factor(aggregator)) {
    do.call(as.character(aggregator), list(algos))
  } else if (is.function(aggregator)) {
    aggregator(algos)
  } else {
    stop('invalid aggregator type')
  }
}

error.accuracy = function (act, pred) {
  mean(act != pred)
}

validation.tqfold.enumerate = function (callback, XLL, folds=5, iters=10) {
  resamples = unname(foreach(it=1:iters, .combine=rbind) %do% sample(nrow(XLL)))
  for (it in 1:iters) {
    perm = resamples[it, ]
    for (fold in 1:folds) {
      foldLength = floor(nrow(XLL) / folds)
      foldStart = (fold - 1) * foldLength
      foldEnd = foldStart + foldLength - 1
      
      controlIdxes = perm[foldStart:foldEnd]
      XK = XLL[controlIdxes, ]
      XL = XLL[-controlIdxes, ]  
      
      callback(XL, XK, it, fold)
    }
  }
}

validation.tqfold = function (XLL, teachFunc, folds=5, iters=10, verbose=F) {
  XKerr = c()
  
  nrows = length(unique(XLL[, ncol(XLL)]))
  tmp.errorMat <<- matrix(0, nrows, nrows)
  draw = function() {
    if (verbose) {
      df = expand.grid(act = 0:(nrows - 1), pred = 0:(nrows - 1))
      df$value <- c(tmp.errorMat)
      df$color <- c(ifelse(diag(nrows) == 1, 'f', 'o'))
      
      g <- (ggplot(df, aes(act, pred, colour=color)) 
            + geom_point(aes(size = value)) 
            + theme_bw() 
            + theme(legend.position="none")
            + xlab("Actual") 
            + ylab("Prediction")
            + scale_colour_manual(breaks=df$color, values=c("black", "green", "red"))
            + scale_size_continuous(range=c(10,30))
            + geom_text(aes(label=value, color=rep('black', nrows^2)))
      )
      print(g)
    }
  }
  
  validation.tqfold.enumerate(function (XL, XK, it, fold) {
    algo = teachFunc(XL)
    act = XK[, ncol(XL)]
    pred = algo(XK[, -ncol(XL)])
    e = error.accuracy(act, pred)
    
    for(i in 1:length(act)) {
      tmp.errorMat[act[i] + 1, pred[i] + 1] <<- tmp.errorMat[act[i] + 1, pred[i] + 1] + 1
    }
    
    XKerr <<- c(XKerr, e)
    
    if (verbose)
      print(paste0('tqfold ', it, '-', fold, '/', iters, '-', folds, ' cur=', e, ' mean=', mean(XKerr), ' sd=', sd(XKerr)))
    
    draw()
    
  }, XLL, folds=folds, iters=iters)
  
  XKerr
}


my.gridSearch = function (XLL, teach, grid, folds=7, iters=6, verbose=F) {
  minE = 1e10
  for (i in 1:nrow(grid)) {
    params = grid[i, ]
    print(params)
    e = mean(validation.tqfold(XLL, teach(params), folds=folds, iters=iters, verbose=verbose))
    print(e)
    if (e < minE) {
      minE = e
      selParams = params
    }
  }
  print('-------------------')
  print(selParams)
  print(minE)
}

meanAggregator = function (baseAlgos, w=NULL) {
  l = length(baseAlgos)
  if (is.null(w))
    w = rep(1/l, l)
  function(x) {
    s = 0
    for (i in 1:l) {
      if (is.function(baseAlgos[[i]])) {
        s = s + w[i] * baseAlgos[[i]](x)
      } else {
        s = s + w[i] * predict(baseAlgos[[i]], x)
      }
    }
    s
  }
}

gmeanAggregator = function (baseAlgos) {
  l = length(baseAlgos)
  function(x) {
    s = 1
    for (algo in baseAlgos) {
      if (is.function(algo)) {
        s = s * algo(x);
      } else {
        s = s * predict(algo, x);
      }
    }
    s ^ (1/l)
  }
}