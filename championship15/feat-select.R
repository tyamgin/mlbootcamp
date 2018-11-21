tqfoldEstimation = function(XL, teach) {
  p = ncol(XL) - 1
  if (p <= 1)
    return( list(int=0, ext=0) )
  
  my.set.seed(227449) # need?
  e = validation.tqfold.parallel(XL, teach, folds=5, iters=16, resample.seed=3934234, algo.seed=5244)
  my.restore.seed()
  list(int=e, ext=e)
}

extendXYCols = function (XL, features) {
  X = XL[, features]
  if ("Y" %in% colnames(XL))
    X = cbind(X, XL[, 'Y', drop=F])
  X
}

dumpFeatures = function (f) {
  print(paste(lapply(f, function (x) paste0("'", x, "'")), collapse=","))
}

addRemoveSelect = function(iterations,  # количество итераций
                           XL, # выборка
                           teach, # teach(XL) - обучение
                           estimate = tqfoldEstimation, # оценка
                           startFeatures = c(),
                           minimize = F
) {
  L = nrow(XL)
  
  features = startFeatures
  
  iterPts = c()
  intPts = c()
  extPts = c()
  
  est = estimate(extendXYCols(XL, features=features), teach)
  print('started at')
  print(est)
  
  tries = c()
  
  for (it in 1:iterations) {
    p = length(features) / ncol(XL)
    addOrRemove = sample(0:1, 1, F, c(1 - p, p))
    #addOrRemove = 1
    # 0 - add
    # 1 - remove
    i = -1
    for (k in sample(ncol(XL) - 1)) {
      if (addOrRemove == 1 && !(colnames(XL)[k] %in% features)) # фичи и так нету
        next
      
      if (addOrRemove == 0 && (colnames(XL)[k] %in% features)) # фича и так есть
        next
      
      if (k %in% tries) # уже пробовали
        next
      
      i = k
      break
    }
    if (i == -1) {
      if (length(tries) == length(features)) {
        print('stopped')
        break
      } else {
        print('skipping step')
        next
      }
    }
    tries = c(tries, i)
    
    newFeatures = features
    if (colnames(XL)[i] %in% features)
      newFeatures = newFeatures[newFeatures != colnames(XL)[i]]
    else
      newFeatures = c(newFeatures, colnames(XL)[i])
    
    newEst = estimate(extendXYCols(XL, features=newFeatures), teach)
    
    if (minimize && newEst$ext < est$ext || !minimize && newEst$ext > est$ext) {
      tries = c()
      print(i)
      print(est)
      
      features = newFeatures
      est = newEst
      
      print(est)
      
      intPts = c(intPts, newEst$int)
      extPts = c(extPts, newEst$ext)
      iterPts = c(iterPts, it)
      
      dumpFeatures(features)
      
      plot(c(iterPts, iterPts), c(intPts, extPts), col=c(rep("green", length(intPts)), rep("red", length(extPts))), pch=20)
    }
    cat()
    cat(paste0('[', i, '] ', ifelse(addOrRemove, '-', '+'), colnames(XL)[i], ' ', newEst$ext, ' ', iterations - it, " iterations remains\n"))
  }
}