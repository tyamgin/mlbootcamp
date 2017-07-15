tqfoldEstimation = function(XL, teach) {
  p = ncol(XL) - 1
  if (p <= 1)
    return( list(int=1e10+p, ext=1e10+p) )
  
  my.set.seed(11341)
  e = mean(validation.tqfold(XL, teach, folds=5, iters=7, verbose=F))
  my.restore.seed()
  list(int=e, ext=e)
}

addRemoveSelect = function(iterations,  # количество итераций
                           XL, # выборка
                           teach, # teach(XL) - обучение
                           estimate = tqfoldEstimation, # оценка
                           startFeatures = c()
) {
  L = nrow(XL)
  size = ncol(XL) - 1
  
  features = startFeatures
  
  iterPts = c()
  intPts = c()
  extPts = c()
  
  est = estimate(extendXYCols(XL, features=features), teach)
  print('started at')
  print(est)
  
  tries = c()
  
  for (it in 1:iterations) {
    addOrRemove = sample(0:1, 1)
    #addOrRemove = 1
    # 0 - add
    # 1 - remove
    i = -1
    for (k in sample(size)) {
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
    
    if (newEst$ext < est$ext) {
      tries = c()
      print(i)
      print(est)
      
      features = newFeatures
      est = newEst
      
      print(est)
      
      intPts = c(intPts, newEst$int)
      extPts = c(extPts, newEst$ext)
      iterPts = c(iterPts, it)
      
      print(features)
      
      plot(c(iterPts, iterPts), c(intPts, extPts), col=c(rep("green", length(intPts)), rep("red", length(extPts))), pch=20)
    }
    print(paste0('[', i, '] ', newEst$ext, ' ', iterations - it, " iterations remains"))
  }
}