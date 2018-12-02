tqfoldEstimation = function(XL, teach, features, thr=0) { # TODO: for maximize only
  p = ncol(XL) - 1
  if (p <= 1)
    return(c(0, 0))
  
  my.set.seed(2874549) # need?
  e1 = validation.tqfold.parallel(XL, teach, folds=1:4, folds.mult=10, resample.seed=324, algo.seed=52, features=features, timefolds=T)
  e2 = NA
  if (e1 > thr)
    #e2 = validation.tqfold.parallel(XL, teach, folds=5, iters=1, resample.seed=4, algo.seed=52, features=features)
    e2=e1
  my.restore.seed()
  c(e1, e2)
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
                           onlyFeatures = NULL,
                           minimize = F
) {
  L = nrow(XL)
  
  features = startFeatures
  
  iterPts = c()
  intPts = c()
  extPts = c()
  
  est = estimate(XL, teach, features)
  print('started at')
  print(est)
  
  tries = c()
  dones = 0
  
  for (it in 1:iterations) {
    p = length(features) / (ncol(XL)-1)
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
      
      if (!is.null(onlyFeatures) && !(colnames(XL)[k] %in% onlyFeatures))
        next
      
      if (k %in% tries) # уже пробовали
        next
      
      i = k
      break
    }
    if (i == -1) {
      if (length(tries) == ncol(XL) - 1) {
        if (dones > 0) {
          print('stopped')
          break
        } else {
          dones = dones + 1
          tries = c()
          next
        }
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
    
    possibleError = tryCatch({
      newEst = estimate(XL, teach, newFeatures, est[1])  
    }, error=function(err) {
      print(err)
    })
    
    if (inherits(possibleError, "error")) {
      next
    }
    
    if (all(!is.na(newEst))) {
      if (minimize && all(newEst < est) || !minimize && all(newEst > est) || addOrRemove == 1 && all(newEst == est)) {
        #tries = c()
        dones = 0
        print(i)
        print(est)
        
        features = newFeatures
        est = newEst
        
        print(est)
        
        intPts = c(intPts, newEst[1])
        extPts = c(extPts, newEst[2])
        iterPts = c(iterPts, it)
        
        dumpFeatures(features)
        
        plot(c(iterPts, iterPts), c(intPts, extPts), col=c(rep("green", length(intPts)), rep("red", length(extPts))), pch=20)
      }
    }
    cat()
    cat(paste0('[', i, '] ', ifelse(addOrRemove, '-', '+'), colnames(XL)[i], ' ', newEst[1], ' ', newEst[2], ' ', iterations - it, " iterations remains\n"))
  }
}