qwe = function (nrow, ncol) (ncol * (log(2*nrow/ncol) + 1) / nrow - log(0.05) / nrow^0.5) / 1

tqfoldEstimation = function(XL, teach, features, thr=0) { # TODO: for maximize only
  p = ncol(XL) - 1
  if (p <= 1)
    return(c(0, 0))
  
  my.set.seed(2874549) # need?
  #ff = function (ncol) (ncol * (log(2*nrow(XL)/ncol) + 1) / nrow(XL) - log(0.05) / nrow(XL))^0.5
  e1 = validation.tqfold.parallel(XL, teach, folds=rep(3,80), resample.seed=377575, algo.seed=53, features=features) - qwe(nrow(XL), length(features))
  e2 = NA
  if (e1 > thr)
    #e2 = validation.tqfold.parallel(XL, teach, folds=rep(3,150), resample.seed=4, algo.seed=52, features=features)
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
  
  est = estimate(XL[, feats.cut(features)], teach, features)
  print('started at')
  print(est)
  
  tries = c()
  dones = 0
  
  for (it in 1:iterations) {
    p = length(features) / (ncol(XL)-1)
    addOrRemove = sample(0:1, 1, F, c(0.9, 0.1))
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
      newEst = estimate(XL[, feats.cut(newFeatures)], teach, newFeatures, est[1])  
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
        print(c(est[1], est[1] + qwe(nrow(XL), length(features))))
        
        features = newFeatures
        est = newEst
        
        print(c(est[1], est[1] + qwe(nrow(XL), length(features))))
        
        intPts = c(intPts, newEst[1])
        extPts = c(extPts, newEst[2])
        iterPts = c(iterPts, it)
        
        dumpFeatures(features)
        
        plot(c(iterPts, iterPts), c(intPts, extPts), col=c(rep("green", length(intPts)), rep("red", length(extPts))), pch=20)
      }
    }
    cat()
    cat(paste0('[', i, '] ', ifelse(addOrRemove, '-', '+'), colnames(XL)[i], ' ', newEst[1], ' ', newEst[1] + qwe(nrow(XL), length(newFeatures)), ' ', iterations - it, " iterations remains\n"))
  }
  dumpFeatures(features)
}