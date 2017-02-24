my.rfe = function (XL) {
  XL = XL[1:15000,]
  
  sz = ncol(XL) - 1
  for (j in 2:sz) {
    for (k in 1:(j-1)) {
      num = XX[, j]
      denum = XX[, k]
      
      XL = insertXLCol(XL, num / (denum + ifelse(min(denum) == 0, 1, 0)))
      XL = insertXLCol(XL, num * denum)
    }
  }
  
  xxx = XL[, -ncol(XL),drop=F]
  aaa = XL[, ncol(XL)]
  aaa = factor(aaa, labels=c('a', 'b'))
  
  trControl=trainControl(
    method='cv', 
    number=2, 
    classProbs=TRUE, 
    summaryFunction=mnLogLoss
  )
  tuGrid = expand.grid(
    nrounds = 50, 
    max_depth = 3, 
    eta = 0.2, 
    gamma = 1, # WTF??
    colsample_bytree = 1, 
    min_child_weight = 1,
    subsample = 1
  )
  
  "
  model <- train(xxx, aaa, method='xgbTree', metric='logLoss', maximize=F, trControl=trControl, tuneGrid=tuGrid, verbose=F)
  pr = predict(model, xxx, type='prob')$b
  print( error.logloss(c(aaa)-1, pr) )
  "  
  
  ctrl <- rfeControl(functions=caretFuncs, method='cv', repeats=1, verbose=F)
  colnames(xxx) <- paste('col', 1:ncol(xxx), sep='')
  lmProfile <<- rfe(xxx, aaa, sizes=c(1:5, 10, 12), rfeControl=ctrl, method='xgbTree')
}