my.train.knn = function (XL, params, newdata=NULL) {
  XL = unnameMatrix(XL)
  XL[which(is.infinite(XL))] = 0
  
  hash = my.matrixHash(XL)
  
  cache_filename = paste0('cache2/knn6_', hash)
  if ((my.enableCache == T || my.enableCache == 'readOnly') && file.exists(cache_filename)) {
    return(readRDS(cache_filename))
  }
  if (my.enableCache == 'readOnly') stop('my.train.knn cache is read only')
  
  X = XL[, -ncol(XL)]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b', 'c', 'd', 'e')[1:length(unique(XL[, ncol(XL)]))])
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    #k=params$k,
    kmax=params$k,
    kernel=params$kernel,
    distance=2
  )
  
  model <- train(X, Y, method=params$km, metric='Accuracy',
                 maximize=F, trControl=trControl,
                 tuneGrid=tuneGrid)
  
  ret = function (X) {
    X = unnameMatrix(X)
    X[which(is.infinite(X))] = 0
    colnames(X) <- paste0('X', 1:ncol(X))
    predict(model, X, type='prob')
  }
  
  if (!is.null(newdata)) {
    if (is.matrix(newdata) || is.data.frame(newdata))
      newdata = list(newdata)
    results = list()
    for (i in 1:length(newdata))
      results[[i]] = ret(newdata[[i]])
    
    rm(model)
    ret = function (X) {
      for (i in 1:length(newdata))
        if (my.matrixEquals(newdata[[i]], X))
          return( results[[i]] )
      
      stop('[knn] newdata is not available')
    }
  }
  
  if (my.enableCache == T) {
    saveRDS(ret, cache_filename)
  }
  
  ret
}

knnTrainAlgo = function (XL, params, newdata=NULL) {
  idxes = rep(0, 223)
  for(i in intCols)
    idxes[i] = 1
  pairs=c(1,1,0,1,1,1,0,1,1,0,1,1,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,
          0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
  
  pairs=c(1,1,1,1,1,1,0,1,1,0,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
          0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,1,1,0,0,0,0,
          0,0,1,0,0)
  
  my.extendedColsTrain(XL, function(XL, newdata=NULL) {
    my.normalizedTrain(XL, function (XL, newdata=NULL) {
      my.train.knn(XL, params, newdata=newdata)
    }, newdata=newdata)
  }, idxes=neee, pairs=pairs, angles=T, newdata=newdata)
}

knnTrainRoundAlgo = function (XL, params, newdata=NULL) {
  knn = knnTrainAlgo(XL, params, newdata)
  function (X) {
    r = knn(X)
    ret = rep(-1, nrow(r))
    for (i in 1:length(ret)) {
      if (1 %in% r[i, ]) {
        ret[i] = which.max(r[i, ]) - 1
      }
    }
    ret
  }
}

"knnEtTrainAlgo = function (XL, params, newdata=NULL) {
knn = knnTrainAlgo(XL, params, newdata=newdata)
et = etTrainAlgo(XL, params, newdata=newdata)

function (X) {
r = et(X)
kr = knn(X)
return(r)
for (i in 1:nrow(r)) {
if (1 %in% kr[i, ]) {
r[i, ] = kr[i, ]
}
}
r
}
}"

knnEtTrainAlgo = function (XL, params, newdata=NULL) {
  knn = knnTrainAlgo(XL, params, newdata=newdata)
  et = etTrainAlgo(XL, params, newdata=newdata)
  
  function (X) {
    r = et(X)
    kr = knn(X)
    return(r)
    for (i in 1:nrow(r)) {
      if (1 %in% kr[i, ]) {
        r[i, ] = kr[i, ]
      }
    }
    r
  }
}