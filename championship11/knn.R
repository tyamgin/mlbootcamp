my.train.knn = function (XL, params, newdata=NULL) {
  XL = unnameMatrix(XL)
  
  X = XL[, -ncol(XL)]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = factor(XL[, ncol(XL)], labels=c('a', 'b', 'c', 'd', 'e')[1:length(unique(XL[, ncol(XL)]))])
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    k=params$k
  )
  
  model <- train(X, Y, method='knn', metric='Accuracy',
                 maximize=F, trControl=trControl,
                 tuneGrid=tuneGrid)
  
  ret = function (X) {
    X = unnameMatrix(X)
    colnames(X) <- paste0('X', 1:ncol(X))
    predict(model, X, type='prob')
  }
  
  if (!is.null(newdata)) {
    ret = ret(newdata)
    rm(model)
    return( function (X) ret )
  }
  
  ret
}

knnTrainAlgo = function (XL, params, newdata=NULL) {
  idxes = rep(0, 223)
  for(i in intCols)
    idxes[i] = 1
  
  my.extendedColsTrain(XL, function(XL, newdata=NULL) {
    my.normalizedTrain(XL, function (XL, newdata=NULL) {
      my.train.knn(XL, params, newdata=newdata)
    }, newdata=newdata)
  }, idxes=idxes, pairs=F, angles=F, params$extra, newdata=newdata)
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