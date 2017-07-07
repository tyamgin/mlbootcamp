my.train.nnet = function (XL, params, newdata=NULL) {
  XL = unnameMatrix(XL)
  X = XL[, -ncol(XL)]
  Y = XL[, ncol(XL)]

  # https://github.com/dmlc/mxnet/blob/906c3585dd1f81bf4e966357e0e2db5cdbe4a315/R-package/R/mlp.R
  out_node = ifelse(params$out_activation == 'logistic', 1, 2)
  
  capture.output(
    model <- mx.mlp(X, Y, hidden_node=params$hidden_node, out_node=out_node, out_activation=params$out_activation,
                  num.round=params$num.round, array.batch.size=params$array.batch.size, array.layout="rowmajor", 
                  learning.rate=params$learning.rate, momentum=params$momentum, dropout=params$dropout,
                  eval.metric=mx.metric.rmse, activation=as.character(params$activation), verbose=F,
                  device=mx.cpu())
  )
  
  function (X) {
    r = predict(model, unnameMatrix(X))
    r[nrow(r), ]
  }
}

# https://www.kaggle.com/gmilosev/r-mxnet/code
# helper function
# normalizes log loss preds so that sum is always 1
# probably not needed here, but anyway
mLogLoss.normalize = function(p, min_eta=1e-15, max_eta = 1.0){
  #min_eta
  for(ix in 1:dim(p)[2]) {
    p[,ix] = ifelse(p[,ix]<=min_eta,min_eta,p[,ix]);
    p[,ix] = ifelse(p[,ix]>=max_eta,max_eta,p[,ix]);
  }
  #normalize
  for(ix in 1:dim(p)[1]) {
    p[ix,] = p[ix,] / sum(p[ix,]);
  }
  return(p);
}

# helper function
#calculates logloss
mlogloss = function(y, p, min_eta=1e-15,max_eta = 1.0){
  class_loss = c(dim(p)[2]);
  loss = 0;
  p = mLogLoss.normalize(p,min_eta, max_eta);
  for(ix in 1:dim(y)[2]) {
    p[,ix] = ifelse(p[,ix]>1,1,p[,ix]);
    class_loss[ix] = sum(y[,ix]*log(p[,ix]));
    loss = loss + class_loss[ix];
  }
  #return loss
  return (list("loss"=-1*loss/dim(p)[1],"class_loss"=class_loss));
}

# mxnet specific logloss metric
mx.metric.mlogloss <- mx.metric.custom("mlogloss", function(label, pred){
  p = t(pred)
  m = mlogloss(class.ind(label),p)
  -m$loss
})


nnetTrainAlgo = function (XL, params, newdata=NULL) {
  my.fixedDataTrain(XL, function (XL, newdata=NULL) {
    my.filledHolesTrain(XL, function (XL, newdata=NULL) {
      my.extendedColsTrain(XL, function (XL, newdata=NULL) {
        my.normalizedTrain(XL, function (XL, newdata=NULL) {
          my.train.nnet(XL, params, newdata)
        }, newdata=newdata)
      }, features=xgb.features, newdata=newdata)
    }, newdata=newdata)
  }, newdata=newdata)
}


my.train.et = function (XL, params, newdata=NULL) {
    model <- ctree(cardio ~ ., data=XL, controls = ctree_control(
      maxsurrogate=params$maxsurrogate, 
      mincriterion=params$mincriterion
    ))
      #train(X, Y, method='ctree', metric='logLoss',
      #             maximize=F, trControl=trControl,# numThreads=4, 
                   #ntree=params$ntree, nodesize=params$nodesize,
      #             tuneGrid=tuneGrid)
  
  function (X) {
    predict(model, X)[, 1]
  }
}

etTrainAlgo = function (XL, params, newdata=NULL) {
  my.fixedDataTrain(XL, function (XL, newdata=NULL) {
    my.filledHolesTrain(XL, function (XL, newdata=NULL) {
      my.extendedColsTrain(XL, function (XL, newdata=NULL) {
        my.normalizedTrain(XL, function (XL, newdata=NULL) {
          my.train.et(XL, params, newdata)
        }, newdata=newdata)
      }, features=xgb.features, newdata=newdata)
    }, newdata=newdata)
  }, newdata=newdata)
}
