my.stacking = function (X, Y, teachers, aggregate) {
  models = foreach(teacher=teachers) %do% {
    teacher(unname(cbind(X, Y)))
  }
  
  extend = function (X) {
    answers = foreach(model=models, .combine=cbind) %do% model(X)
    unname(answers)
  }
  
  X = extend(X)
  finalModel = aggregate(cbind(X, Y))
  
  function (x) {
    x = extend(x)
    finalModel(x)
  }
}

#knnTrainAlgo, svmTrainAlgo, nnetTrainAlgo, glmTrainAlgo, lgbTrainAlgo

stackedAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    my.stacking(XL[, -ncol(XL)], XL[, ncol(XL)], c(knnTrainAlgo, svmTrainAlgo, nnetTrainAlgo, glmTrainAlgo, lgbTrainAlgo), lgb2TrainAlgo)
  })
}

my.stacking2 = function (X, Y, models, aggregate) {
  extend = function (X) {
    answers = foreach(model=models, .combine=cbind) %dopar% {
      model(X)
    }
    cbind(X, answers) 
  }
  
  X = extend(X)
  finalModel = aggregate(cbind(X, Y))
  
  function (x) {
    x = extend(x)
    finalModel(x)
  }
}

glmTrainAlgo = function (XL) {
  my.normalizedTrain(XL, function (XL) {
    X = XL[, -ncol(XL)]
    Y = factor(XL[, ncol(XL)], labels=c('a', 'b'))
    
    tuneGrid = NULL # there is no tuning parameters
    trControl = trainControl(method='none', number=5, classProbs=T, summaryFunction=mnLogLoss)
    #capture.output(
      model <- train(X, Y, method='glm', metric='logLoss', maximize=F, 
                     trControl=trControl, tuneGrid=tuneGrid, family=binomial)
    #)
    
    mglm <<- model
    #print(model)
    
    function (X) {
      predict(model, X, type='prob')$b
    }
  })
}