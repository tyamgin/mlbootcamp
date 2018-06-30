library(plyr)  
library(dplyr)  
library(caret)  
library(e1071)
library(lightgbm)
library(foreach)
library(pROC)

debugSource("cv.R")
debugSource("tune.R")

#XLL = readRDS('data/data_t.rds')
#train_answers = read.table(file="data/mlboot_train_answers.tsv", sep='\t', head=T)
#test_cuids = read.table(file="data/mlboot_test.tsv", sep='\t', head=T)
#j1_features = readRDS('data/j1_reatures.rds')
#j3_features = readRDS('data/j3_reatures.rds')
#XY_all = left_join(XLL, train_answers, by="cuid")
#XY_all = cbind(XY_all, j1_features, j3_features)

create_features = function (XG, remove.cuid=T) {
  XG$cat0 = as.integer(XG$cat_feature == 0)
  XG$cat1 = as.integer(XG$cat_feature == 1)
  XG$cat2 = as.integer(XG$cat_feature == 2)
  XG$cat3 = as.integer(XG$cat_feature == 3)
  XG$cat4 = as.integer(XG$cat_feature == 4)
  XG$cat5 = as.integer(XG$cat_feature == 5)
  target_exists = "target" %in% colnames(XG)
  if (!target_exists) {
    XG$target = NA
  }
  grp = XG %>% group_by(cuid) 
  XG1 = grp %>% summarise(
      count=n(),
      cat0=sum(cat0), cat1=sum(cat1), cat2=sum(cat2), cat3=sum(cat3), cat4=sum(cat4), cat5=sum(cat5),
      dt_diff=mean(dt_diff),
      j1c=sum(j1c), j2c=sum(j2c), j3c=sum(j3c)
    )
  XG2 = grp %>% 
    select(c('cuid', grep('j[1-3]_[0-9]', colnames(.)))) %>%
    summarise_all(funs(sum)) %>%
    select(-cuid)
    
  XG = cbind(XG1, XG2, grp %>% summarise(target=max(target)) %>% select(-cuid))
  
  if (remove.cuid) {
    XG = select(XG, -cuid)
  }
  if (!target_exists) {
    XG = select(XG, -target)
  }
  XG
}

my.train.lm = function (XL, params) {
  model = lm(target~., XL)
  
  function (X) {
    predict(model, X)
  }
}

my.train.glm = function (XL, params, newdata=NULL) {
  X = XL[, -ncol(XL), drop=F]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = factor(XL[, ncol(XL), drop=T], labels=c('a', 'b'))
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = NULL
  
  capture.output(
    model <- train(X, Y, method='glm',
                   maximize=F, trControl=trControl,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    colnames(X) <- paste0('X', 1:ncol(X))
    predict(model, X, type='prob')$b
  }
}

my.train.nnet = function (XL, params, newdata=NULL) {
  X = XL[, -ncol(XL), drop=F]
  colnames(X) <- paste0('X', 1:ncol(X))
  Y = factor(XL[, ncol(XL), drop=T], labels=c('a', 'b'))
  
  trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
  
  tuneGrid = expand.grid(
    size=5,
    decay=100
  )
  
  
  capture.output(
    model <- train(X, Y, method='nnet', metric='ROC',
                   maximize=F, trControl=trControl,
                   maxit=200,
                   tuneGrid=tuneGrid)
  )
  
  function (X) {
    colnames(X) <- paste0('X', 1:ncol(X))
    predict(model, X, type='prob')$b
  }
}

my.train.lgb = function (XLL, params) {
  XLL = as.matrix(XLL)
  
  ret = my.boot(XLL, function (XL, XK) {
    dtrain = lgb.Dataset(data=XL[, -ncol(XL), drop=F], label=XL[, ncol(XL), drop=T], free_raw_data=FALSE)
    
    if (params$early_stopping_rounds <= 0) {
      early_stopping_rounds = NULL
      valids = list()
    } else {
      dtest = lgb.Dataset(data=XK[, -ncol(XK)], label=XK[, ncol(XK)], free_raw_data=FALSE)
      valids = list(train=dtrain, test=dtest)
      early_stopping_rounds = params$early_stopping_rounds
    }
    
    model = lgb.train( 
      data=dtrain, 
      num_leaves=params$num_leaves, 
      max_depth=params$max_depth, 
      learning_rate=params$learning_rate,
      nrounds=params$nrounds,
      min_data_in_leaf=params$min_data_in_leaf, 
      lambda_l2=params$lambda_l2,
      feature_fraction=params$feature_fraction,
      bagging_fraction=params$bagging_fraction,
      valids=valids, 
      early_stopping_rounds=early_stopping_rounds,
      objective='binary', 
      metric='auc',
      verbose=0, 
      feature_fraction_seed=sample(1:1000, 1),
      bagging_seed=sample(1:1000, 1),
      nthread=params$nthread
    )
    function (X) {
      X = as.matrix(X)
      predict(model, X)
    }
  }, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
  
  ret
}


algo1 = function (XL) {
  
  model = my.train.lm(XL)
  function (X) {
  
    model(X)
  }
}


XL = XY_all[!is.na(XY_all$target),]

XX = XY_all[is.na(XY_all$target),]
XX = subset(XX, select=-c(target))
XX = create_features(XX, remove.cuid=F)

XL2 = create_features(XL)



lgbParams = list(
  iters=1,
  rowsFactor=1,
  
  num_leaves=5:16,
  nrounds=c(30,50,60,70),
  learning_rate=c(0.05, 0.1, 0.15, 0.2, 0.25, 0.4),
  
  max_depth=c(3,4,5,6),
  lambda_l2=c(7,10,13),
  feature_fraction=c(0.5,0.6,0.746088,0.8),
  min_data_in_leaf=c(50,100,200,382,400),
  bagging_fraction=c(0.7,0.8,0.910187,0.99),
  early_stopping_rounds=0,
  nthread=4
)

#my.tuneSequential(XL2, function (params) {
#  function (XL, newdata) {
#    my.train.lgb(XL, params)
#  }
#}, lgbParams, verbose=T, loops=2, iters=3, folds=5, use.newdata=F)
#lol()

#validation.tqfold(XL2, algo1, folds=5, iters=3, verbose=T, seed=2707); asdasd()

model = algo1(XL2)

XX$target = model(select(XX, -cuid))
R = left_join(test_cuids, XX, "cuid")
write(R$target, file="res/result.txt", sep='\n')
XX = select(XX, -target)