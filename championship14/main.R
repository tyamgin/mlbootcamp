options(java.parameters = "-Xmx16000m")
library(plyr)  
library(dplyr)  
library(caret)  
library(e1071)
library(lightgbm)
library(foreach)
library(pROC)
library(Matrix)
library(glmnet)
#library(irlba)
library(RSpectra)

debugSource("cv.R")
debugSource("tune.R")

set.seed(888)

j1_features = readRDS('data/j1_features.rds')[, 1:200]
j2_features = readRDS('data/j2_features.rds')[, 1:200]
j3_features = readRDS('data/j3_features.rds')[, 1:200]

#j1_sp = readRDS('data/data_j1_sp.rds')
#j2_sp = readRDS('data/data_j2_sp.rds')
#j3_sp = readRDS('data/data_j3_sp.rds')

j1_sp_rest = readRDS('data/j1_svd_rest_50.rds')$u
j2_sp_rest = readRDS('data/j2_svd_rest_50.rds')$u
j3_sp_rest = readRDS('data/j3_svd_rest_50.rds')$u

#j1_sp_rest = j1_sp[, -(as.integer(substr(colnames(j1_features), 4, 100)) + 1)]
#j1_sp_rest = j1_sp_rest[, colSums(j1_sp_rest) > 1]
#j1_features = j1_sp = NULL
#j1_svd_rest_50 = svds(j1_sp_rest, 50)
#saveRDS(j1_svd_rest_50, 'data/j1_svd_rest_50.rds')

#j2_sp_rest = j2_sp[, -(as.integer(substr(colnames(j2_features), 4, 100)) + 1)]
#j2_sp_rest = j2_sp_rest[, colSums(j2_sp_rest) > 1]
#j2_features = j2_sp = NULL
#j2_svd_rest_50 = irlba::svdr(j2_sp_rest, 50)
#saveRDS(j2_svd_rest_50, 'data/j2_svd_rest_50.rds')

#j3_sp_rest = j3_sp[, -(as.integer(substr(colnames(j3_features), 4, 100)) + 1)]
#j3_sp_rest = j3_sp_rest[, colSums(j3_sp_rest) > 1]
#j3_features = j3_sp = NULL
#j3_svd_rest_50 = svds(j3_sp_rest, 50)
#saveRDS(j3_svd_rest_50, 'data/j3_svd_rest_50.rds')



#j1_sp = j1_sp[, colSums(j1_sp) > 1]
#j2_sp = j2_sp[, colSums(j2_sp) > 1]
#j3_sp = j3_sp[, colSums(j3_sp) > 1]

#j1_u = readRDS('data/j1_u.rds')
#colnames(j1_u) = paste0('j1u_', 1:ncol(j1_u))
#j2_u = readRDS('data/j2_u.rds')
#colnames(j2_u) = paste0('j2u_', 1:ncol(j2_u))
#j3_u = readRDS('data/j3_u.rds')
#colnames(j3_u) = paste0('j3u_', 1:ncol(j3_u))

XLL = readRDS('data/data_t.rds')
train_answers = readRDS('data/train_answers.rds')
test_cuids = readRDS('data/test_cuids.rds')
XY_all = left_join(XLL, train_answers, by="cuid")

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
  XG = NULL
  XG1 = grp %>% summarise(
      count=n(),
      cat0=sum(cat0), cat1=sum(cat1), cat2=sum(cat2), cat3=sum(cat3), cat4=sum(cat4), cat5=sum(cat5),
      dt_diff_mean=mean(dt_diff),
      dt_diff_min=min(dt_diff),
      dt_diff_max=max(dt_diff),
      j1s=sum(j1s), j2s=sum(j2s), j3s=sum(j3s)
    )
  tar = grp %>% summarise(target=max(target)) %>% select(-cuid) %>% as.matrix()
  grp = NULL
  
  cuid = XG1$cuid
  XG1 = as.matrix(XG1)
  
  print('before cbind')
  
  XG = cbind(
    XG1, 
    #j1_sp[cuid, ],
    #j2_sp[cuid, ],
    #j3_sp[cuid, ],
    #j1_u[cuid, ],
    #j2_u[cuid, ],
    #j3_u[cuid, ],
    j1_features[cuid, ],
    j2_features[cuid, ],
    j3_features[cuid, ],
    j1_sp_rest[cuid, ],
    j2_sp_rest[cuid, ],
    j3_sp_rest[cuid, ],
    tar
  )
  
  print('after cbind')
  
  if (remove.cuid) {
    #XG = select(XG, -cuid)
    XG = XG[, colnames(XG) != 'cuid']
  }
  if (!target_exists) {
    #XG = select(XG, -target)
    XG = XG[, colnames(XG) != 'target']
  }
  XG
}

my.train.lm = function (XL, params) {
  #model = MatrixModels:::lm.fit.sparse(XL[, -ncol(XL), drop=F], XL[, ncol(XL), drop=T])
  lambda = 0.04709416
  model = glmnet(XL[, -ncol(XL), drop=F], XL[, ncol(XL), drop=T],
                 family='binomial',type.logistic='Newton', type.multinomial='ungrouped', 
                 lambda=lambda, alpha=0)
  
  function (X) {
    #r=predict(model, X, type="response")
    #bestlam <- model$lambda.max
    r2=predict(model, X, type="response", s=lambda)
    r2[,1]
  }
}

my.train.lm2 = function (XL, params) {
  model = lm(target~., as.data.frame(XL))
  
  function (X) {
    predict(model, as.data.frame(X))
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

my.train.et = function (XL, params) {
  ret = my.boot(XL, function (XL, XK) {
    X = XL[, -ncol(XL), drop=F]
    colnames(X) <- paste0('X', 1:ncol(X))
    Y = factor(XL[, ncol(XL), drop=T], labels=c('a', 'b'))
    
    trControl = trainControl(method='none', classProbs=T, summaryFunction=defaultSummary)
    
    tuneGrid = expand.grid(
      numRandomCuts=params$numRandomCuts,
      mtry=params$mtry
    )
    
    model <- train(X, Y, method='extraTrees', metric='ROC',
                   maximize=T, trControl=trControl,
                   ntree=params$ntree,
                   nodesize=params$nodesize,
                   numThreads=4,
                   tuneGrid=tuneGrid)
    
    function (X) {
      colnames(X) <- paste0('X', 1:ncol(X))
      predict(model, X, type='prob')$b
    }
  }, aggregator='meanAggregator', iters=params$iters, rowsFactor=params$rowsFactor, replace=F, nthread=1)
  
  ret
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
      verbose=-1, 
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
  #my.train.lm(XL)
  #my.train.nnet(XL)
  my.train.lgb(XL, lgbParams)
  #my.train.et(XL, etParams)
}

XL2 = XX = NULL
gc()

print('before create features')
XL2 = XY_all[!is.na(XY_all$target),] %>% create_features()
print('after create features')

XX = XY_all[is.na(XY_all$target),] %>%
  select(-target) %>%
  create_features(remove.cuid=F)

j1_features = j2_features = j3_features = NULL
j1_sp = j2_sp = j3_sp = NULL
j1_u = j2_u = j3_u = NULL
j1_sp_rest = j2_sp_rest = j3_sp_rest = NULL

print('preparing complete')

etParams = list(
  numRandomCuts=c(1),
  mtry=c(2),
  ntree=c(250),
  nodesize=1,
  iters=1,
  rowsFactor=1
)

lgbParams = list(
  iters=1,
  rowsFactor=1,
  
  num_leaves=c(10),
  nrounds=c(420),
  learning_rate=c(0.05),
  
  max_depth=c(6),
  lambda_l2=c(10),
  feature_fraction=0.6,
  min_data_in_leaf=382,
  bagging_fraction=0.910187,
  early_stopping_rounds=0,
  nthread=4
)

#XY_all = NULL

#rr = cv.glmnet(XL2[, -ncol(XL2), drop=F], XL2[, ncol(XL2), drop=T], 
#               nfolds=5, type.measure="auc",
#               family='binomial',type.logistic='Newton', type.multinomial='ungrouped', 
#               alpha=0)

#my.tuneSequential(XL2, function (params) {
#  function (XL, newdata) {
#    my.train.lgb(XL, params)
#  }
#}, lgbParams, verbose=T, loops=1, iters=1, folds=5, train.seed=2707, folds.seed=888, use.newdata=F)
#lol()

#set.seed(888)
#validation.tqfold(XL2, algo1, folds=5, iters=3, verbose=T, seed=2707); asdasd()

model = algo1(XL2)

XX = as.data.frame(XX)
XX$target = model(select(XX, -cuid))
R = left_join(test_cuids, XX, "cuid")
write(R$target, file="res/result.txt", sep='\n')
XX = select(XX, -target)
