sink()
sink("data/log.txt", append=T, split=T)

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
debugSource("models.R")

set.seed(888)

#j1_features = readRDS('data/j1_features.rds')[, 1:200]
#j2_features = readRDS('data/j2_features.rds')[, 1:200]
#j3_features = readRDS('data/j3_features.rds')[, 1:200]

readSp = function (cname, i) {
  ids = readRDS(paste0('data/', cname, '_allcat_stat'))$id[i]
  r = readRDS(paste0('data/data_', cname, '_sp.rds'))[, ids + 1]
  colnames(r) = paste0(cname, '_', ids)
  imp = readRDS('data/impb3.rds')
  #r = r[, colnames(r) %in% imp$Feature[imp$Gain > 5e-5]]
  
  #r1 = r[, 1:100]
  #colnames(r1) = paste0(colnames(r1), '_o')
  #cbind(mbool(r), r1)
  mbool(r)
}

mbool = function (a) {
  sparseMatrix(a@i+1, p=a@p, x=min(1, a@x), dims=a@Dim, dimnames=a@Dimnames)
}

j1_sp = readSp('j1', 1:5300)
j2_sp = readSp('j2', 1:5300)
j3_sp = readSp('j3', 1:4300)

#j1_sp_rest = readRDS('data/j1_svd_rest_50.rds')$u
#j2_sp_rest = readRDS('data/j2_svd_rest_50.rds')$u
#j3_sp_rest = readRDS('data/j3_svd_rest_50.rds')$u

#j1_hashed = readRDS('data/j1_hashed')
#j2_hashed = readRDS('data/j2_hashed')
#j3_hashed = readRDS('data/j3_hashed')

#j1_u = readRDS('data/j1_u.rds')
#j2_u = readRDS('data/j2_u.rds')
#j3_u = readRDS('data/j3_u.rds')


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
      dt_diff_max_diff = max(diff(dt_diff), 0),
      #dt_diff_distinct = n_distinct(dt_diff),
      j1s=sum(j1s), j2s=sum(j2s), j3s=sum(j3s)
      #j1s=sum(j1s), j2s=sum(j2s), j3s=sum(j3s)
    ) %>% mutate(
      #cats=(cat0>0)+(cat1>0)+(cat2>0)+(cat3>0)+(cat4>0)+(cat5>0),
      #j1s_per_day = j1s / dt_diff_distinct,
      #j2s_per_day = j2s / dt_diff_distinct,
      #j3s_per_day = j3s / dt_diff_distinct,
      dt_diff_spread = dt_diff_max - dt_diff_min
    )
  tar = grp %>% summarise(target=max(target)) %>% select(-cuid) %>% as.matrix()
  grp = NULL
  
  cuid = XG1$cuid
  XG1 = as.matrix(XG1)
  
  print('before cbind')
  
  XG = cbind(
    XG1, 
    j1_sp[cuid, ],
    j2_sp[cuid, ],
    j3_sp[cuid, ],
    #j1_u[cuid, ],
    #j2_u[cuid, ],
    #j3_u[cuid, ],
    #j1_features[cuid, ],
    #j2_features[cuid, ],
    #j3_features[cuid, ],
    #j1_hashed[cuid, ],
    #j2_hashed[cuid, ],
    #j3_hashed[cuid, ],
    
    #j1_sp_rest[cuid, ],
    #j2_sp_rest[cuid, ],
    #j3_sp_rest[cuid, ],
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



algo1 = function (XL) {
  #my.train.lm(XL)
  #my.train.nnet(XL)
  #my.train.knn(XL)
  my.train.lgb(XL, lgbParams)
  #my.train.et(XL, etParams)
}

XL2 = XX = NULL
gc()

print('before create features')
XL2 = XY_all[!is.na(XY_all$target),] %>% create_features()
print('after create features')

if(0) {
XX = XY_all[is.na(XY_all$target),] %>%
  select(-target) %>%
  create_features(remove.cuid=F)
}

j1_features = j2_features = j3_features = NULL
j1_sp = j2_sp = j3_sp = NULL
j1_u = j2_u = j3_u = NULL
j1_sp_rest = j2_sp_rest = j3_sp_rest = NULL
j1_hashed = j2_hashed = j3_hashed = NULL

print('preparing complete')


#XY_all = NULL

#rr = cv.glmnet(XL2[, -ncol(XL2), drop=F], XL2[, ncol(XL2), drop=T], 
#               nfolds=5, type.measure="auc",
#               family='binomial',type.logistic='Newton', type.multinomial='ungrouped', 
#               alpha=0)
if(1){
print(system.time({
  my.gridSearch(XL2, function (params) {
    function (XL, newdata) {
      my.train.lgb(XL, params)
    }
  }, expand.grid(lgbParams), verbose=T, iters=1, folds=5, train.seed=2708, folds.seed=888, use.newdata=F)
})
)
lol()}

#set.seed(888)
#validation.tqfold(XL2, algo1, folds=5, iters=3, verbose=T, seed=2707); asdasd()




model = algo1(XL2)

#XX = as.data.frame(XX)
R = data.frame(target = model(XX[, colnames(XX) != 'cuid']), cuid = XX[, colnames(XX) == 'cuid', drop=T])
R = left_join(test_cuids, R, "cuid")
write(R$target, file="res/result.txt", sep='\n')
