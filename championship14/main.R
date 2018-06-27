require(dplyr)  
require(caret)  
require(e1071)

debugSource("cv.R")

#dddddd = function(){
#substr_count = function(s, substr) {
  #length(strsplit(s, substr)[[1]]) - 1
#}

#ss_count = function (x) {substr_count(x, ":")}

#XLL = readRDS('data/data.rds')


#XLL$j1c = unlist(lapply(XLL$j1, ss_count))
#XLL$j1 <- NULL

#XLL$j2c = unlist(lapply(XLL$j2, ss_count))
#XLL$j2 <- NULL

#XLL$j3c = unlist(lapply(XLL$j3, ss_count))
#XLL$j3 <- NULL

#saveRDS(XLL, file='data/data_t.rds')
#}
print("1111")
#XLL = readRDS('data/data_t.rds')
print("2222")

create_features = function (XG, remove.cuid=T) {
  XG$cat0 = ifelse(XG$cat_feature == 0, 1, 0)
  XG$cat1 = ifelse(XG$cat_feature == 1, 1, 0)
  XG$cat2 = ifelse(XG$cat_feature == 2, 1, 0)
  XG$cat3 = ifelse(XG$cat_feature == 3, 1, 0)
  XG$cat4 = ifelse(XG$cat_feature == 4, 1, 0)
  XG$cat5 = ifelse(XG$cat_feature == 5, 1, 0)
  target_exists = "target" %in% colnames(XG)
  if (!target_exists) {
    XG$target = NA
  }
  XG = XG %>% group_by(cuid) %>% summarise(
    cat0=sum(cat0), cat1=sum(cat1), cat2=sum(cat2), cat3=sum(cat3), cat4=sum(cat4), cat5=sum(cat5),
    dt_diff=mean(dt_diff),
    j1c=sum(j1c), j2c=sum(j2c), j3c=sum(j3c),
    target=max(target)
    )
  if (remove.cuid) {
    XG = select(XG, -cuid)
  }
  if (!target_exists) {
    XG = select(XG, -target)
  }
  XG
}

create_features0 = function (XG, remove.cuid=T) {
  target_exists = "target" %in% colnames(XG)
  if (!target_exists) {
    XG$target = NA
  }
  XG = XG %>% group_by(cuid) %>% summarise(
    cat_feature=mean(cat_feature),
    dt_diff=mean(dt_diff),
    j1c=sum(j1c), j2c=sum(j2c), j3c=sum(j3c),
    target=max(target)
  )
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

algo1 = function (XL) {
  
  model = my.train.lm(XL)
  function (X) {
  
    model(X)
  }
}

train_answers = read.table(file="data/mlboot_train_answers.tsv", sep='\t', head=T)
XY_all = left_join(XLL, train_answers, by="cuid")
XL = XY_all[!is.na(XY_all$target),]


test_cuids = read.table(file="data/mlboot_test.tsv", sep='\t', head=T)
XX = XY_all[is.na(XY_all$target),]
XX = subset(XX, select=-c(target))
XX = create_features(XX, remove.cuid=F)

XL2 = create_features(XL)
print(validation.tqfold(XL2, algo1, folds=5, iters=2, verbose=T, seed=2707)); asdasd()

model = algo1(XL2)

XX$target = model(select(XX, -cuid))
R = left_join(test_cuids, XX, "cuid")
write(R$target, file="res/result.txt", sep='\n')