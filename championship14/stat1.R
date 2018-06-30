require(plyr)
require(dplyr)

XLL = readRDS('data/data_t.rds')
#j1 = readRDS('data/data_j1.rds')
#j2 = readRDS('data/data_j2.rds')
#j3 = readRDS('data/data_j3.rds')

train_answers = read.table(file="data/mlboot_train_answers.tsv", sep='\t', head=T)

#XLL$j3s = 0
#j3s = j3 %>% group_by(i) %>% summarise(j3s=sum(count))
#XLL$j3s[j3s$i] = j3s$j3s

#XLL$j2s = 0
#j2s = j2 %>% group_by(i) %>% summarise(j2s=sum(count))
#XLL$j2s[j2s$i] = j2s$j2s

#XLL$j1s = 0
#j1s = j1 %>% group_by(i) %>% summarise(j1s=sum(count))
#XLL$j1s[j1s$i] = j1s$j1s

get_stat = function (df) {
  stat = df %>% group_by(id) %>% summarise(count=sum(count)) %>% arrange(desc(count))
  stat$percent = stat$count / sum(df$count)
  stat
}

#j1_allcat_stat = get_stat(j1)
#j2_allcat_stat = get_stat(j2)
#j3_allcat_stat = get_stat(j3)

#saveRDS(j1_allcat_stat, file='data/j1_allcat_stat', compress=F)
#saveRDS(j2_allcat_stat, file='data/j2_allcat_stat', compress=F)
saveRDS(j3_allcat_stat, file='data/j3_allcat_stat', compress=F)

#j1_5cat_stat = j1[j1$i %in% which(XLL$cat_feature == 5), ] %>% group_by(id) %>% summarise(count=sum(count)) %>% arrange(desc(count))

gen_feature = function (fid, df) {
  ft = df %>% filter(id==fid) %>% group_by(i) %>% summarise(f=sum(count))
  my_feat = rep(0, nrow(XLL))
  my_feat[ft$i] = ft$f
  ret = data.frame(f=my_feat)
  colnames(ret) = paste0(deparse(substitute(df)), '_', fid)
  ret
}

#j1_features = do.call(cbind, lapply(j1_allcat_stat$id[1:50], gen_feature, df=j1))
#j2_features = do.call(cbind, lapply(j2_allcat_stat$id[1:20], gen_feature, df=j2))
#j3_features = do.call(cbind, lapply(j3_allcat_stat$id[1:50], gen_feature, df=j3))

#saveRDS(j1_features, file='data/j1_reatures.rds', compress=F)
#saveRDS(j2_features, file='data/j2_reatures.rds', compress=F)
#saveRDS(j3_features, file='data/j3_reatures.rds', compress=F)