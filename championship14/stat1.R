require(plyr)
require(dplyr)

XLL = readRDS('data/data_t.rds')
j1 = readRDS('data/data_j1.rds')
j3 = readRDS('data/data_j3.rds')

train_answers = read.table(file="data/mlboot_train_answers.tsv", sep='\t', head=T)

XLL$idx = 1:nrow(XLL)

get_stat = function (df) {
  stat = df %>% group_by(id) %>% summarise(count=sum(count)) %>% arrange(desc(count))
  stat$percent = stat$count / sum(df$count)
  stat
}

j1_allcat_stat = get_stat(j1)
j3_allcat_stat = get_stat(j3)

j1_5cat_stat = j1[j1$i %in% which(XLL$cat_feature == 5), ] %>% group_by(id) %>% summarise(count=sum(count)) %>% arrange(desc(count))

gen_feature = function (fid, df) {
  ft = df %>% filter(id==fid) %>% group_by(i) %>% summarise(f=sum(count))
  my_feat = rep(0, nrow(XLL))
  my_feat[ft$i] = ft$f
  ret = data.frame(f=my_feat)
  colnames(ret) = paste0(deparse(substitute(df)), '_', fid)
  ret
}

j1_features = do.call(cbind, lapply(c(809001,  834560, 1376123,  338935,  377795, 1387851,  219266, 1139394, 1060624), gen_feature, df=j1))
j3_features = do.call(cbind, lapply(c(632420, 401824, 961931, 447167, 595712, 272922, 337285, 739245), gen_feature, df=j3))

saveRDS(j1_features, file='data/j1_reatures.rds', compress=F)
saveRDS(j3_features, file='data/j3_reatures.rds', compress=F)