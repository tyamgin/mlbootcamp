XLL = read.table(file="data/mlboot_data.tsv",
                 sep='\t',
                 head=F,
                 quote='\'', # prevent losing double-quotes
                 colClasses=c("character", "integer", "character", "character", "character", "integer"),
                 col.names=c("cuid", "cat_feature", "j1", "j2", "j3", "dt_diff"))

train_answers = read.table(file="data/mlboot_train_answers.tsv", sep='\t', head=T)

print('Read finished')

saveRDS(XLL, file="data/data.rds", compress=F)


###################

for (col_name in c('j3')) {
  col_tsv = read.table(file=paste0("data/data_", col_name, ".tsv"),
                   sep='\t',
                   head=F,
                   colClasses=c("integer", "integer", "integer"),
                   col.names=c("id", "count", "i"))
  
  saveRDS(col_tsv, file=paste0('data/data_', col_name, '.rds'), compress=F)
}

