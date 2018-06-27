XLL = read.table(file="data/mlboot_data.tsv",
                 sep='\t',
                 head=F,
                 colClasses=c("character", "integer", "character", "character", "character", "integer"),
                 col.names=c("cuid", "cat_feature", "j1", "j2", "j3", "dt_diff"))

train_answers = read.table(file="data/mlboot_train_answers.tsv", sep='\t', head=T)

print('Read finished')

saveRDS(XLL, file="data/data.rds", compress=F)

print('Write finished')

#X_train = XLL[XLL$cuid %in% train_answers$cuid,]
#X_test = XLL[!(XLL$cuid %in% train_answers$cuid),]

#write.table(X_train, 'data/train.tsv', sep='\t', col.names=colnames(X_train), quote=F, row.names=F)
#write.table(X_test, 'data/test.tsv', sep='\t', col.names=colnames(X_train), quote=F, row.names=F)