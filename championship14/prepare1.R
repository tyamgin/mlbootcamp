for (col_name in c('j2')) {
  col_tsv = read.table(file=paste0("data/data_", col_name, ".tsv"),
                   sep='\t',
                   head=F,
                   colClasses=c("integer", "integer", "integer"),
                   col.names=c("id", "count", "i"))
  
  saveRDS(col_tsv, file=paste0('data/data_', col_name, '.rds'), compress=F)
}