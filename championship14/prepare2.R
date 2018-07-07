require(plyr)
require(dplyr)
require(Matrix)

j1 = readRDS('data/data_j2.rds')
m = max(j1$id) + 1
ii = j1$i 
j1 = NULL
ii = ii > 14646406

XLL = readRDS('data/data_t.rds')
cuid = XLL$cuid
XLL = NULL

j1 = readRDS('data/data_j2.rds') %>% filter(ii)
ii = NULL
gc()
j1 = j1 %>% group_by(cuid[i], id) %>% summarise(count=sum(count))
x = j1$`cuid[i]` - min(j1$`cuid[i]`) + 1
j1 = sparseMatrix(x, j1$id+1, x=j1$count, dims=c(max(x), m))

saveRDS(j1, file='data/data_j2_sp_4.rds', compress=F)

# <= 4882147
# <= 9764281
# <= 14646406