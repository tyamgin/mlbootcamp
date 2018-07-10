library(FeatureHashing)
library(Matrix)

encode_col = function (stat, sp_mat, size_from, size_to) {
  ord = order(colSums(sp_mat), decreasing=T)
  start = 1
  res = list()
  for (i in 1:length(size_from)) {
    sz_from = size_from[i]
    sz_to = size_to[i]
    
    idxes = ord[start:(start + sz_from - 1)]
    start = start + sz_from
    submat = sp_mat[, idxes]
    
    cat(paste0('doing ', i, 'th: ', sz_from, ' => ', sz_to, '\n'))
    
    if (sz_from > sz_to) {
      res[[i]] = as(hashed.model.matrix(~., as.data.frame(as.matrix(submat)), hash.size=sz_to), 'dgCMatrix')
    } else {
      res[[i]] = submat
    }
    rm(submat)
    gc(verbose=F)
  }
  unname(do.call('cbind', res))
}

for (cname in c('j1', 'j2', 'j3')) {
  stat = readRDS(paste0('data/', cname, '_allcat_stat'))
  sp = readRDS(paste0('data/data_', cname, '_sp.rds'))
  hashed = encode_col(stat, sp,
                         c(200, 1000, 1000, 1000, rep(1000, 10)),
                         c(200, 500,  300,  100, rep(30, 10)))
  saveRDS(hashed, file=paste0('data/', cname, '_hashed'))
  rm(hashed)
  rm(sp)
  gc(verbose=F)
}