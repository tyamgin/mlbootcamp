library(FeatureHashing)
library(Matrix)

encode_col = function (stat, sp_mat, size_from, size_to) {
  col_sums = colSums(sp_mat)
  ord = order(col_sums, decreasing=T)
  
  start = 1
  res = list()
  for (i in 1:length(size_from)) {
    sz_from = size_from[i]
    sz_to = size_to[i]
    
    end = start + sz_from - 1
    if (end > ncol(sp_mat) || col_sums[ord[end]] <= 2) { #TODO end is skipped
      break
    }
      
    idxes = ord[start:end]
    start = start + sz_from
    submat = sp_mat[, idxes]
    
    cat(paste0('doing ', i, 'th: ', sz_from, ' => ', sz_to, '\n'))
    
    if (sz_from > sz_to) {
      res[[i]] = as(hashed.model.matrix(~., as.data.frame(as.matrix(submat)), hash.size=sz_to, signed.hash=T), 'dgCMatrix')
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
                         c(1300, 1000, rep(1000, 10), rep(1000, 20)),
                         c(1300, 50,   rep(10,   10), rep(5,    20)))
  saveRDS(hashed, file=paste0('data/', cname, '_hashed'))
  rm(hashed)
  rm(sp)
  gc(verbose=F)
}