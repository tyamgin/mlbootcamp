set.seed(32121)
require(kernlab)
require(scatterplot3d)
require(xgboost)

source("algos.R")

XX = read.csv(file="x_train.csv", head=T, sep=";", na.strings="?")
YY = read.csv(file="y_train.csv", head=F, sep=";", na.strings="?")
XX = as.matrix(unname(data.matrix(XX)))


means = rep(NA, nrow(XX))
sds = rep(NA, nrow(XX))
for (j in 1:ncol(XX)) {
  means[j] = mean(XX[, j])
  sds[j] = sd(XX[, j])
  XX[, j] = (XX[, j] - means[j]) / sds[j]
}




"
pc = princomp(XX)
XX = XX %*% solve(t(pc$loadings))
XX = XX[, 1:12]
"

XLL = as.matrix(unname(cbind(data.matrix(XX), YY)))
sampleIdxes = sample(1:nrow(XLL), nrow(XLL)-3)
XKK = XLL[-sampleIdxes, ]
XLL = XLL[sampleIdxes, ]  

clr = ifelse(XL[,ncol(XL)]==0,"red","green")
#plot(XL[,1], XL[,2], col=clr)
#scatterplot3d(XL[,1:3], color=clr)
#pairs(XL[,1:3], col=clr)

folds = 10
iters = 10
XLerr = 0
XKerr = 0
algos = list(iters*folds)
pos = 1
for (it in 1:iters) {
  for (fold in 1:folds) {
    foldLength = floor(nrow(XLL) / folds)
    controlIdxes = sample(1:nrow(XLL), foldLength)
    XK = XLL[controlIdxes, ]
    XL = XLL[-controlIdxes, ]  
  
    dtrain <- xgb.DMatrix(data=XL[,-ncol(XL)], label=XL[,ncol(XL)])
    dtest <- xgb.DMatrix(data=XK[,-ncol(XK)], label=XK[,ncol(XK)])
    watchlist <- list(train=dtrain, test=dtest)
    
    bstSparse = xgb.train(data=dtrain, watchlist=watchlist, max_depth=2, eta=0.1, nthread=4, nrounds=1000, eval_metric="logloss", objective="binary:logistic", early_stopping_rounds=30, verbose=0)
    algos[[pos]] = bstSparse
    pos = pos + 1
    XLerr = XLerr + error.logloss(XL[,ncol(XL)], predict(bstSparse, XL[,-ncol(XL)]))
    XKerr = XKerr + error.logloss(XK[,ncol(XL)], predict(bstSparse, XK[,-ncol(XL)]))
  }
}
print(paste0("Learn   average logloss: ", XLerr / folds / iters))
print(paste0("Control average logloss: ", XKerr / folds / iters))
aggregated = randomForestTreeFloatAggregator(0, algos)
print(paste0("Super control average logloss: ", calculateError(XKK, aggregated, multi=T)))


XXX = read.csv(file='x_test.csv', head=T, sep=';', na.strings='?')
XXX = data.matrix(XXX)
results = rep(0, nrow(XXX))
for (j in 1:ncol(XXX)) {
  XXX[, j] = (XXX[, j] - means[j]) / sds[j]
}
results = aggregated(XXX)
"for (i in 1:nrow(XXX)) {
  x = XXX[i,]
  #results[i] = calssifier(x) 
  results[i] = predict(bstSparse, matrix(x, nrow=1))
}
"
write(results, file='res.txt', sep='\n')
