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

XL = as.matrix(unname(cbind(data.matrix(XX), YY)))
clr = ifelse(XL[,ncol(XL)]==0,"red","green")
#plot(XL[,1], XL[,2], col=clr)
#scatterplot3d(XL[,1:3], color=clr)
#pairs(XL[,1:3], col=clr)

#sampleIdxes = sample(1:nrow(XL), nrow(XL)-10)
sampleIdxes = sample(1:nrow(XL), 15000)
XK = XL[-sampleIdxes,]
XL = XL[sampleIdxes,]


#calssifier = ID3.treeClassifier(XL, predicates, 6)
#calssifier = ID3.classifier(aggregator=randomForestTreeFloatAggregator, XL=XL, treesDepth=5, partsFactor=5000/5000, treesCount=100, colsFactor=6/10)
#calssifier = ID3.classifier(aggregator=adaBoostAggregator, XL=XL, treesDepth=2, partsFactor=200/5000, treesCount=1000, colsFactor=3/12)

#algos = svm.getBaseAlgos(XL, count=1, partsFactor=1)
#calssifier = randomForestTreeFloatAggregator(0, algos)

dtrain <- xgb.DMatrix(data=XL[,-ncol(XL)], label=XL[,ncol(XL)])
dtest <- xgb.DMatrix(data=XK[,-ncol(XK)], label=XK[,ncol(XK)])
watchlist <- list(train=dtrain, test=dtest)

bstSparse = xgb.train(data=dtrain, watchlist=watchlist, max_depth=2, eta=0.1, nthread=4, nrounds=1000, eval_metric="logloss", objective="binary:logistic", early_stopping_rounds=30)
#bstSparse = xgb.train(data=dtrain, watchlist=watchlist, max_depth=3, booster="gblinear", nthread=4, nrounds=200, eval_metric="logloss", objective="binary:logistic")
print( error.logloss(XL[,ncol(XL)], predict(bstSparse, XL[,-ncol(XL)])) )
print( error.logloss(XK[,ncol(XL)], predict(bstSparse, XK[,-ncol(XL)])) )

#print(paste0('learn error = ', calculateError(XL, calssifier, multi=T)))
#print(paste0('control error = ', calculateError(XK, calssifier, multi=T)))



XXX = read.csv(file='x_test.csv', head=T, sep=';', na.strings='?')
XXX = data.matrix(XXX)
results = rep(0, nrow(XXX))
for (j in 1:ncol(XXX)) {
  XXX[, j] = (XXX[, j] - means[j]) / sds[j]
}
for (i in 1:nrow(XXX)) {
  x = XXX[i,]
  #results[i] = calssifier(x)
  results[i] = predict(bstSparse, matrix(x, nrow=1))
}
write(results, file='res.txt', sep='\n')

