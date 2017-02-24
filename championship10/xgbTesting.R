sampleIdxes = 1:15000

X1 = XLL

sz = ncol(X1) - 1
for (j in 2:sz) {
  for (k in 1:(j-1)) {
    num = X1[, j]
    denum = X1[, k]
    
    X1 = insertXLCol(X1, num / (denum + ifelse(min(denum) == 0, 1, 0)))
    X1 = insertXLCol(X1, num * denum)
  }
}

for(i in 1:(ncol(X1)-1)) {
  X1[, i] = (X1[, i] - mean(X1[, i])) / sd(X1[, i])
}

"
XX1 = X1[, -ncol(X1)]
#pc = princomp(XX1)
#pc$loadings
pc = prcomp(XX1)
XX1 = XX1 %*% solve(t(pc$rotation))
XX1 = XX1[,]

X1 = cbind(XX1, X1[, ncol(X1)])
"

colnames(X1) <- paste('col', 1:ncol(X1), sep='')


X1 = X1[, ]

XK = X1[-sampleIdxes, ]
XL = X1[sampleIdxes, ]  

dtrain <- xgb.DMatrix(data=XL[, -ncol(XL)], label=XL[, ncol(XL)])
dtest <- xgb.DMatrix(data=XK[, -ncol(XK)], label=XK[, ncol(XK)])
watchlist <- list(train=dtrain, test=dtest)

bstSparse <- xgb.train(data=dtrain, watchlist=watchlist, max_depth=3, eta=0.06, nthread=8, nrounds=3000, eval_metric='logloss', objective='binary:logistic', early_stopping_rounds=50, verbose=0)
print( error.logloss(XK[, ncol(XK)], predict(bstSparse, XK[, -ncol(XK)])) )

imp = xgb.importance(model=bstSparse, feature_names=colnames(XL))
