require(lightgbm)

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


X1 = X1[, ]

XK = X1[-sampleIdxes, ]
XL = X1[sampleIdxes, ]  

dtrain <- lgb.Dataset(data=XL[, -ncol(XL)], label=XL[, ncol(XL)], free_raw_data=FALSE)
dtest <- lgb.Dataset(data=XK[, -ncol(XK)], label=XK[, ncol(XK)], free_raw_data=FALSE)

valids <- list(train=dtrain, test=dtest)

bst <- lgb.train(data=dtrain, num_leaves=9, max_depth=3, learning_rate=0.06, nrounds=1000, valids=valids,
                 eval = c("binary_logloss"),
                 nthread = 4, objective = "binary", verbose=0, early_stopping_rounds=50,
                 boosting='gbdt')

print( error.logloss(XK[, ncol(XK)], predict(bst, XK[, -ncol(XK)])) )


