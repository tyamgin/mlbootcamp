set.seed(2707)
require(ggplot2)
require(corrgram)
require(lightgbm)
require(foreach)
require(doParallel)
require(caret)
require(randomForest)
require(lars)
require(xgboost)

debugSource("algos.R")
debugSource("lgb.R")
debugSource("xgb.R")
debugSource("nnet.R")
debugSource("genetic.R")
debugSource("preprocess.R")

my.dopar.exports = c('validation.tqfold', 'validation.tqfold.enumerate', 'my.normalizedTrain', 'nnetTrainAlgo', 
                     'my.extendedColsTrain', 'my.roundedTrain', 'error.accuracy', 'my.train.nnet',
                     'my.boot', 'meanAggregator', 'extendXYCols', 'extendCols', 'my.train.lgb',
                     'my.dopar.exports', 'my.dopar.packages')
my.dopar.packages = c('caret', 'lightgbm', 'foreach')

XX = read.csv(file="data/x_train.csv", head=F, sep=";", na.strings="?")
YY = read.csv(file="data/y_train.csv", head=F, sep=";", na.strings="?")

unnameMatrix = function (XX) {
  as.matrix(unname(data.matrix(XX)))
}

XX = unnameMatrix(XX)
XX = my.data.transformFeatures(XX)

#pc = princomp(XX)
#XX = XX %*% solve(t(pc$loadings))
#XX = XX[, 1:30]

"
XX2 = XX
while(ncol(XX2) > 70) {
  corMat = cor(XX2)
  rr = foreach(x=corMat, .combine=c) %do% { sum(x^2) }
  remCol = which.max(rr)
  print(paste0('remove column ', remCol, ' (sum=', max(rr), ')'))
  XX2 = XX2[,-remCol]
}


png(filename='graph/name2.png', width=2000, height=2000)
corrgram(XX2, order=NULL, panel=panel.shade, text.panel=panel.txt, main='Correlogram')
dev.off()

XX = XX2
"

XLL = unnameMatrix(cbind(data.matrix(XX), YY))

extendCols = function (XX, idxes=NULL) {
  if (!is.null(idxes)) {
    if (is.logical(idxes) && idxes) {
      #все
    } else {
      XX = XX[, which(1 == idxes)]
    }
  }
  
  XX
}

extendXYCols = function (XL, idxes) {
  X = XL[, -ncol(XL), drop=F]
  Y = XL[, ncol(XL), drop=F]
  X = extendCols(X, idxes)
  cbind(X, Y)
}

my.extendedColsTrain = function (XL, trainFunc, idxes=NULL) {
  featuresNumber = ncol(XL) - 1
  XL = extendXYCols(XL, idxes)
  model = trainFunc(XL)
  function (X) {
    if (ncol(X) != featuresNumber)
      stop('invalid number of columns')
    X = extendCols(X, idxes)
    model(X)
  }
}

my.normalizedTrain = function (XL, trainFunc) {
  m = ncol(XL) - 1
  means = rep(NA, m)
  sds = rep(NA, m)
  for (j in 1:m) {
    means[j] = mean(XL[, j])
    sds[j] = sd(XL[, j])
    XL[, j] = (XL[, j] - means[j]) / sds[j]
  }
  model = trainFunc(XL)
  function (X) {
    for (j in 1:m)
      X[, j] = (X[, j] - means[j]) / sds[j]
    model(X)
  }
}

my.roundedTrain = function (XL, trainFunc) {
  model = trainFunc(XL)
  function (X) {
    ans = model(X)
    if (is.vector(ans)) {
      mat = matrix(ans, nrow=5, byrow=F)
    } else {
      mat = matrix(c(ans$a, ans$b, ans$c, ans$d, ans$e), nrow=5, byrow=T)
    }
    foreach(x=mat, .combine=c) %do% { which.max(x) - 1 }
  }
}

"
set.seed(2707);algb = my.roundedTrain(XLL, function (XL) {
  my.normalizedTrain(XL, function (XL) {
    my.train.xgb(XL, rowsFactor=0.9, iters=1)
  })
})
imp = xgb.importance(model=tmp.xgb.model)
idxes = rep(0, ncol(XX))
for(s in imp$Feature[1:88]) {
  idxes[as.integer(s)]=1
}
print(paste(idxes, collapse=','))
"

#set.seed(2707);algb = lgbTrainAlgo(XLL)
set.seed(2707);annet = nnetTrainAlgo(XLL)
#set.seed(2707);print(validation.tqfold(XLL, lgbTrainAlgo, folds=7, iters=4, verbose=T))
#set.seed(2707);print(validation.tqfold(XLL, xgbTrainAlgo, folds=7, iters=4, verbose=T))
#set.seed(2707);print(validation.tqfold(XLL, nnetTrainAlgo, folds=7, iters=4, verbose=T))
alg=annet

"
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(2708);print(geneticSelect(iterations=200, XL=XLL, teach=function (XL) {
  my.roundedTrain(XL, function (XL) {
    my.normalizedTrain(XL, function (XL) {
      #my.train.lgb(XL, rowsFactor=0.9, iters=3, lgb.nthread=1)
      my.train.nnet(XL)
    })
  })
}, maxPopulationSize=13, mutationProb=0.05, startOnesProbab=0.3))
stopCluster(cl)
"

# https://www.r-bloggers.com/7-visualizations-you-should-learn-in-r/


#ggplot(train, aes(Item_Visibility, Item_MRP)) + geom_point() + scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+ scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))+ theme_bw() 

#X_X = data.frame(XX)
#plt = ggplot(data=X_X, aes(X_X[,18]))+geom_histogram(binwidth=0.01)
#print(plt)
  #scale_x_continuous("Item MRP", breaks = seq(0,270,by = 30)) +
  #scale_y_continuous("Count", breaks = seq(0,200,by = 20)) +
  #labs(title = "Histogram")


#plot(density((X_X[,77]-mean(X_X[,77])/sd(X_X[,77]))))
#lines(density((X_X[,103]-mean(X_X[,103])/sd(X_X[,103]))), col='red')


XXX = read.csv(file='data/x_test.csv', head=F, sep=';', na.strings='?')
XXX = unnameMatrix(XXX)
XXX = my.data.transformFeatures(XXX, T)
results = alg(XXX)
write(results, file='res/res.txt', sep='\n')
print('done')
