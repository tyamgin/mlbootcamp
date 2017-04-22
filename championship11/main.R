set.seed(2707)
require(ggplot2)
require(corrgram)
require(lightgbm)
require(foreach)

debugSource("algos.R")
debugSource("lgb.R")

XX = read.csv(file="data/x_train.csv", head=F, sep=";", na.strings="?")
YY = read.csv(file="data/y_train.csv", head=F, sep=";", na.strings="?")

unnameMatrix = function (XX) {
  as.matrix(unname(data.matrix(XX)))
}

XX = unnameMatrix(XX)
XLL = unnameMatrix(cbind(data.matrix(XX), YY))

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

set.seed(2707);algb = lgbTrainAlgo(XLL)
#set.seed(2707);print(validation.tqfold(XLL, lgbTrainAlgo, folds=7, iters=10, verbose=T))
alg=algb

# https://www.r-bloggers.com/7-visualizations-you-should-learn-in-r/


#ggplot(train, aes(Item_Visibility, Item_MRP)) + geom_point() + scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+ scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))+ theme_bw() 

#plt = ggplot(data=XX, aes(XX[,11]))+geom_histogram(binwidth=0.01)
#print(plt)
  #scale_x_continuous("Item MRP", breaks = seq(0,270,by = 30)) +
  #scale_y_continuous("Count", breaks = seq(0,200,by = 20)) +
  #labs(title = "Histogram")

#corrgram(XX, order=NULL, panel=panel.shade, text.panel=panel.txt, main="Correlogram") 

XXX = read.csv(file='data/x_test.csv', head=F, sep=';', na.strings='?')
XXX = unnameMatrix(XXX)
results = alg(XXX)
write(results, file='res/res.txt', sep='\n')
print('done')