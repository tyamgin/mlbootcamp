require(caret)

debugSource('clear.R')

XLL = read.csv(file="data/train.csv", head=T, sep=";", na.strings="None")
XXX = read.csv(file="data/test.csv", head=T, sep=";", na.strings="None")

XLL = my.fixData(XLL)
XXX = my.fixData(XXX)

XA = rbind(XLL[, -ncol(XLL)], XXX)
gd = which(!is.na(XA$alco) & !is.na(XA$smoke) & !is.na(XA$active))
XA1 = XA[gd, ]
XA2 = XA[-gd, ]

tuneGrid = expand.grid(
  kmax = 20,
  distance = 2,
  kernel = 'inv'
)
sel.col = function(X) subset(X, select=-c(smoke, alco, active, id))

knn.model.active = train(sel.col(XA1), as.factor(XA1$active), method='kknn', metric='Accuracy',
               maximize=F, trControl=trainControl(method='none'),
               tuneGrid=tuneGrid)

knn.model.alco = train(sel.col(XA1), as.factor(XA1$alco), method='kknn', metric='Accuracy',
               maximize=F, trControl=trainControl(method='none'),
               tuneGrid=tuneGrid)

knn.model.smoke = train(sel.col(XA1), as.factor(XA1$smoke), method='kknn', metric='Accuracy',
               maximize=F, trControl=trainControl(method='none'),
               tuneGrid=tuneGrid)
