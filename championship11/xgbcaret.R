X = XLL[, -ncol(XLL)]
colnames(X) <- paste0('X', 1:ncol(X))
Y = factor(XLL[, ncol(XLL)], labels=c('a', 'b', 'c', 'd', 'e'))

trControl = trainControl(method='cv', number=4, repeats=3,  classProbs=T, summaryFunction=defaultSummary)

tuneGrid = expand.grid(
  nrounds = c(250, 300, 350, 400),
  max_depth = 3:4,
  eta = c(0.04,0.05,0.06),
  gamma = c(3, 3.5, 4),
  colsample_bytree = c(0.9, 1),
  min_child_weight = 1,
  subsample = c(0.8, 0.9, 1)
)


model <- train(X, Y, method='xgbTree', metric='Accuracy',
                 maximize=T, trControl=trControl,
                 tuneGrid=tuneGrid)
