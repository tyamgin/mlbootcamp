set.seed(123)

X = XLL[, -ncol(XLL)]
colnames(X) <- paste0('X', 1:ncol(X))
Y = factor(XLL[, ncol(XLL)], labels=c('a', 'b', 'c', 'd', 'e'))

model <- mx.mlp(X, Y, hidden_node=10, out_activation="softmax",
                num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9,
                eval.metric=mx.metric.accuracy)