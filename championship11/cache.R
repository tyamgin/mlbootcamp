my.rowHash = function (x) {
  x[which(is.na(x))] = 0
  x[which(is.infinite(x))] = 0
  sum(x)+sum(diff(x))
}

my.matrixHash = function (X) {
  my.rowHash(c(as.matrix(X)))+nrow(X)+ncol(X)
}

my.matrixEquals = function (A, B) {
  if ((!is.matrix(A) && !is.data.frame(A)) || (!is.matrix(B) && !is.data.frame(B))) {
    stop('my.matrixEquals wrong arguments')
  }
  my.matrixHash(A) == my.matrixHash(B)
}

my.enableCache = F