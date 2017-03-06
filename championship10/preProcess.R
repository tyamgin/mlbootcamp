getPreDefinedData = function (XL) {
  XL = XL[do.call(order, as.data.frame(XL)), ]
  n = nrow(XL)
  m = ncol(XL) - 1
  cnt = 1
  removed = rep(F, n)
  XA = matrix(NA, 0, m + 1)
  for (i in 2:(n+1)) {
    if (i > n || sum(XL[i, -(m + 1)] == XL[i - 1, -(m + 1)]) != m) {
      if (cnt > 3) {
        idxes = (i-cnt):(i-1)
        answers = XL[idxes, m + 1]
        o = sum(answers == 1)
        removed[idxes] = T
        
        prob = o / length(answers)
        XA = rbind(XA, c(XL[i - 1, -(m + 1)], prob))
      }
      cnt = 1
    } else {
      cnt = cnt + 1
    }
  }
  
  list(
    XA = XA,
    XL = XL[!removed, ]
  )
}

correctAnswers = function (XL, X, Y) {
  XA = getPreDefinedData(XL)$XA
  for (i in 1:nrow(X)) {
    for (j in 1:nrow(XA)) {
      if (all(XA[j, -ncol(XA)] == X[i, ])) {
        Y[i] = max(0.02, min(0.98, XA[j, ncol(XA)]))
        break
      }
    }
  }
  Y
}

preDefinedTrain = function (XL, train) {
}