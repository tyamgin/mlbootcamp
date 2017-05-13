
my.data.featurestGcd = rep(-1, 223)
my.data.featurestGcd[12] = 0.00333333333
my.data.featurestGcd[77] = 0.028170
my.data.featurestGcd[80] = 0.00166666666
my.data.featurestGcd[97] = 0.000767
my.data.featurestGcd[98] = 0.062194
my.data.featurestGcd[116] = 0.004132
my.data.featurestGcd[132] = 0.067202
my.data.featurestGcd[139] = 0.013974
my.data.featurestGcd[157] = 0.001661
my.data.featurestGcd[183] = 0.006024
my.data.featurestGcd[201] = 0.008846

my.data.transformDiscreteFeature = function (arr, gcd) {
  arr = arr - min(arr)
  res = round(arr / gcd)
  if (sum(res != round(arr / gcd, 1)) > 0) {
    stop('bad precision')
  }
  res
}

my.data.transformFeatures = function (XX) {
  
  "for (i in 1:ncol(XX)) {
    if (my.data.featurestGcd[i] != -1) {
      print(paste0('transforming ', i, ' feature'))
        XX[, i] = my.data.transformDiscreteFeature(XX[, i], my.data.featurestGcd[i])
    }
  }"

  XX
}







getPreProcessedData = function (XL) {
  XL = XL[, c(intCols, ncol(XL))]
  XL = XL[do.call(order, as.data.frame(XL)), ]
  n = nrow(XL)
  m = ncol(XL) - 1
  cnt = 1
  removed = rep(F, n)
  XA = matrix(NA, 0, m + 1)
  for (i in 2:(n+1)) {
    if (i > n || sum(XL[i, -(m + 1)] == XL[i - 1, -(m + 1)]) != m) {
      if (cnt > 1) {
        idxes = (i-cnt):(i-1)
        answers = XL[idxes, m + 1]
        #o = sum(answers == 1)
        #removed[idxes] = T
        
        #prob = o / length(answers)
        #prob = max(1/length(answers), min(prob, 1-1/length(answers)))
        #XA = rbind(XA, c(XL[i - 1, -(m + 1)], prob))
        print(answers)
      }
      cnt = 1
    } else {
      cnt = cnt + 1
    }
  }
  
  #XL2 = XA
  #for (i in 1:nrow(XL2))
  #  XL2[i, m + 1] = round(XL2[i, m + 1])
  
  #list(
  #  XA = XA,
  #  XL = rbind(XL[!removed, ], XL2)
  #)
}