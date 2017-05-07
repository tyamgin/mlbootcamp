
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
  
  for (i in 1:ncol(XX)) {
    if (my.data.featurestGcd[i] != -1) {
      print(paste0('transforming ', i, ' feature'))
        XX[, i] = my.data.transformDiscreteFeature(XX[, i], my.data.featurestGcd[i])
    }
  }

  XX
}