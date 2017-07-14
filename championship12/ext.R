jgc <- function() 
  .jcall("java/lang/System", method = "gc")

unnameMatrix = function (X) 
  as.matrix(unname(data.matrix(X)))

gmean = function (x, y) sqrt(x * y)

extendCols = function (X, features=T) {
  w = X$weight
  h = X$height / 100 # в метры
  X$bmi = w / h^2 # https://ru.wikipedia.org/wiki/%D0%98%D0%BD%D0%B4%D0%B5%D0%BA%D1%81_%D0%BC%D0%B0%D1%81%D1%81%D1%8B_%D1%82%D0%B5%D0%BB%D0%B0
  X$bmi4 = w / h^4
  X$al_diff = X$ap_hi - X$ap_lo
  X$map = X$ap_lo * 2 + X$ap_hi
  
  #X$age = X$age * ifelse(X$gender == 1, 19510.12 /  19392.1, 1)
  
  X$lol2 = X$cholesterol - X$gluc
  X$lol3 = X$cholesterol + X$gluc + ifelse(is.na(X$smoke), 0, 3*X$smoke) + X$alco - 4*X$active
  X$fat = (1.39 * w / h^2) + (0.16 * X$age / 365) - (10.34 * X$gender) - 9 # http://halls.md/race-body-fat-percentage/
  #X$score = gmean(apply(X, 1, getScore), apply(X, 1, getFRS))
  
  allFeatures = is.logical(features) && features
  
  if (allFeatures || 'FRSq' %in% features) {
    X$FRSq = apply(X, 1, function (x) getFRS(x, quantity.only=T))
  }
  #X$FRS = apply(X, 1, getFRS)
  
  if (allFeatures || length(features) > 1) {
    cnames = colnames(X)
    sz = ncol(X)
    
    categorial = c('gender', 'cholesterol', 'gluc', 'smoke', 'alco', 'active')
    categorial_values = list(0:1, 1:3, 1:3, 0:1, 0:1, 0:1)
    for (j in 2:length(categorial)) {
      b = X[, categorial[j]]
      jp = categorial_values[[j]]
      for (i in 1:(j-1)) {
        a = X[, categorial[i]]
        ip = categorial_values[[i]]
        
        for (aa in ip) {
          for (bb in jp) {
            cname = sprintf('%s_le%d_and_%s_le%d', categorial[i], aa, categorial[j], bb)
            if (allFeatures || cname %in% features) {
              Z = matrix(a <= aa & b <= bb)
              colnames(Z) = cname
              X = cbind(X, Z)
            }
            
            cname = sprintf('%s_le%d_or_%s_le%d', categorial[i], aa, categorial[j], bb)
            if (allFeatures || cname %in% features) {
              Z = matrix(a <= aa | b <= bb)
              colnames(Z) = cname
              X = cbind(X, Z)
            }
          }
        }
      }
    }
    
    quantity = c('age', 'height', 'weight', 'ap_hi', 'ap_lo')
    funcs = list(log, sqrt, function (x) x^2, function (x) x)
    func.names = c('log_', 'sqrt_', 'pow2_', '')
    for (j in 2:length(quantity)) {
      b = X[, quantity[j]]
      for (i in 1:(j-1)) {
        a = X[, quantity[i]]
        for (afunc in 1:length(funcs)) {
          for (bfunc in 1:length(funcs)) {
            cname = sprintf('%s%s_mul_%s%s', func.names[afunc], quantity[i], func.names[bfunc], quantity[j])
            if (allFeatures || cname %in% features) {
              Z = matrix(funcs[[afunc]](a) * funcs[[bfunc]](b))
              colnames(Z) = cname
              X = cbind(X, Z)
            }
            
            cname = sprintf('%s%s_div_%s%s', func.names[afunc], quantity[i], func.names[bfunc], quantity[j])
            if (allFeatures || cname %in% features) {
              Z = matrix(funcs[[afunc]](a) / funcs[[bfunc]](b))
              colnames(Z) = cname
              X = cbind(X, Z)
            }
          }
        }
      }
    }
  }
  
  if (!allFeatures) {
    X = subset(X, select=intersect(colnames(X), features))
  }
  #X = subset(X, select=-c(id))
  
  #X$a120_80 = X$ap_hi <= 120 | X$ap_lo <= 80
  #X$weight = X$weight * ifelse(X$gender == 1, 1.066385, 1)
  
  #X$sq_diff = abs(X$ap_hi^2 - X$ap_lo^2)
  
  X
}

extendXYCols = function (XL, ...) {
  X = XL[, -ncol(XL), drop=F]
  Y = XL[, ncol(XL), drop=F]
  X = extendCols(X, ...)
  cbind(X, Y)
}

my.extendedColsTrain = function (XL, trainFunc, ..., newdata=NULL) {
  featuresNumber = ncol(XL) - 1

  XL = extendXYCols(XL, ...)
  
  proc = function (X) {
    if (is.null(X))
      return(X)
    if (is.list(X) && !is.matrix(X) && !is.data.frame(X)) {
      return(foreach(x=X) %do% {
        proc(x)
      })
    }
    
    if (ncol(X) != featuresNumber)
      stop('invalid number of columns')
    
    X = extendCols(X, ...)
    X
  }
  model = trainFunc(XL, newdata=proc(newdata))
  
  function (X) {
    X = proc(X)
    model(X)
  }
}

postProcess = function (X) {
  if ('smoke' %in% colnames(X))
    X$smoke[which(is.na(X$smoke))] = 0# predict(knn.model.smoke, sel.col(X[which(is.na(X$smoke)),]))
  if ('alco' %in% colnames(X))
    X$alco[which(is.na(X$alco))] = 0#predict(knn.model.smoke, sel.col(X[which(is.na(X$alco)),]))
  if ('active' %in% colnames(X))
    X$active[which(is.na(X$active))] = 1#predict(knn.model.smoke, sel.col(X[which(is.na(X$active)),]))
  X
}

my.removedNasColumnsTrain = function (XL, trainFunc, newdata=NULL) {
  good = c()
  for (j in 1:(ncol(XL)-1)) {
    if (!all(is.na(XL[, j])))
      good = c(good, colnames(XL)[j])
  }
  XL = XL[, c(good, colnames(XL)[ncol(XL)])]
  if (!is.null(newdata))
    newdata = newdata[, good]
  model = trainFunc(XL, newdata=newdata)
  function (X) model(X[, good])  
}

my.filledHolesTrain = function (XL, trainFunc, newdata=NULL) {
  if (!is.null(newdata))
    newdata = postProcess(newdata)
  model = trainFunc(XL, newdata=newdata)
  function (X) model(postProcess(X))
}

my.normalizedTrain = function (XL, trainFunc, newdata=NULL) {
  m = ncol(XL) - 1
  means = rep(NA, m)
  sds = rep(NA, m)
  for (j in 1:m) {
    idxes = which(!is.na(XL[, j]))
    means[j] = mean(XL[idxes, j])
    sds[j] = sd(XL[idxes, j])
    XL[idxes, j] = (XL[idxes, j] - means[j]) / sds[j]
  }
  
  proc = function (X) {
    if (is.null(X))
      return( X )
    if (is.list(X) && !is.matrix(X) && !is.data.frame(X))
      return( foreach(x=X) %do% proc(x) )
    
    for (j in 1:m) {
      idxes = which(!is.na(X[, j]))
      X[idxes, j] = (X[idxes, j] - means[j]) / sds[j]
    }
    X
  }
  
  model = trainFunc(XL, newdata=proc(newdata))
  function (X) {
    X = proc(X)
    model(X)#norm
  }
}

my.fixedDataTrain = function (XL, trainFunc, newdata=NULL) {
  if (!is.null(newdata))
    newdata = my.fixData(newdata)
  XL = my.fixData(XL, T)
  model = trainFunc(XL, newdata)
  function (X) model(my.fixData(X))
}

my.trimmedTrain = function (XL, trainFunc, newdata=NULL) {
  model = trainFunc(XL, newdata)
  function (X) {
    r = model(X)

    g = 0.85
    s = 0.15
    
    #rogue = X$ap_lo < 40 | abs(X$ap_hi - X$ap_lo) > 80 | X$weight < 30 | X$height < 70 | X$weight > X$height | X$weight > 200 | X$height > 220
    
    #rogue = X$ap_lo <= 40 | X$ap_hi <= X$ap_lo | abs(X$ap_hi - X$ap_lo) > 80 | X$ap_hi > 220 | X$weight < 38 | X$height < 130 | X$weight > X$height | X$weight > 200 | X$height > 220
    
    rogue = X$ap_lo <= 40 | X$ap_hi <= X$ap_lo | abs(X$ap_hi - X$ap_lo) > 80 | X$ap_hi > 220
    
    #r = ifelse(r > g & rogue, (r - g) / 2 + g, r)
    #r = ifelse(r < s & rogue, s - (s - r) / 2, r)
    
    r
  }
}