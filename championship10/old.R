# Возвращает число, которое больше всего раз встречается в векторе x
countMax = function(x) {
  as.integer(names(sort(table(x),decreasing=T))[1])
}

ID3.treeClassifier = function(XL, cols, maxDepth = 1000) {
  m = ncol(XL)
  buildTreeRec = function(XL, depth) {
    n = nrow(XL)
    # если закончились предикаты, или достингута максимальная глубина
    # посчитать среднее
    if (depth >= maxDepth) {
      return( list(mean(XL[,m])) )
    }
    
    # если все одного класса, то выбрать этот класс
    if (sum(XL[,m] == XL[1, m]) == n) {
      return( list(XL[1, m]) )
    }
    
    # ищем предикат с максимальной информативностью
    maxInform = -1e10
    selCol = -1
    selThr = -1
    
    totalOnes = sum(XL[, m])
    totalZeroes = n - totalOnes
    for (col in cols) {
      ordered = order(XL[, col])
      cnt0 = 0
      cnt1 = 0
      for (i in 1:(n - 1)) {
        if (XL[ordered[i], m] == 0)
          cnt0 = cnt0 + 1
        else
          cnt1 = cnt1 + 1
        
        if (ordered[i] == ordered[i + 1])
          next
        
        inform = cnt0 * (totalOnes - cnt1) + cnt1 * (totalZeroes - cnt0)
        
        if (inform > maxInform) {
          maxInform = inform
          selCol = col
          selThr = (XL[ordered[i], col] + XL[ordered[i + 1], col]) / 2
        }
      }
    }
    if (selCol == -1) {
      stop("cannot find any threshold")
    }
    
    leftIdxes = which(XL[, selCol] < selThr)
    
    U0 = XL[-leftIdxes, ,drop=F]
    U1 = XL[leftIdxes, ,drop=F]
    if (nrow(U0) == 0 || nrow(U1) == 0) {
      return( list(mean(XL[,m])) )
    }
    L = buildTreeRec(U0, depth + 1)
    R = buildTreeRec(U1, depth + 1)
    return( list(L, R, selCol, selThr) )
  }
  tree = buildTreeRec(XL, 1)
  
  classify = function(tree, x) {
    if (length(tree) == 1)
      return( tree[[1]] )
    
    col = tree[[3]]
    thr = tree[[4]]
    if (x[col] < thr)
      return( classify(tree[[2]], x) )
    return( classify(tree[[1]], x) )
  }
  function(x) { classify(tree, x) }
}

ID3.classifier = function(aggregator, XL, treesCount=100, treesDepth=3, partsFactor=0.3, colsFactor=1) {
  simpleAlgos = c()
  n = nrow(XL)
  m = ncol(XL) - 1
  for(i in 1:treesCount) {
    subXL = XL[sample(1:n, n*partsFactor, T), ]
    cols = sample(1:m, m*colsFactor)
    simpleAlgos = c(simpleAlgos, ID3.treeClassifier(subXL, cols, treesDepth))
    
    if (DEBUG_OUTPUT) {
      print(paste0(treesCount - i, ' simple algos remains'))
    }
    gc()
  }
  
  aggregator(XL, simpleAlgos)
}


adaBoostAggregator = function(XL, baseAlgos) {
  n = nrow(XL)
  m = ncol(XL)
  weight = rep(1/n, times=n)
  alpha = c()
  algo = c()
  
  algoResults = matrix(NA, length(baseAlgos), n)
  for (i in 1:length(baseAlgos))
    for (j in 1:n)
      algoResults[i, j] = baseAlgos[[i]](XL[j, -m])
  
  algoUsages = rep(0, length(baseAlgos))
  
  while(sum(algoUsages) < length(baseAlgos)) {
    # Находим классификатор который минимизирует взвешенную ошибку классификации
    minError = 1e10
    selIdx = NA
    for(j in 1:length(baseAlgos)) {
      if (algoUsages[j] > 0)
        next
      
      error = 0
      for(i in 1:n) {
        if (algoResults[j, i] != XL[i, m]) {
          error = error + weight[i]
        }
      }
      if (error < minError) {
        minError = error
        selIdx = j
      }
    }
    if (abs(minError - 1) < 1e-5) {
      break
    }
    
    a = log((1-minError)/minError)/2
    
    for(i in 1:n)
      weight[i] = weight[i] * exp(-a * ifelse(XL[i, m] == algoResults[selIdx, i], 1, -1))
    weight = weight / sum(weight)
    
    algo = c(algo, baseAlgos[[selIdx]])
    alpha = c(alpha, a)
    
    algoUsages[selIdx] = algoUsages[selIdx] + 1
    
    if (DEBUG_OUTPUT) {
      print(paste0(length(baseAlgos) - sum(algoUsages), ' adaBoost algos remains'))
    }
  }
  
  classes = unique(XL[, m])
  
  return( function(x) {
    cnt = rep(0, times=length(classes))
    for(i in 1:length(algo)) {
      b = algo[[i]]
      cl = b(x)
      for(j in 1:length(classes)) {
        if (classes[j] == cl) {
          cnt[j] = cnt[j] + alpha[i]
        }
      }
    }
    return( classes[which.max(cnt)] )
  } )
}