# Возвращает число, которое больше всего раз встречается в векторе x
countMax = function(x) {
  as.integer(names(sort(table(x),decreasing=T))[1])
}

calculateError = function(XL, classifier) {
  errors = 0
  n = nrow(XL)
  m = ncol(XL)
  for(i in 1:n) {
    x = XL[i, -m]
    y = XL[i, m]
    if (classifier(x) != y) {
      errors = errors + 1
    }
  }
  return( errors / n )
}

donskoiInform01Criteria = function(XL, pred) {
  res = 0
  n = nrow(XL)
  m = ncol(XL)
  c01 = 0
  c10 = 0
  c00 = 0
  c11 = 0
  for (i in 1:n) {
    v = pred(XL[i, ])
    if (is.na(v))
      next
      
    if (XL[i, m] == 0 && v)
      c01 = c01 + 1
    else if (XL[i, m] == 1 && !v)
      c10 = c10 + 1
    else if (XL[i, m] == 0 && !v)
      c00 = c00 + 1
    else
      c11 = c11 + 1
  }
  return( 2 * (c01 * c10 + c00 * c11) )
}

donskoiInformCriteria = function(XL, pred) {
  res = 0
  for(i in 1:nrow(XL)) {
    for(j in 1:nrow(XL)) {
      if (XL[i,ncol(XL)] != XL[j,ncol(XL)] && pred(XL[i,]) != pred(XL[j,])) {
        res = res + 1
      }
    }
  }
  return( res )
}

ID3.treeClassifier = function(XL, predicates, maxDepth = 1000) {
  m = ncol(XL)
  buildTreeRec = function(XL, predicates, depth) {
    p = length(predicates)
    L = nrow(XL)
    # если закончились предикаты, или достингута максимальная глубина
    # выбрать класс, который больше раз встречается
    if (p == 0 || depth >= maxDepth) {
      return( list(countMax(XL[,m])) )
    }
    
    # если все одного класса, то выбрать этот класс
    if (sum(XL[,m] == XL[1, m]) == L) {
      return( list(XL[1, m]) )
    }
    
    classes = unique(XL[,m])
    
    # ищем предикат с максимальной информативностью
    predicateIdx = -1
    maxInform = -1e10
    for(i in 1:p) {
      pred = predicates[[i]]
      inf = donskoiInform01Criteria(XL, pred)#!!!!!!!!!!
      if (inf >= maxInform) {
        maxInform = inf
        predicateIdx = i
      }
    }
    pred = predicates[[predicateIdx]] # вот он
    qwe = c()
    for(i in 1:L) {
      qwe = c(qwe, pred(XL[i,]))
    }
    failback = countMax(XL[, m])
    
    U0 = XL[which(!qwe),,drop=F]
    U1 = XL[which(qwe),,drop=F]
    if (nrow(U0) == 0 || nrow(U1) == 0) {
      return( list(countMax(XL[,m])) )
    }
    L = buildTreeRec(U0, predicates[-predicateIdx], depth + 1)
    R = buildTreeRec(U1, predicates[-predicateIdx], depth + 1)
    return( list(L, R, pred, failback) )
  }
  tree = buildTreeRec(XL, predicates, 1)
  
  classify = function(tree, x) {
    if (length(tree) == 1)
      return( tree[[1]] )
    
    pred = tree[[3]]
    test = pred(x)
    if (is.na(test)) {
      return( tree[[4]] ) # failback
    }
    if (test)
      return( classify(tree[[2]], x) )
    return( classify(tree[[1]], x) )
  }
  return( function(x) { classify(tree, x) } )
}

ID3.classifier = function(aggregator, XL, predicates, treesCount=100, treesDepth=3, partsFactor = 0.3, predicatesFactor = 1) {
  simpleAlgos = c()
  for(i in 1:treesCount) {
    subXL = XL[sample(1:nrow(XL), nrow(XL)*partsFactor), ]
    subPreds = predicates[sample(1:length(predicates), length(predicates) * predicatesFactor)]
    simpleAlgos = c(simpleAlgos, ID3.treeClassifier(subXL, subPreds, treesDepth))
    
    if (T) {
      print(paste0(treesCount - i, ' simple algos remains'))
    }
  }
  
  return( aggregator(XL, simpleAlgos) )
}

randomForestTreeAggregator = function (XL, baseAlgos) {
  return( function(x) {
    results = c()
    for (algo in baseAlgos)
      results = c(results, algo(x))
    return( countMax(results) )
  } )
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
    
    if (T) {
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