# Example:
# crossover(c(1,2,3,4,5), c(10,20,30,40,50))
#             1 2 30 4 50
crossover = function(a, b) {
  n = length(a)
  for(i in 1:n)
    a[i] = sample(c(a[i], b[i]), 1)
  a
}

# Example: 
# mutation(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), 0.1)
#            1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1
mutation = function(a, p = 0.1) {
  n = length(a)
  for(i in 1:n)
    a[i] = sample(c(1 - a[i], a[i]), 1, prob=c(p, 1 - p))
  a
}

tqfoldEstimation = function(XL, G, teach) {
  p = sum(G)
  if (p <= 1)
    return( list(int=1e10+p, ext=1e10+p) )
  
  L = nrow(XL)
  m = ncol(XL) - 1
  subXL = XL[, c(which(G == 1), m + 1)]
  
  nextSeed = sample(1:10^5, 1)
  set.seed(666)
  e = mean(validation.tqfold(subXL, teach, folds=5, iters=6, verbose=F))
  set.seed(nextSeed)
  list(int=e, ext=e)
}

geneticSelect = function(iterations,  # количество итераций
                         XL, # выборка
                         teach, # teach(XL) - обучение
                         maxPopulationSize, # размер популяции
                         mutationProb = 0.1, # вероятность мутации
                         estimate = tqfoldEstimation, # оценка
                         d = 100, # дополнительный параметр останова
                         startOnesProbab = 0.5,
                         removeDuplicates=T,
                         config=NULL
) {
  if (is.character(config)) {
    source(config, local=TRUE)
  }
  
  L = nrow(XL)
  size = ncol(XL) - 1
  R = matrix(NA, maxPopulationSize^2, size + 2) # генерация хромосом
  
  chr = function(vec) { # получить хромосому
    vec[1:size]
  }
  computed = function(vec) {
    if (is.na(vec[size + 1])) {
      est = estimate(XL, chr(vec), teach)
      vec[size + 1] = est$ext
      vec[size + 2] = est$int
    }
    vec
  }
  
  
  R = unname(foreach(i=1:nrow(R), .combine=rbind, .export=my.dopar.exports, .packages=my.dopar.packages) %dopar% {
    computed(c(sample(0:1, size, T, prob=c(1-startOnesProbab, startOnesProbab)), NA, NA))
  })
  
  best = R[1, ]
  bestIteration = 0
  
  plot(x=c(0, iterations), y=c(0,max(R[, size + 1])), col=c(0,0),
       xlab="Номер итерации", ylab="переобучение")
  
  for (iter in 1:iterations) {
    print(c("iteration", iter))
    
    # sort
    R = unname(foreach(i=1:nrow(R), .combine=rbind, .export=my.dopar.exports, .packages=my.dopar.packages) %dopar% {
      computed(R[i, ])
    })
    R = R[order(R[, size + 1]), ]
    
    if (removeDuplicates) {
      # удаление повторяющихся экземпляров (если ответы разные, то оставить лучшего)
      R = R[!duplicated(R[, -ncol(R)]), ]
    }
    
    if (nrow(R) > maxPopulationSize) { # отбор лучших
      R = R[1:maxPopulationSize, ]
    }
    
    if (R[1, size + 1] < best[size + 1]) {
      best = R[1, ]
      bestIteration = iter
    }
    
    popSize = nrow(R)
    points(x=rep(iter, popSize), y=R[,size + 1], pch=rep(16, popSize), col=rep("blue", popSize))
    print(
      c(
        paste(R[1, ], collapse=','),
        paste0('(', sum(chr(R[1, ])), ')')
      )
    )
    
    if (iter - bestIteration >= d)
      break
    
    if (is.character(config)) {
      source(config, local=TRUE)
    }
    dput(R, file=paste0(iter, '.tmp.txt'))
    
    for (i in 1:popSize) {
      for (j in 1:i) {
        child = mutation(crossover(chr(R[i, ]), chr(R[j, ])), mutationProb)
        R = rbind(R, c(child, NA, NA))
      }
    }
  }
  
  best
}

addRemoveSelect = function(iterations,  # количество итераций
                           XL, # выборка
                           teach, # teach(XL) - обучение
                           estimate = tqfoldEstimation, # оценка
                           startVec = c(T)
) {
  L = nrow(XL)
  size = ncol(XL) - 1
  
  vec = startVec
  if (length(vec) < size) {
    vec = c(vec, rep(F, size - length(vec)))
  }
  
  iterPts = c()
  intPts = c()
  extPts = c()
  
  est = estimate(XL, vec, teach)
  
  for (it in 1:iterations) {
    addOrRemove = sample(0:1, 1)
    i = 1
    for (k in sample(size)) {
      if (vec[k] == addOrRemove) {
        i = k
        break
      }
    }
    
    newVec = vec
    newVec[i] = !newVec[i]
    
    newEst = estimate(XL, newVec, teach)
    
    #print(i)
    #print(est)
    
    if (newEst$ext < est$ext) {
      vec = newVec
      est = newEst
      
      intPts = c(intPts, newEst$int)
      extPts = c(extPts, newEst$ext)
      iterPts = c(iterPts, it)
      print(paste0("current ", newEst$ext, newEst$int))
      print(vec)
      
      plot(c(iterPts, iterPts), c(intPts, extPts), col=c(rep("green", length(intPts)), rep("red", length(extPts))), pch=20)
    }
    #print(newEst)
    
    print(paste0(iterations - it, " iterations remains"))
  }
}