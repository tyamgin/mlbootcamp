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
    return( 10^10 )
  
  L = nrow(XL)
  m = ncol(XL) - 1
  subXL = XL[, c(which(G == 1), m + 1)]
  mean(validation.tqfold(subXL, teach, folds=5, iters=2, verbose=F))
}

geneticSelect = function(iterations,  # количество итераций
                         XL, # выборка
                         teach, # teach(XL) - обучение
                         maxPopulationSize, # размер попул¤ции
                         mutationProb = 0.1, # вероятность мутации
                         estimate = tqfoldEstimation, # оценка
                         d = 100 # дополнительный параметр останова
) {
  L = nrow(XL)
  size = ncol(XL) - 1
  R = matrix(NA, maxPopulationSize, size + 1) # генерация хромосом
  
  chr = function(vec) { # получить хромосому
    vec[-length(vec)]
  }
  Q = function(vec) { # получить её оценку
    n = length(vec)
    if (is.na(vec[n]))
      vec[n] = estimate(XL, chr(vec), teach)
    
    vec[n]
  }
  
  for(i in 1:maxPopulationSize) {
    R[i, ] = c(sample(0:1, size, T), NA)
    R[i, size + 1] = Q(R[i, ])
  }
  
  best = R[1, ]
  bestIteration = 0
  
  plot(x=c(0, iterations), y=c(0,max(R[, size + 1])), col=c(0,0),
       xlab="Номер итерации", ylab="переобучение")
  
  for(iter in 1:iterations) {
    print(c("iteration", iter))
    
    # sort
    popSize = nrow(R)
    for(i in 1:popSize)
      R[i, size + 1] = Q(R[i,])
    R = R[order(R[,size + 1]), ]
    
    if (popSize > maxPopulationSize) { # отбор лучших
      popSize = maxPopulationSize;
      R = R[1:popSize, ]
    }
    
    if (Q(R[1, ]) < Q(best)) {
      best = R[1, ]
      bestIteration = iter
    }
    
    points(x=rep(iter, popSize), y=R[,size + 1], pch=rep(16, popSize), col=rep("blue", popSize))
    print(R[1, ])
    
    if (iter - bestIteration >= d)
      break
    
    for(i in 1:popSize) {
      for(j in 1:(popSize - 1)) {
        child = mutation(crossover(chr(R[i,]), chr(R[j,])), mutationProb)
        R = rbind(R, c(child, NA))
      }
    }
  }
  
  c(chr(best), Q(best))
}