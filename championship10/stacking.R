my.stacking = function (X, Y, teachers, aggregate) {
  models = foreach(teacher=teachers) %do% {
    teacher(cbind(X, Y))
  }
  
  extend = function (X) {
    answers = foreach(model=models, .combine=cbind) %do% model(X)
    cbind(X, answers) 
  }
  
  X = extend(X)
  finalModel = aggregate(cbind(X, Y))
  
  function (x) {
    x = extend(x)
    finalModel(x)
  }
}

my.stacking2 = function (X, Y, models, aggregate) {
  extend = function (X) {
    answers = foreach(model=models, .combine=cbind) %dopar% {
      model(X)
    }
    cbind(X, answers) 
  }
  
  X = extend(X)
  finalModel = aggregate(cbind(X, Y))
  
  function (x) {
    x = extend(x)
    finalModel(x)
  }
}