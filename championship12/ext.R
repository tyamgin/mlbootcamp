jgc <- function() 
  .jcall("java/lang/System", method = "gc")

unnameMatrix = function (X) 
  as.matrix(unname(data.matrix(X)))

extendCols = function (X) {
  w = X$weight
  h = X$height / 100 # в метры
  #X$bmi = w / h^2 # https://ru.wikipedia.org/wiki/%D0%98%D0%BD%D0%B4%D0%B5%D0%BA%D1%81_%D0%BC%D0%B0%D1%81%D1%81%D1%8B_%D1%82%D0%B5%D0%BB%D0%B0
  #X$bmi4 = w / h^4
  #X$al_diff = X$ap_hi - X$ap_lo
  
  X$lol1 = ifelse(X$cholesterol == 1 & X$gluc == 1, 1, 0)
  X$lol2 = X$cholesterol - X$gluc
  X$lol3 = X$cholesterol + X$gluc + 3*X$smoke + X$alco - 4*X$active
  X$fat = (1.39 * w / h^2) + (0.16 * X$age / 365) - (10.34 * X$gender) - 9 # http://halls.md/race-body-fat-percentage/
  
  age = round(X$age / 365)
  e = function (x, L, R, p) ifelse(L <= x & x <= R, p, 0)
  
  women.score = 0
  #Framingham Risk Score for Women
  #Age: 
  women.score = women.score + (
    e(age, 20, 34, -7) + # 20–34: -7.   
    e(age, 35, 39, -3) + # 35–39: -3.  
    e(age, 40, 44, 0)  + # 40–44: 0.  
    e(age, 45, 49, 3)  + # 45–49: 3.  
    e(age, 50, 54, 6)  + # 50–54: 6.  
    e(age, 55, 59, 8)  + # 55–59: 8.   
    e(age, 60, 64, 10) + # 60–64: 10.   
    e(age, 65, 69, 12) + # 65–69: 12.  
    e(age, 70, 74, 14) + # 70–74: 14. 
    e(age, 75, 200, 16)   # 75–79: 16.
  )
  #Total cholesterol, mg/dL: 
  women.score = women.score + ifelse(20 <= age & age <= 39, 
    # Age 20–39 years: 
     (
      #   160-199:   4 points. 
      #   200-239:   8 points. 
      #   240-279:   11 points. 
      #   280+:      13 points. 
      e(X$cholesterol, 2, 2, 6) + e(X$cholesterol, 3, 3, 12)
    ), 0)
  women.score = women.score + ifelse(40 <= age & age <= 49,
    # Age 40–49 years: 
    (
    #   160-199:   3 points. 
    #   200-239:   6 points. 
    #   240-279:   8 points. 
    #   280+:      10 points. 
      e(X$cholesterol, 2, 2, 4) + e(X$cholesterol, 3, 3, 9)
    ), 0)
  women.score = women.score + ifelse(50 <= age & age <= 59,
    # Age 50–59 years: 
    (
    #   160-199:   2 points. 
    #   200-239:   4 points. 
    #   240-279:   5 points. 
    #   280+:      7 points. 
      e(X$cholesterol, 2, 2, 3) + e(X$cholesterol, 3, 3, 6)
    ), 0)
  women.score = women.score + ifelse(60 <= age & age <= 69,
    # Age 60–69 years: 
    (
    #   160-199:   1 point. 
    #   200-239:   2 points. 
    #   240-279:   3 points. 
    #   280+:      4 points.
      e(X$cholesterol, 2, 2, 2) + e(X$cholesterol, 3, 3, 3)
    ), 0)
  women.score = women.score + ifelse(70 <= age & age <= 79,
  # Age 70–79 years: 
    (
    #   160-199: 1 point. 
    #   200-239: 1 point. 
    #   240-279: 2 points.  
    #   280+:    2 points.
      e(X$cholesterol, 2, 2, 1) + e(X$cholesterol, 3, 3, 2)
    ), 0)
  #If cigarette smoker: 
  women.score = women.score + ifelse(X$smoke == 1, (
    e(age, 20, 39, 9) + # Age 20–39 years: 9 points. 
    e(age, 40, 49, 7) + # Age 40–49 years: 7 points. 
    e(age, 50, 59, 4) + # Age 50–59 years: 4 points. 
    e(age, 60, 69, 2) + # Age 60–69 years: 2 points. 
    e(age, 70, 79, 1)   # Age 70–79 years: 1 point.
  ), 0)
  #HDL cholesterol, mg/dL: 
  # 60+:    -1 point. 
  # 50-59:  0 points. 
  # 40-49:  1 point. 
  # 40-:    2 points.
  women.score = women.score + (
    e(X$cholesterol, 1, 1, -1) + 
    e(X$cholesterol, 2, 2, 1) + 
    e(X$cholesterol, 3, 3, 2)
  )
  #Systolic blood pressure, mm Hg: 
  # Untreated: 
  women.score = women.score + (
    #  120-:    0 points. 
    e(X$ap_hi, 120, 129, 3) + #  120-129: 1 point. 
    e(X$ap_hi, 130, 139, 4) + #  130-139: 2 points. 
    e(X$ap_hi, 140, 159, 5) + #  140-159: 3 points. 
    e(X$ap_hi, 160, 1000, 6)  #  160+:    4 points. 
  )
  # Treated: Under 120: 0 points. 120-129: 3 points. 130-139: 4 points. 140-159: 5 points. 160 or higher: 6 points.
  #10-year risk in %: Points total: 
  # Under 9 points: <1%. 
  # 9-12 points:    1%. 
  # 13-14 points:   2%. 
  # 15 points:      3%. 
  # 16 points:      4%. 
  # 17 points:      5%. 
  # 18 points:      6%. 
  # 19 points:      8%. 
  # 20 points:      11%. 
  # 21              14%, 
  # 22              17%, 
  # 23              22%, 
  # 24              27%, 
  #>25              Over 30%
  
  #Framingham Risk Score for Men
  #Age: 20–34 years: Minus 9 points. 35–39 years: Minus 4 points. 40–44 years: 0 points. 45–49 years: 3 points. 50–54 years: 6 points. 55–59 years: 8 points. 60–64 years: 10 points. 65–69 years: 11 points. 70–74 years: 12 points. 75–79 years: 13 points.
  #Total cholesterol, mg/dL: Age 20–39 years: Under 160: 0 points. 160-199: 4 points. 200-239: 7 points. 240-279: 9 points. 280 or higher: 11 points. • Age 40–49 years: Under 160: 0 points. 160-199: 3 points. 200-239: 5 points. 240-279: 6 points. 280 or higher: 8 points. • Age 50–59 years: Under 160: 0 points. 160-199: 2 points. 200-239: 3 points. 240-279: 4 points. 280 or higher: 5 points. • Age 60–69 years: Under 160: 0 points. 160-199: 1 point. 200-239: 1 point. 240-279: 2 points. 280 or higher: 3 points. • Age 70–79 years: Under 160: 0 points. 160-199: 0 points. 200-239: 0 points. 240-279: 1 point. 280 or higher: 1 point.
  #If cigarette smoker: Age 20–39 years: 8 points. • Age 40–49 years: 5 points. • Age 50–59 years: 3 points. • Age 60–69 years: 1 point. • Age 70–79 years: 1 point.
  #HDL cholesterol, mg/dL: 60 or higher: Minus 1 point. 50-59: 0 points. 40-49: 1 point. Under 40: 2 points.
  #Systolic blood pressure, mm Hg: Untreated: Under 120: 0 points. 120-129: 0 points. 130-139: 1 point. 140-159: 1 point. 160 or higher: 2 points. • Treated: Under 120: 0 points. 120-129: 1 point. 130-139: 2 points. 140-159: 2 points. 160 or higher: 3 points.
  #10-year risk in %: Points total: 0 point: <1%. 1-4 points: 1%. 5-6 points: 2%. 7 points: 3%. 8 points: 4%. 9 points: 5%. 10 points: 6%. 11 points: 8%. 12 points: 10%. 13 points: 12%. 14 points: 16%. 15 points: 20%. 16 points: 25%. 17 points or more: Over 30%.[20]
  
  #X$score = women.score
  
  X = subset(X, select=-c(id))
  
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
  
  function (X) model(proc(X))
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
  function (X) model(proc(X))
}