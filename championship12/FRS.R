# https://en.wikipedia.org/wiki/Framingham_Risk_Score

linearSpline = function (x, y, p) {
  if (p <= x[1]) 
    return( y[1] )
  for (i in 2:length(x))
    if (p <= x[i])
      return( ((x[i] - p) * y[i - 1] + (p - x[i - 1]) * y[i]) / (x[i] - x[i - 1]) )
  return( y[length(y)] )
}

women.spline.points = data.frame(
  x = c(0, 9, 12, 13, 14, 15:25                         ),
  y = c(0, 1, 1,  2,  2,  3:6, 8, 11, 14, 17, 22, 27, 30)
)

men.spline.points = data.frame(
  x = c(0, 1, 4, 5, 6, 7:17                          ),
  y = c(0, 1, 1, 2, 2, 3:6, 8, 10, 12, 16, 20, 25, 30)
)

getFRS = function (x) {
  asdasd(x[['gender']], x[['age']] / 365, x[['smoke']], x[['cholesterol']], x[['ap_hi']])
}

asdasd = function (gender, age, smoke, cholesterol, ap_hi) {
  m = function (a, b) (a + b) / 2
  
  score = 0
  
  if (gender == 1) {
    score = score + linearSpline(
      x = c(m(20, 34), m(35, 39), m(40, 44), m(45, 49), m(50, 54), m(55, 59), m(60, 64), m(65, 69), m(70, 74), 80),
      y = c(-7,        -3,        0,         3,         6,         8,         10,        12,        14,        16),
      age
    )
  } else {
    score = score + linearSpline(
      x = c(m(20, 34), m(35, 39), m(40, 44), m(45, 49), m(50, 54), m(55, 59), m(60, 64), m(65, 69), m(70, 74), 80),
      y = c(-9,        -4,        0,         3,         6,         8,         10,        11,        12,        13),
      age
    )
  }
  
  if (gender == 1) {
    if (cholesterol == 2) {
      score = score + linearSpline(
        x = c(m(20, 39), m(40, 49), m(50, 59), m(60, 69), m(70, 79)),
        y = c(6,         4,         3,         2,         1),
        age
      )
    } else if (cholesterol == 3){
      score = score + linearSpline(
        x = c(m(20, 39), m(40, 49), m(50, 59), m(60, 69), m(70, 79)),
        y = c(12,        9,         6,         3,         2),
        age
      )
    }
  } else {
    if (cholesterol == 2) {
      score = score + linearSpline(
        x = c(m(20, 39), m(40, 49), m(50, 59), m(60, 69), m(70, 79)),
        y = c(5.5,       4,         2.5,       1,         0),
        age
      )
    } else if (cholesterol == 3){
      score = score + linearSpline(
        x = c(m(20, 39), m(40, 49), m(50, 59), m(60, 69), m(70, 79)),
        y = c(10,        7,         4.5,       2.5,         1),
        age
      )
    }
  }
  
  if (smoke) {
    if (gender == 1) {
      score = score + linearSpline(
        x = c(m(20, 39), m(40, 49), m(50, 59), m(60, 69), m(70, 79)),
        y = c(9,         7,         4,         2,         1),
         age
      )
    } else {
      score = score + linearSpline(
        x = c(m(20, 39), m(40, 49), m(50, 59), m(60, 69), m(70, 79)),
        y = c(8,         5,         3,         1,         1),
        age
      )
    }
  }

  #HDL cholesterol, mg/dL: 
  # 60+:    -1 point. 
  # 50-59:  0 points. 
  # 40-49:  1 point. 
  # 40-:    2 points.
  score = score + linearSpline(
    x = c(1, 2, 3),
    y = c(-1, 1, 2),
    cholesterol
  )
  
  #Systolic blood pressure, mm Hg: 
  # Untreated: 
  if (gender == 1) {
    score = score + linearSpline(
      x = c(120, m(120, 129), m(130, 139), m(140, 159), 160),
      y = c(0,   3,           4,           5,           6),
      ap_hi
    )
  } else {
    score = score + linearSpline(
      x = c(120, m(120, 129), m(130, 139), m(140, 159), 160),
      y = c(0,   0,           1,           1,           2),
      ap_hi
    )
  }
  
  if (gender == 1)
    return( linearSpline(women.spline.points$x, women.spline.points$y, score) )
  else
    return( linearSpline(men.spline.points$x, men.spline.points$y, score) )
}


asdasd(1, (45 + 49)/2, 1, 2, 132)
asdasd(2, (45 + 49)/2, 1, 2, 132)