str = '7 8 9 10 12  13 15 17 19 22  14 16 19 22 26  26 30 35 41 47
5 5 6 7 8    9 10 12 13 16   9 11 13 15 16   18 21 25 29 34
3 3 4 5 6    6 7 8 9 11      6 8 9 11 13     13 15 17 20 24
2 2 3 3 4    4 5 5 6 7       4 5 6 7 9       9 10 12 14 17
4 4 5 6 7    8 9 10 11 13    9 11 13 15 18   18 21 24 28 33
3 3 3 4 5    5 6 7 8 9       6 7 9 10 12     12 14 17 20 24
2 2 2 3 3    3 4 5 5 6       4 5 6 7 9       8 10 12 14 17
1 1 2 2 2    2 3 3 4 4       3 3 4 5 6       6 7 8 10 12
2 2 3 3 4    4 5 5 6 7       6 7 8 10 12     12 13 16 19 22
1 2 2 2 3    3 3 4 4 5       4 5 6 7 8       8 9 11 13 16
1 1 1 1 2    2 2 2 3 3       3 3 4 5 6       5 6 8 9 11
1 1 1 1 1    1 1 2 2 2       2 2 3 3 4       4 4 5 6 8
1 1 1 2 2    2 2 3 3 4       4 4 5 6 7       7 8 10 12 14
1 1 1 1 1    1 2 2 2 3       2 3 3 4 5       5 6 7 8 10
0 1 1 1 1    1 1 1 1 2       2 2 2 3 3       3 4 5 6 7
0 0 1 1 1    1 1 1 1 1       1 1 2 2 2       2 3 3 4 5
0 0 0 0 0    0 0 0 1 1       1 1 1 2 2       2 2 3 3 4
0 0 0 0 0    0 0 0 0 0       1 1 1 1 1       1 2 2 2 3
0 0 0 0 0    0 0 0 0 0       0 1 1 1 1       1 1 1 2 2
0 0 0 0 0    0 0 0 0 0       0 0 1 1 1       1 1 1 1 1'

getScore2 = function (A, chol) {
  #if (chol == 1)
  #  return( A[1] )
  #if (chol == 2)
  #  return( (A[2] + A[3]) / 2 )
  #return( (A[4] + A[5]) / 2 )
  if (chol == 1)
    return( A[1] )
  if (chol == 2)
    return( (A[2] + A[3] + A[4]) / 3 )
  return( A[5] )
}

getScore1 = function (A, ap, chol) {
  if (             ap <= 120) return(                                        getScore2(A[1,], chol) )
  if (120 <= ap && ap <= 140) return( (getScore2(A[1,], chol) * (140 - ap) + getScore2(A[2,], chol) * (ap - 120)) / 20 )
  if (140 <= ap && ap <= 160) return( (getScore2(A[2,], chol) * (160 - ap) + getScore2(A[3,], chol) * (ap - 140)) / 20 )
  if (160 <= ap && ap <= 180) return( (getScore2(A[3,], chol) * (180 - ap) + getScore2(A[4,], chol) * (ap - 160)) / 20 )
  if (180 <= ap             ) return(  getScore2(A[4,], chol) )
}

getScore0 = function (gender, smoke, age, ap, chol) {
  A = score.table[[gender]][[smoke + 1]]
  
  if (            age <= 40) return(                                             getScore1(A[[1]], ap, chol) )
  if (40 <= age & age <= 50) return( (getScore1(A[[1]], ap, chol) * (50 - age) + getScore1(A[[2]], ap, chol) * (age - 40)) / 10)
  if (50 <= age & age <= 55) return( (getScore1(A[[2]], ap, chol) * (55 - age) + getScore1(A[[3]], ap, chol) * (age - 50)) / 5)
  if (55 <= age & age <= 60) return( (getScore1(A[[3]], ap, chol) * (60 - age) + getScore1(A[[4]], ap, chol) * (age - 55)) / 5)
  if (60 <= age & age <= 65) return( (getScore1(A[[4]], ap, chol) * (65 - age) + getScore1(A[[5]], ap, chol) * (age - 60)) / 5)
  if (65 <= age            ) return(  getScore1(A[[5]], ap, chol) )
}

getScore = function (x) {
  getScore0(x[['gender']], x[['smoke']], x[['age']] / 365, x[['ap_hi']], x[['cholesterol']])
}

strs = rev(strsplit(str, '\n')[[1]])

t1 = matrix(0, 4, 5)

score.table = list(
  #gender:
  #women
  list(
    #smoke
    #no
    list(
      #age
      t1, t1, t1, t1, t1
    ),
    #yes
    list(
      #age
      t1, t1, t1, t1, t1
    )
  ),
  #man
  list(
    #smoke
    #no
    list(
      #age
      t1, t1, t1, t1, t1
    ),
    #yes
    list(
      #age
      t1, t1, t1, t1, t1
    )
  )
)


for (i in 1:length(strs)) {
  r = strsplit(strs[[i]], ' ')[[1]]
  r = r[r != '']
  
  idx.ap = (i - 1) %% 4 + 1
  idx.age = (i - 1) %/% 4 + 1
  
  for (j in 1:length(r)) {
    idx.gender = (j - 1) %/% 10 + 1
    idx.smoke = (j - 1) %% 10 %/% 5 + 1
    idx.chol = (j - 1) %% 5 + 1
    #print(c(idx.gender, idx.smoke, idx.age, idx.ap, idx.chol))
    score.table[[idx.gender]][[idx.smoke]][[idx.age]][idx.ap, idx.chol] = as.integer(r[j])
  }
}

#if (getScore0(1, 1, 70, 170, 0) != 11) stop()
#if (getScore0(2, 1, 60, 140, 1) != 11) stop()
#if (getScore0(2, 0, 62.5, 150, 0) != (4+6+6+9)/4) stop()



