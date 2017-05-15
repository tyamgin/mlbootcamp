print('starting repeats check...')

my.rowHash2 = function (x) {
  x = x[intCols]
  x = signif(x, 1)
  r = 0
  p = 1
  for(i in x) {
    r = r + p * i
    p = p * 1.1 
  }
  paste0(r, sum(x))
}


tbl = table(-1)
for(i in 1:nrow(XLL)) {
  tbl[my.rowHash2(XLL[i, -ncol(XLL)])] = XLL[i, ncol(XLL)]
}

for(i in 1:nrow(XXX)) {
  v = tbl[my.rowHash2(XXX[i,])]
  if (!is.na(v) && results[i] != v) {
    print(paste0('[repeats] fixed ', i, ', answer is ', v, ', not ', results[i]))
    results[i] = v
  }
}