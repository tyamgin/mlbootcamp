XA = rbind(XLL[,-ncol(XLL)], XXX)
XA = XA[order(XA$ap_hi + XA$ap_lo / 10000), ]

prev = NULL
for (i in 1:nrow(XA)) {
  x = XA[i, ]
  if (!is.null(prev) && x$ap_hi == prev$ap_hi && x$ap_lo == prev$ap_lo) {
    next
  }
  
  if (x$ap_lo < 50 | x$ap_lo > 140 | x$ap_hi < 70 | x$ap_hi > 210) {
    print(paste0('m(', x$ap_hi, ', ', x$ap_lo, ', , )'))
    prev = x
  } else if (x$ap_lo > x$ap_hi) {
    print(paste0('m(', x$ap_hi, ', ', x$ap_lo, ', , )'))
    prev = x
  }
}