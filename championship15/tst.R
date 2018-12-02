set.seed(1232553)
#XL2 = XL[order(XL$CONTACT_DATE), ]
XL2 = XL[sample(nrow(XL)), ]
ts_idx = 1:round(nrow(XL2)/2)
ts1_idx = 1:round(nrow(XL2)/4)

XL_tr = XL2[-ts_idx, ]
XL_ts = XL2[ts_idx, -ncol(XL2)]
Y_ts = XL2[ts_idx, ]$Y

for (model in list(algoLgb(XL_tr, lgbParams), algoLm(XL_tr))) {
  print(roc(Y_ts, model(XL_ts))$auc)
  y = foreach(i=1:10, .combine=c) %do% {
    l = floor(nrow(XL2)/10)
    start = ((i-1)*l+1)
    end = start + l - 1
    r = roc(XL2$Y[start:end], model(XL2[start:end, -ncol(XL2)]))$auc
    #print(r)
    r
  }
  print(ggplot(data=data.frame(x=1:10,y=y,type=c(rep('test',5),rep('train',5))),aes(x,y,z)) + 
          xlab("CONTACT_DATE BIN") + ylab("SCORE") + 
          geom_bar(stat="identity", aes(fill=type)) + 
          scale_y_continuous(limits=c(0, 1))
  )
  print(roc(Y_ts, model(normalize_test(XL_tr, XL_ts, c("C27", "C29S"))))$auc)
}
