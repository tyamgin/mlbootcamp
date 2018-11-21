xll=as.data.frame(XL)
xxx=as.data.frame(XX)

stat1 = foreach (cc=1:ncol(XX), .combine=rbind) %do% {
  data.frame(
    name = colnames(xxx)[cc],
    nas_train = mean(is.na(xll[,cc])|is.nan(xll[,cc])),
    nas_test = mean(is.na(xxx[,cc])|is.nan(xxx[,cc])),
    mean_train = mean(xll[,cc], na.rm=T),
    mean_test = mean(xxx[,cc], na.rm=T)
  )
}

stat2 = stat1 %>% mutate(dff=abs(mean_test-mean_train)/abs(mean_train)) %>% arrange(dff)