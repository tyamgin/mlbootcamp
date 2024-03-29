library(MASS)
require(pROC)
library(lightgbm)
library(plyr)  
library(dplyr)
library(foreach)
library(caret)
library(e1071)
library(nnet)
library(extraTrees)
library(doParallel)
library(glmnet)
library(splitstackshape)

# NOTES
# subs_csi_train$CONTACT_DATE                  01.05 - 31.05 (31 дн)   Дата опроса CSI абонента
#          test                 01.04 - 28.04  01.05 - 31.05 (59 дн)
# 
# subs_features_train$SNAP_DATE 01.06.01 - 01.05.02 (12 мес)           Дата расчета абонентских показателей
#               test            01.05.01 - 01.05.02 (13 мес)



debugSource("cv.R")
debugSource("tune.R")
debugSource("models.R")
debugSource("feat-select.R")

set.seed(888)

parse.date = function (s, n.d=31, n.m=12) {
  x = strsplit(s, '.', fixed=T)[[1]]
  if (length(x) < 2 || length(x) > 3)
    stop(paste0('Unsupported date format ', x))
  if (nchar(x[1]) != 2)
    stop(paste0('Unsupported day ', x[1]))
  if (nchar(x[2]) != 2)
    stop(paste0('Unsupported month ', x[2]))
  
  if (length(x) == 2) {
    return((as.integer(x[2])-1) * n.d + (as.integer(x[1])-1))
  }
  if (length(x) == 3) {
    if (nchar(x[3]) != 2)
      stop(paste0('Unsupported year ', x[3]))
    return((as.integer(x[3])-1) * n.d * n.m + (as.integer(x[2])-1) * n.d + (as.integer(x[1])-1))
  }
}

most.freq = function (vec) {
  vec[is.na(vec)] = 0
  as.integer(names(sort(table(vec), decreasing=T))[[1]])
}

not.most.freq.count = function (vec) {
  sum(vec != most.freq(vec))
}

subs_features_train = readRDS("cache/subs_features_train")
subs_csi_train = readRDS("cache/subs_csi_train")
subs_bs_data_session_train = readRDS("cache/subs_bs_data_session_train")
subs_bs_voice_session_train = readRDS("cache/subs_bs_voice_session_train")
subs_bs_consumption_train = readRDS("cache/subs_bs_consumption_train")

subs_features_test = readRDS("cache/subs_features_test")
subs_csi_test = readRDS("cache/subs_csi_test")
subs_bs_data_session_test = readRDS("cache/subs_bs_data_session_test")
subs_bs_voice_session_test = readRDS("cache/subs_bs_voice_session_test")
subs_bs_consumption_test = readRDS("cache/subs_bs_consumption_test")

subs_features_train = subs_features_train %>% arrange(SK_ID)
subs_csi_train = subs_csi_train %>% arrange(SK_ID)
subs_bs_data_session_train = subs_bs_data_session_train %>% arrange(SK_ID)
subs_bs_voice_session_train = subs_bs_voice_session_train %>% arrange(SK_ID)
subs_bs_consumption_train = subs_bs_consumption_train %>% arrange(SK_ID)

G_data = readRDS('cache/G_data')
bd = readRDS('cache/bd')
bd = left_join(bd, G_data, 'CELL_LAC_ID')
bd$G = ifelse(bd$i4G, 4, ifelse(bd$i3G, 3, 2))

bd_c_train = left_join(subs_bs_consumption_train, bd, 'CELL_LAC_ID')
bd_c_test = left_join(subs_bs_consumption_test, bd, 'CELL_LAC_ID')
bd_d_train = left_join(subs_bs_data_session_train, bd, 'CELL_LAC_ID')
bd_d_test = left_join(subs_bs_data_session_test, bd, 'CELL_LAC_ID')
bd_v_train = left_join(subs_bs_voice_session_train, bd, 'CELL_LAC_ID')
bd_v_test = left_join(subs_bs_voice_session_test, bd, 'CELL_LAC_ID')

act.last.3 = function (ACT, SNAP_DATE, metric) {
  SNAP_DATE = as.character(SNAP_DATE)
  ord = order(sapply(SNAP_DATE, parse.date), decreasing=T)
  SNAP_DATE = SNAP_DATE[ord]
  ACT = ACT[ord]
  
  if (metric == 'cz') {
    r = rle(ACT)
    v = r$lengths[r$values == 0]
    if (length(v) == 0) return(-1)
    return(max(v))
  }
  if (metric == 'm3') {
    return(mean(ACT[1:3], na.rm=T))
  }
  if (metric == 'q') {
    s = 0
    q = 1
    for (x in ACT) {
      s = s + q * x
      q = q * 0.75
    }
    return(s)
  }
  stop('invalid metric')
}

mean2 = function (x) {
  mean(x, na.rm=T)
}

fillna = function (x, val) {
  ifelse(is.na(x), x, val)
}

create_features = function (subs_features, subs_csi, bd_d, bd_v, bd_c) {
  ss = subs_features %>% group_by(SK_ID) %>% summarise(
    REVENUE = sum(REVENUE),
    C1_FREQ = most.freq(COM_CAT.1),
    C2_FREQ = most.freq(COM_CAT.2),
    C3 = mean(COM_CAT.3),
    C3_FREQ = most.freq(COM_CAT.3),
    BASE_TYPE_MEAN = mean(BASE_TYPE),
    BASE_TYPE_FREQ = most.freq(BASE_TYPE),
    ACT_MEAN = mean(ACT),
    ACT_CZ=act.last.3(ACT, SNAP_DATE, 'cz'),
    ACT_M3=act.last.3(ACT, SNAP_DATE, 'm3'),
    ACT_MQ=act.last.3(ACT, SNAP_DATE, 'q'),
    ARPU_GROUP = mean(ARPU_GROUP),
    ARPU_GROUP_FREQ = most.freq(ARPU_GROUP),
    C7_FREQ = most.freq(COM_CAT.7),
    C7_3456 = most.freq(COM_CAT.7) >= 3 & most.freq(COM_CAT.7) <= 6,
    C8_FREQ = most.freq(COM_CAT.8),
    DEVICE_TYPE_ID_FREQ = most.freq(DEVICE_TYPE_ID),
    INTERNET_TYPE_3G = mean(INTERNET_TYPE_ID == 1 | is.na(INTERNET_TYPE_ID), na.rm=T),
    INTERNET_TYPE_4G = mean(INTERNET_TYPE_ID == 2, na.rm=T),
    INTERNET_TYPE_NOT_3G = mean(INTERNET_TYPE_ID != 1, na.rm=T),
    INTERNET_TYPE_2G = mean(INTERNET_TYPE_ID == 3, na.rm=T),
    #INTERNET_TYPE_G = ifelse(INTERNET_TYPE_ID == 2, 4, ifelse(INTERNET_TYPE_ID == 3, 2, 3)),
    
    REVENUE = sum(REVENUE),
    ITC = sum(ITC),
    VAS = sum(VAS),
    
    REVENUE_M = mean(REVENUE),
    ITC_M = mean(ITC),
    VAS_M = mean(VAS),
    
    RENT_CHANNEL = sum(RENT_CHANNEL),
    ROAM = sum(ROAM),
    COST = mean(COST),
    
    RENT_CHANNEL_M = mean(RENT_CHANNEL),
    ROAM_M = mean(ROAM),
    COST_S = sum(COST),
    
    C17 = mean(COM_CAT.17),
    C18 = mean(COM_CAT.18),
    C19 = mean(COM_CAT.19),
    C20 = mean(COM_CAT.20),
    C21 = mean(COM_CAT.21),
    C22 = mean(COM_CAT.22),
    C23 = mean(COM_CAT.23),
    C25 = mean(COM_CAT.25),#01
    C25_FREQ = most.freq(COM_CAT.25),#01
    C26 = mean(COM_CAT.26),#01
    C26_FREQ = most.freq(COM_CAT.26),#01
    C27 = mean(COM_CAT.27),
    C28 = mean(COM_CAT.28),
    C29 = mean(COM_CAT.29),
    C30 = mean(COM_CAT.30),
    C31 = mean(COM_CAT.31),
    C32 = mean(COM_CAT.32),
    C33 = mean(COM_CAT.33),
    
    C17S = sum(COM_CAT.17),
    C18S = sum(COM_CAT.18),
    C19S = sum(COM_CAT.19),
    C20S = sum(COM_CAT.20),
    C21S = sum(COM_CAT.21),
    C22S = sum(COM_CAT.22),
    C23S = sum(COM_CAT.23),
    C27S = sum(COM_CAT.27),
    C28S = sum(COM_CAT.28),
    C29S = sum(COM_CAT.29),
    C30S = sum(COM_CAT.30),
    C31S = sum(COM_CAT.31),
    C32S = sum(COM_CAT.32),
    C33S = sum(COM_CAT.33),
    
    C34 = most.freq(COM_CAT.34),
    NEW_USER = n() < 12,
    C20_EQ_C22 = C20 == C22,
    count = n()
  )
  
  deli_umnozhai_cols = c('REVENUE', 'C3', 'BASE_TYPE_MEAN', 'ACT_MEAN', 'ARPU_GROUP', 'REVENUE_M', 'ITC_M', 'VAS_M', 'COST', 
                         'RENT_CHANNEL_M', 'ROAM_M',
                             'C17',
                             'C18',
                             'C19',
                             'C20',
                             'C21',
                             'C22',
                             'C23',
                             'C25',
                             'C26',
                             'C27',
                             'C28',
                             'C29',
                             'C30',
                             'C31',
                             'C32',
                             'C33'
  )
  for (i in 1:length(deli_umnozhai_cols)) {
    for (j in 1:length(deli_umnozhai_cols)) {
      if (i < j)
        ss[, paste0(deli_umnozhai_cols[i], '_MULT_', deli_umnozhai_cols[j])] = ss[, deli_umnozhai_cols[i]]*ss[,deli_umnozhai_cols[j]]
      if (i != j)
        ss[, paste0(deli_umnozhai_cols[i], '_DIV_', deli_umnozhai_cols[j])] = ss[, deli_umnozhai_cols[i]]/ss[,deli_umnozhai_cols[j]]
    }
  }
  
  for (x in 1:8)
    ss[, paste0('C1_', x)] = ss$C1_FREQ == x
  for (x in 1:17)
    ss[, paste0('C3_', x)] = ss$C3_FREQ == x

  feats_avg_chnn = list(
    'CELL_AVAILABILITY_2G', 'CELL_AVAILABILITY_3G', 'CELL_AVAILABILITY_4G', 'CSSR_2G', 'CSSR_3G', 'ERAB_PS_BLOCKING_RATE_LTE',
    'ERAB_PS_BLOCKING_RATE_PLMN_LTE', 'ERAB_PS_DROP_RATE_LTE', 'PART_CQI_QPSK_LTE', 'PART_MCS_QPSK_LTE', 'PSSR_2G', 'PSSR_3G',
    'PSSR_LTE', 'RAB_CS_BLOCKING_RATE_3G', 'RAB_CS_DROP_RATE_3G', 'RAB_PS_BLOCKING_RATE_3G', 'RAB_PS_DROP_RATE_3G', 'RBU_AVAIL_DL',
    'RBU_AVAIL_UL', 'RBU_OTHER_DL', 'RBU_OTHER_UL', 'RBU_OWN_DL', 'RBU_OWN_UL', 'RRC_BLOCKING_RATE_3G', 'RRC_BLOCKING_RATE_LTE',
    'SHO_FACTOR', 'TBF_DROP_RATE_2G', 'TCH_DROP_RATE_2G', 'UTIL_CE_DL_3G', 'UL_VOLUME_LTE', 'DL_VOLUME_LTE','TOTAL_DL_VOLUME_3G',
    'TOTAL_UL_VOLUME_3G',
  
    'AVEUSERNUMBER', 'AVEUSERNUMBER_PLMN', 'AVR_DL_HSPA_USER_3G', 'AVR_DL_R99_USER_3G', 'AVR_DL_USER_3G', 'AVR_DL_USER_LTE',
    'AVR_TX_POWER_3G', 'AVR_UL_HSPA_USER', 'AVR_UL_R99_USER', 'AVR_UL_USER_3G', 'AVR_UL_USER_LTE', 'DL_AVR_THROUGHPUT_3G',
    'DL_AVR_THROUGHPUT_LTE', 'DL_AVR_THROUGHPUT_R99', 'DL_MEAN_USER_THROUGHPUT_LTE', 'DL_MEAN_USER_THROUGHPUT_DL_2G',
    'DL_MEAN_USER_THROUGHPUT_HSPA3G', 'DL_MEAN_USER_THROUGHPUT_PLTE', 'DL_MEAN_USER_THROUGHPUT_REL93G', 'RBU_USED_DL',
    'RBU_USED_UL', 'RELATIVE_RBU_USED_DL','RELATIVE_RBU_USED_UL', 'RELATIVE_TX_POWER_3G', 'UL_AVR_THROUGHPUT_3G',
    'UL_AVR_THROUGHPUT_LTE', 'UL_AVR_THROUGHPUT_R99', 'UL_MEAN_USER_THROUGHPUT_LTE', 'UL_MEAN_USER_THROUGHPUT_HS3G',
    'UL_MEAN_USER_THROUGHPUT_PLTE', 'UL_MEAN_USER_THROUGHPUT_REL93G'
  )
  
  rename_cols = function (names, suffix) {
    as.character(sapply(names, function (x) ifelse(x == 'SK_ID', x, paste0(x, suffix))))
  }
  
  bd_d_grp = bd_d %>% group_by(SK_ID)
  dd1 = bd_d_grp %>% summarise(
    DATA_VOL_MB_MEAN = mean(DATA_VOL_MB, na.rm=T),
    DATA_VOL_MB_SUM = sum(DATA_VOL_MB, na.rm=T),
    DATA_VOL_MB_MIN = min(DATA_VOL_MB, na.rm=T),
    DATA_VOL_MB_MAX = max(DATA_VOL_MB, na.rm=T),
    count_d = n()
  )
  dd2 = bd_d_grp %>% summarise_at(
    do.call(vars, feats_avg_chnn),
    mean2
  )
  colnames(dd2) = rename_cols(colnames(dd2), '_D_MEAN')
  dd3 = left_join(bd_d %>% select(SK_ID, CELL_LAC_ID, CELL_AVAILABILITY_2G, CELL_AVAILABILITY_3G, CELL_AVAILABILITY_4G, G), 
                  ss %>% select(SK_ID, INTERNET_TYPE_3G,  INTERNET_TYPE_4G,  INTERNET_TYPE_2G), "SK_ID") %>% group_by(SK_ID) %>% summarise(
                    PHONE_GOVNO_FACTOR_D = mean(INTERNET_TYPE_2G * (G > 2) + INTERNET_TYPE_3G * (G > 3)),
                    PHONE_CELL_G_DIFF_D = mean(G - (INTERNET_TYPE_2G*2+INTERNET_TYPE_3G*3+INTERNET_TYPE_4G*4)),
                    CELL_AVAILABILITY_G_D = mean(fillna(CELL_AVAILABILITY_2G, 1)*INTERNET_TYPE_2G + 
                                               fillna(CELL_AVAILABILITY_3G, 1)*INTERNET_TYPE_3G +
                                               fillna(CELL_AVAILABILITY_4G, 1)*INTERNET_TYPE_4G) 
                  )
  dd = left_join(dd1, dd2, 'SK_ID')
  dd = left_join(dd, dd3, 'SK_ID')


  bd_v_grp = bd_v %>% group_by(SK_ID)
  vv1 = bd_v_grp %>% summarise(
    VOICE_DUR_MIN_MEAN = mean(VOICE_DUR_MIN, na.rm=T),
    VOICE_DUR_MIN_SUM = sum(VOICE_DUR_MIN, na.rm=T),
    VOICE_DUR_MIN_MIN = min(VOICE_DUR_MIN, na.rm=T),
    VOICE_DUR_MIN_MAX = max(VOICE_DUR_MIN, na.rm=T),
    count_v = n()
  )
  vv2 = bd_v_grp %>% summarise_at(
    do.call(vars, feats_avg_chnn),
    mean2
  )
  colnames(vv2) = rename_cols(colnames(vv2), '_V_MEAN')
  vv3 = left_join(bd_v %>% select(SK_ID, CELL_LAC_ID, CELL_AVAILABILITY_2G, CELL_AVAILABILITY_3G, CELL_AVAILABILITY_4G, G), 
                  ss %>% select(SK_ID, INTERNET_TYPE_3G,  INTERNET_TYPE_4G,  INTERNET_TYPE_2G), "SK_ID") %>% group_by(SK_ID) %>% summarise(
                    PHONE_GOVNO_FACTOR_V = mean(INTERNET_TYPE_2G * (G > 2) + INTERNET_TYPE_3G * (G > 3)),
                    PHONE_CELL_G_DIFF_V = mean(G - (INTERNET_TYPE_2G*2+INTERNET_TYPE_3G*3+INTERNET_TYPE_4G*4))
                  )
  vv = left_join(vv1, vv2, 'SK_ID')
  vv = left_join(vv, vv3, 'SK_ID')

  
  cc = bd_c %>% group_by(SK_ID) %>% summarise(
    SUM_MINUTES_SUM = sum(SUM_MINUTES, na.rm=T),
    SUM_DATA_MB_SUM = sum(SUM_DATA_MB, na.rm=T),
    SUM_DATA_MIN_SUM = sum(SUM_DATA_MIN, na.rm=T),
    SUM_DATA_MB_PER_MIN_SUM = sum(SUM_DATA_MB / SUM_DATA_MIN, na.rm=T),
    SUM_MINUTES_MEAN = mean(SUM_MINUTES, na.rm=T),
    SUM_DATA_MB_MEAN = mean(SUM_DATA_MB, na.rm=T),
    SUM_DATA_MIN_MEAN = mean(SUM_DATA_MIN, na.rm=T),
    SUM_DATA_MB_PER_MIN_MEAN = mean(SUM_DATA_MB / SUM_DATA_MIN, na.rm=T),
    
    SUM_DATA_MB_mult_CELL_AVAILABILITY_2G_MEAN = mean(SUM_DATA_MB * CELL_AVAILABILITY_2G, na.rm=T),
    SUM_DATA_MB_mult_CELL_AVAILABILITY_3G_MEAN = mean(SUM_DATA_MB * CELL_AVAILABILITY_3G, na.rm=T),
    SUM_DATA_MB_mult_CELL_AVAILABILITY_4G_MEAN = mean(SUM_DATA_MB * CELL_AVAILABILITY_4G, na.rm=T),
    SUM_DATA_MIN_mult_CELL_AVAILABILITY_2G_MEAN = mean(SUM_DATA_MIN * CELL_AVAILABILITY_2G, na.rm=T),
    SUM_DATA_MIN_mult_CELL_AVAILABILITY_3G_MEAN = mean(SUM_DATA_MIN * CELL_AVAILABILITY_3G, na.rm=T),
    SUM_DATA_MIN_mult_CELL_AVAILABILITY_4G_MEAN = mean(SUM_DATA_MIN * CELL_AVAILABILITY_4G, na.rm=T),
    SUM_MINUTES_mult_CELL_AVAILABILITY_2G_MEAN = mean(SUM_MINUTES * CELL_AVAILABILITY_2G, na.rm=T),
    SUM_MINUTES_mult_CELL_AVAILABILITY_3G_MEAN = mean(SUM_MINUTES * CELL_AVAILABILITY_3G, na.rm=T),
    SUM_MINUTES_mult_CELL_AVAILABILITY_4G_MEAN = mean(SUM_MINUTES * CELL_AVAILABILITY_4G, na.rm=T),
    SUM_MINUTES_mult_CSSR_2G_MEAN = mean(SUM_MINUTES * CSSR_2G, na.rm=T),
    SUM_MINUTES_mult_CSSR_3G_MEAN = mean(SUM_MINUTES * CSSR_3G, na.rm=T),
    
    count_c = n()
  )
  
  cc3 = left_join(bd_c %>% select(SK_ID, CELL_LAC_ID, CELL_AVAILABILITY_2G, CELL_AVAILABILITY_3G, CELL_AVAILABILITY_4G, G), 
                  ss %>% select(SK_ID, INTERNET_TYPE_3G,  INTERNET_TYPE_4G,  INTERNET_TYPE_2G), "SK_ID") %>% group_by(SK_ID) %>% summarise(
                    PHONE_GOVNO_FACTOR_C = mean(INTERNET_TYPE_2G * (G > 2) + INTERNET_TYPE_3G * (G > 3)),
                    PHONE_CELL_G_DIFF_C = mean(G - (INTERNET_TYPE_2G*2+INTERNET_TYPE_3G*3+INTERNET_TYPE_4G*4))
                    )
  
  cc = left_join(cc, cc3, 'SK_ID')
  
  XL = data.frame(
    SK_ID = subs_csi$SK_ID,
    CONTACT_DATE = sapply(as.character(subs_csi$CONTACT_DATE), parse.date, USE.NAMES=F)
  )
  if ("CSI" %in% colnames(subs_csi)) {
    XL$Y = subs_csi$CSI
  }

  XL1 = left_join(ss, dd, 'SK_ID')
  XL1 = left_join(XL1, vv, 'SK_ID')
  XL1 = left_join(XL1, cc, 'SK_ID')
  XL = left_join(XL1, XL, 'SK_ID')
  
  XL$count_d[is.na(XL$count_d)] = 0
  XL$count_v[is.na(XL$count_v)] = 0
  
  XL = cbind(
    count_d_0 = XL$count_d == 0,
    count_v_0 = XL$count_v == 0,
    C1_AVG_Y = 0, 
    C2_AVG_Y = 0, 
    XL
  )

  XL
}

my.extendedColsTrain = function (XL, trainFunc, feats) {
  C1_AVG_Y = foreach(i=1:7, .combine=c) %do% mean(XL[XL$C1_FREQ==i, ]$Y)
  C1_AVG_Y = order(order(C1_AVG_Y))
  
  C2_AVG_Y = foreach(i=1:83, .combine=c) %do% mean(XL[XL$C2_FREQ==i, ]$Y)
  C2_AVG_Y = order(order(C2_AVG_Y))
  
  XL$C1_AVG_Y = C1_AVG_Y[XL$C1_FREQ]
  XL$C2_AVG_Y = C2_AVG_Y[XL$C2_FREQ]
  
  model = trainFunc(extendXYCols(XL, feats))
  function (X) {
    X$C1_AVG_Y = C1_AVG_Y[X$C1_FREQ]
    X$C2_AVG_Y = C2_AVG_Y[X$C2_FREQ]
    model(extendXYCols(X, feats))
  }
}

my.fillNasTrain = function (XL, trainFunc, aggr=mean) {
  means = rep(0, ncol(XL))
  cnames = colnames(XL)
  for (j in 1:ncol(XL)) {
    means[j] = aggr(XL[, j], na.rm=T)
    XL[is.na(XL[, j]) | is.nan(XL[, j]), j] = means[j]
  }
  model = trainFunc(XL)
  function (X) {
    X = as.matrix(X) # WTF tibble ???
    for (j in 1:ncol(X)) {
      X[is.na(X[, j]) | is.nan(X[, j]), j] = means[j]
    }
    model(X)
  }
}

XL = create_features(subs_features_train, subs_csi_train, bd_d_train, bd_v_train, bd_c_train)

feats_lm = c('C28','AVR_DL_R99_USER_3G_V_MEAN','count_d_0','AVR_DL_R99_USER_3G_D_MEAN','C7_FREQ','C30S','SUM_DATA_MIN_mult_CELL_AVAILABILITY_2G_MEAN','C3_7','RELATIVE_TX_POWER_3G_V_MEAN','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','SUM_DATA_MIN_mult_CELL_AVAILABILITY_4G_MEAN','RBU_USED_UL_V_MEAN','TBF_DROP_RATE_2G_D_MEAN','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN','C21','RBU_AVAIL_DL_D_MEAN','ACT_CZ','SUM_MINUTES_mult_CSSR_2G_MEAN','SUM_DATA_MIN_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','C1_8','ERAB_PS_DROP_RATE_LTE_V_MEAN','BASE_TYPE_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','CELL_AVAILABILITY_3G_V_MEAN','SUM_DATA_MIN_SUM','VAS_M','RAB_CS_DROP_RATE_3G_D_MEAN','RENT_CHANNEL_M','C1_1','SUM_MINUTES_mult_CELL_AVAILABILITY_3G_MEAN','C26_FREQ','C22_DIV_ACT_MEAN','ITC_M_DIV_C20','C31_DIV_C20','ACT_MEAN_DIV_C33','PHONE_CELL_G_DIFF_D','C17_DIV_REVENUE','C3_2','C29_DIV_C33','C29','C30_DIV_COST','C20_MULT_C29','VAS_M_MULT_C27','RENT_CHANNEL_M_MULT_C28','REVENUE_M_DIV_C3','ROAM_M_MULT_C18','C25_MULT_C26','C27_MULT_C32','C22_MULT_C29','ARPU_GROUP_DIV_REVENUE','ACT_MEAN_MULT_ITC_M','ARPU_GROUP_DIV_VAS_M','C29_DIV_C20','C31_MULT_C32','C21_DIV_ACT_MEAN','C19_DIV_C21','VAS_M_DIV_ARPU_GROUP','C19_DIV_C25','ARPU_GROUP_MULT_COST','BASE_TYPE_MEAN_MULT_C29','AVR_TX_POWER_3G_D_MEAN','ROAM_M_DIV_C31','C26_DIV_ARPU_GROUP','C23_DIV_ARPU_GROUP','RENT_CHANNEL_M_DIV_C3','C26_MULT_C28')
feats_lmr = c('AVR_DL_R99_USER_3G_V_MEAN','count_d_0','AVR_DL_R99_USER_3G_D_MEAN','C7_FREQ','C30S','SUM_DATA_MIN_mult_CELL_AVAILABILITY_2G_MEAN','C3_7','RELATIVE_TX_POWER_3G_V_MEAN','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','SUM_DATA_MIN_mult_CELL_AVAILABILITY_4G_MEAN','RBU_USED_UL_V_MEAN','TBF_DROP_RATE_2G_D_MEAN','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN','C21','RBU_AVAIL_DL_D_MEAN','SUM_MINUTES_mult_CSSR_2G_MEAN','SUM_DATA_MIN_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','C1_8','ERAB_PS_DROP_RATE_LTE_V_MEAN','BASE_TYPE_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','CELL_AVAILABILITY_3G_V_MEAN','SUM_DATA_MIN_SUM','RAB_CS_DROP_RATE_3G_D_MEAN','RENT_CHANNEL_M','C1_1','SUM_MINUTES_mult_CELL_AVAILABILITY_3G_MEAN','ITC_M_DIV_C20','C31_DIV_C20','ACT_MEAN_DIV_C33','PHONE_CELL_G_DIFF_D','C3_2','C29_DIV_C33','C30_DIV_COST','VAS_M_MULT_C27','RENT_CHANNEL_M_MULT_C28','REVENUE_M_DIV_C3','ROAM_M_MULT_C18','C25_MULT_C26','C27_MULT_C32','ARPU_GROUP_DIV_REVENUE','ACT_MEAN_MULT_ITC_M','C29_DIV_C20','C31_MULT_C32','C21_DIV_ACT_MEAN','C19_DIV_C21','VAS_M_DIV_ARPU_GROUP','BASE_TYPE_MEAN_MULT_C29','AVR_TX_POWER_3G_D_MEAN','ROAM_M_DIV_C31','C26_DIV_ARPU_GROUP','C23_DIV_ARPU_GROUP','C26_MULT_C28','ROAM_M_MULT_C30','C32_DIV_VAS_M','C32_DIV_REVENUE_M','RENT_CHANNEL_M_MULT_C20','ACT_MEAN_DIV_C25','C29_DIV_C22','C3_DIV_ACT_MEAN','RENT_CHANNEL_M_DIV_C22','RENT_CHANNEL_M_MULT_C22','DL_MEAN_USER_THROUGHPUT_LTE_D_MEAN','RRC_BLOCKING_RATE_3G_D_MEAN','C28_DIV_C22','C29_MULT_C33','C28_DIV_C20','C17_DIV_REVENUE_M','C29_DIV_C21','C20_DIV_ACT_MEAN','C17_MULT_C29','ITC_M_DIV_C22','C32_DIV_REVENUE','C17_DIV_REVENUE','CELL_AVAILABILITY_G_D','C29_DIV_C23','C23_MULT_C29','C21_DIV_C20','C29_DIV_C19')
feats = c('ACT_MEAN','C30S','DATA_VOL_MB_MEAN','C23S','count_v','CSSR_3G_D_MEAN','RENT_CHANNEL','SUM_DATA_MIN_SUM','UL_VOLUME_LTE_D_MEAN','C33','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','RBU_OWN_DL_D_MEAN','SHO_FACTOR_V_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','INTERNET_TYPE_NOT_3G','AVR_DL_USER_LTE_D_MEAN','AVR_UL_USER_LTE_D_MEAN','PART_CQI_QPSK_LTE_D_MEAN','AVR_DL_R99_USER_3G_D_MEAN','TOTAL_DL_VOLUME_3G_V_MEAN','C1_FREQ','SUM_DATA_MIN_MEAN','C27','CSSR_2G_D_MEAN','RENT_CHANNEL_M','C7_FREQ','RBU_AVAIL_DL_D_MEAN','RENT_CHANNEL_M_MULT_C26','ACT_MEAN_DIV_REVENUE','C25_DIV_C26','RENT_CHANNEL_M_MULT_C20','REVENUE_M_MULT_VAS_M','PHONE_CELL_G_DIFF_D','ACT_MEAN_DIV_C19','VAS_M_MULT_C29','C19_DIV_BASE_TYPE_MEAN','C3_DIV_ITC_M','C27_DIV_C28','BASE_TYPE_MEAN_DIV_ACT_MEAN','BASE_TYPE_MEAN_MULT_C23','VAS_M_MULT_C19','ARPU_GROUP_DIV_C30','C19_DIV_RENT_CHANNEL_M','VOICE_DUR_MIN_MIN','SUM_MINUTES_mult_CELL_AVAILABILITY_3G_MEAN','RBU_OTHER_UL_D_MEAN','BASE_TYPE_MEAN_DIV_C17','C25_DIV_C27','ACT_MEAN_MULT_C27','C3_2','UL_MEAN_USER_THROUGHPUT_HS3G_V_MEAN','C17_DIV_C19','C3_DIV_C32','VAS_M_MULT_COST','SUM_MINUTES_mult_CELL_AVAILABILITY_2G_MEAN','RAB_CS_DROP_RATE_3G_D_MEAN','C27_DIV_BASE_TYPE_MEAN','SUM_MINUTES_mult_CSSR_2G_MEAN','C25_MULT_C26','REVENUE_M_DIV_ACT_MEAN','C33_DIV_C27','C17_DIV_REVENUE_M','C17_DIV_C33','C28_DIV_ITC_M','C21_DIV_C26','C17_DIV_C27','AVEUSERNUMBER_D_MEAN','C1_4','ACT_MEAN_MULT_C33','ACT_MEAN_DIV_BASE_TYPE_MEAN','count','RBU_OTHER_DL_D_MEAN','RENT_CHANNEL_M_DIV_C21','C19_DIV_C17','DEVICE_TYPE_ID_FREQ','C30_DIV_C23','C21_DIV_C27','C25_DIV_C19','C20_DIV_C22','VAS_M_DIV_C30','RENT_CHANNEL_M_MULT_C21')
feats_lm2 = c('AVR_DL_R99_USER_3G_V_MEAN','count_d_0','AVR_DL_R99_USER_3G_D_MEAN','C7_FREQ','SUM_DATA_MIN_mult_CELL_AVAILABILITY_2G_MEAN','RELATIVE_TX_POWER_3G_V_MEAN','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','SUM_DATA_MIN_mult_CELL_AVAILABILITY_4G_MEAN','RBU_USED_UL_V_MEAN','TBF_DROP_RATE_2G_D_MEAN','C21','RBU_AVAIL_DL_D_MEAN','SUM_MINUTES_mult_CSSR_2G_MEAN','SUM_DATA_MIN_MEAN','C1_8','ERAB_PS_DROP_RATE_LTE_V_MEAN','BASE_TYPE_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','CELL_AVAILABILITY_3G_V_MEAN','SUM_DATA_MIN_SUM','RAB_CS_DROP_RATE_3G_D_MEAN','C1_1','SUM_MINUTES_mult_CELL_AVAILABILITY_3G_MEAN','C22_DIV_ACT_MEAN','ACT_MEAN_DIV_C33','PHONE_CELL_G_DIFF_D','C17_DIV_REVENUE','C3_2','C29_DIV_C33','C29','C30_DIV_COST','C20_MULT_C29','RENT_CHANNEL_M_MULT_C28','REVENUE_M_DIV_C3','ROAM_M_MULT_C18','C25_MULT_C26','C27_MULT_C32','C22_MULT_C29','ARPU_GROUP_DIV_REVENUE','ACT_MEAN_MULT_ITC_M','ARPU_GROUP_DIV_VAS_M','C31_MULT_C32','C19_DIV_C21','VAS_M_DIV_ARPU_GROUP','BASE_TYPE_MEAN_MULT_C29','AVR_TX_POWER_3G_D_MEAN','ROAM_M_DIV_C31','C26_DIV_ARPU_GROUP','C23_DIV_ARPU_GROUP','RENT_CHANNEL_M_DIV_C3','C26_MULT_C28','C22_MULT_C28','ACT_MEAN_MULT_C27','C19_MULT_C27','C22_DIV_C33','C29_DIV_C25','C17_MULT_C31','ACT_MEAN_MULT_C21','C25_MULT_C31','C31_DIV_C3','C29_DIV_C23','RRC_BLOCKING_RATE_3G_D_MEAN','C29_MULT_C33','VAS_M_MULT_COST','ACT_MEAN_DIV_C22','SUM_DATA_MIN_mult_CELL_AVAILABILITY_3G_MEAN','SUM_DATA_MB_mult_CELL_AVAILABILITY_3G_MEAN','C27_MULT_C29','C33_DIV_C25','ROAM_M_MULT_C30','AVR_UL_USER_LTE_V_MEAN','AVR_DL_USER_LTE_V_MEAN','C27','RELATIVE_RBU_USED_UL_D_MEAN','C29_DIV_C32','C19_DIV_ARPU_GROUP','RENT_CHANNEL_M_MULT_C27','C23_MULT_C29','C19_DIV_REVENUE','C28_DIV_C22','C19_MULT_C32','UL_VOLUME_LTE_V_MEAN','ITC_M_DIV_C19','RBU_USED_UL_D_MEAN','C19_MULT_C28','C27_MULT_C28','C3_17','C27_DIV_COST','RRC_BLOCKING_RATE_LTE_D_MEAN','C22_DIV_C3')

feats_lm2 = c('ACT_MEAN','VAS_M_DIV_C33','PHONE_CELL_G_DIFF_D','C29_DIV_C23','C29_DIV_BASE_TYPE_MEAN','C1_1','C7_FREQ','AVR_TX_POWER_3G_V_MEAN','C1_8','SUM_MINUTES_mult_CSSR_3G_MEAN','count_d_0','BASE_TYPE_MEAN_DIV_C25','CSSR_3G_D_MEAN','C17_MULT_C29','RAB_CS_DROP_RATE_3G_D_MEAN','C23_MULT_C28','RENT_CHANNEL_M_DIV_C21','C3_2','COST_MULT_C28','REVENUE_M_DIV_C3','C30_DIV_RENT_CHANNEL_M','RRC_BLOCKING_RATE_LTE_D_MEAN','RBU_USED_UL_V_MEAN','C32_DIV_ARPU_GROUP')

feats.cut = function (f) unique(c(f, 'C1_FREQ', 'C2_FREQ', 'Y'))

stopCluster(cl)
cl = makeCluster(4)
registerDoParallel(cl)
algoLgb = function (XL, params, ._f) {
  my.extendedColsTrain(XL, function (XL) {
    my.fillNasTrain(XL, function (XL) {
      my.train.lgb(XL, params)
    }, aggr=median)
  }, feats)
}
algoLm = function (XL, params, ._f) {
  my.extendedColsTrain(XL, function (XL) {
    my.fillNasTrain(XL, function (XL) {
      my.train.lm2(XL, params)
    }, aggr=median)
  }, c(feats_lm2))
}
#my.gridSearch(XL, algoLm, expand.grid(lgbParams), verbose=T, folds=1:4, folds.mult=40, timefolds=T, resample.seed=2708, algo.seed=442)
my.gridSearch(XL[, feats.cut(feats_lm2)], algoLm, expand.grid(lgbParams), verbose=T, folds=rep(3,200), resample.seed=2708, algo.seed=442)
stopCluster(cl)

stopCluster(cl)
cl = makeCluster(4)
registerDoParallel(cl)
algoBlend = function (XL, params, ._f) {
  m1 = my.extendedColsTrain(XL, function (XL) {
    my.fillNasTrain(XL, function (XL) {
      my.train.lm(XL)
    }, aggr=median)
  }, feats_lm)
  m2 = my.extendedColsTrain(XL, function (XL) {
    my.fillNasTrain(XL, function (XL) {
      my.train.lmr(XL, lmrParams)
    }, aggr=median)
  }, feats_lmr)
  m3 = my.extendedColsTrain(XL, function (XL) {
    my.fillNasTrain(XL, function (XL) {
      my.train.lgb(XL, lgbParams)
    }, aggr=median)
  }, feats)
  m4 = my.extendedColsTrain(XL, function (XL) {
    my.fillNasTrain(XL, function (XL) {
      my.train.lm2(XL, lmrParams)
    }, aggr=median)
  }, feats_lm2)
  function (X) {
    p=0.19
    q=0.19
    v=0.26
    r = function (x) order(order(x)) / length(x)
    r(m1(X))*p + r(m2(X))*q + r(m4(X))*v + r(m3(X))*(1-p-q-v)
  }
}
validation.tqfold.parallel(XL[, feats.cut(c(feats, feats_lm, feats_lmr, feats_lm2))], algoBlend, folds=rep(3,200), resample.seed=2708, algo.seed=442)
stopCluster(cl)

stopCluster(cl)
cl = makeCluster(4)
registerDoParallel(cl)
addRemoveSelect(6000, XL, function (XL, params, features) {
  my.extendedColsTrain(XL, function (XL) {
    my.fillNasTrain(XL, function (XL) {
      my.train.lgb(XL, lgbParams)
    }, aggr=median)
  }, features)
}, startFeatures=feats)
stopCluster(cl)

stopCluster(cl)
cl = makeCluster(4)
registerDoParallel(cl)
addRemoveSelect(8000, XL, function (XL, params, features) {
  my.extendedColsTrain(XL, function (XL) {
    my.fillNasTrain(XL, function (XL) {
      my.train.lm2(XL, lmrParams)
    }, aggr=median)
  }, features)
}, startFeatures=feats_lm2)
stopCluster(cl)


XX = create_features(subs_features_test, subs_csi_test, bd_d_test, bd_v_test, bd_c_test)
model = algoBlend(XL)
#model = algoLm(XL, lgbParams)
XX$Y = model(XX)
resulted_table = left_join(subs_csi_test, XX, "SK_ID")
YY = resulted_table$Y
YY[is.na(YY)] = mean(YY[!is.na(YY)])
write(YY, file="res/result.txt", sep='\n')