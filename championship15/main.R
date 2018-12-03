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

if (F) {
  bs_avg_kpi_1_12 = readRDS('cache/bs_avg_kpi_1_12')
  bs_chnn_kpi_1_12 = readRDS('cache/bs_chnn_kpi_1_12')
  
  na3g = is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_3G)
  bs_avg_kpi_1_12$CELL_AVAILABILITY_3G[na3g] = bs_avg_kpi_1_12$CELL_AVAILABILITY_4G[na3g]
  ls3g = !is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_3G) & !is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_4G) & bs_avg_kpi_1_12$CELL_AVAILABILITY_3G < bs_avg_kpi_1_12$CELL_AVAILABILITY_4G
  bs_avg_kpi_1_12$CELL_AVAILABILITY_3G[ls3g] = bs_avg_kpi_1_12$CELL_AVAILABILITY_4G[ls3g]
  
  na2g = is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_2G)
  bs_avg_kpi_1_12$CELL_AVAILABILITY_2G[na2g] = bs_avg_kpi_1_12$CELL_AVAILABILITY_3G[na2g]
  ls2g = !is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_2G) & !is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_3G) & bs_avg_kpi_1_12$CELL_AVAILABILITY_2G < bs_avg_kpi_1_12$CELL_AVAILABILITY_3G
  bs_avg_kpi_1_12$CELL_AVAILABILITY_2G[ls2g] = bs_avg_kpi_1_12$CELL_AVAILABILITY_3G[ls2g]
  
  avg1 = bs_avg_kpi_1_12 %>% group_by(CELL_LAC_ID) %>% summarise(
    CELL_AVAILABILITY_2G = mean(CELL_AVAILABILITY_2G, na.rm=T),
    CELL_AVAILABILITY_3G = mean(CELL_AVAILABILITY_3G, na.rm=T),
    CELL_AVAILABILITY_4G = mean(CELL_AVAILABILITY_4G, na.rm=T),
    CSSR_2G = mean(CSSR_2G, na.rm=T),
    CSSR_3G = mean(CSSR_3G, na.rm=T),
    ERAB_PS_BLOCKING_RATE_LTE = mean(ERAB_PS_BLOCKING_RATE_LTE, na.rm=T),
    ERAB_PS_BLOCKING_RATE_PLMN_LTE = mean(ERAB_PS_BLOCKING_RATE_PLMN_LTE, na.rm=T),
    ERAB_PS_DROP_RATE_LTE = mean(ERAB_PS_DROP_RATE_LTE, na.rm=T)
  )
  
  chnn1 = bs_chnn_kpi_1_12 %>% group_by(CELL_LAC_ID) %>% summarise(
    AVEUSERNUMBER = mean(AVEUSERNUMBER, na.rm=T),
    AVEUSERNUMBER_PLMN = mean(AVEUSERNUMBER_PLMN, na.rm=T),
    AVR_DL_HSPA_USER_3G = mean(AVR_DL_HSPA_USER_3G, na.rm=T),
    AVR_DL_R99_USER_3G = mean(AVR_DL_R99_USER_3G, na.rm=T),
    AVR_DL_USER_3G = mean(AVR_DL_USER_3G, na.rm=T),
    AVR_DL_USER_LTE = mean(AVR_DL_USER_LTE, na.rm=T),
    AVR_TX_POWER_3G = mean(AVR_TX_POWER_3G, na.rm=T),
    AVR_UL_HSPA_USER = mean(AVR_UL_HSPA_USER, na.rm=T),
    AVR_UL_R99_USER = mean(AVR_UL_R99_USER, na.rm=T),
    AVR_UL_USER_3G = mean(AVR_UL_USER_3G, na.rm=T)
  )
  
  saveRDS(avg1, 'cache/avg1', compress=F)
  saveRDS(chnn1, 'cache/chnn1', compress=F)
} else {
  avg1 = readRDS('cache/avg1')
  chnn1 = readRDS('cache/chnn1')
}

if (F) {
  bs_avg_kpi_2 = readRDS('cache/bs_avg_kpi_2')
  bs_chnn_kpi_2 = readRDS('cache/bs_chnn_kpi_2')
  
  avg2 = bs_avg_kpi_2 %>% group_by(CELL_LAC_ID) %>% summarise(
    PART_CQI_QPSK_LTE = mean(PART_CQI_QPSK_LTE, na.rm=T),
    PART_MCS_QPSK_LTE = mean(PART_MCS_QPSK_LTE, na.rm=T),
    #PROC_LOAD_3G = mean(PROC_LOAD_3G, na.rm=T),                      #пустая колонка
    PSSR_2G = mean(PSSR_2G, na.rm=T),
    PSSR_3G = mean(PSSR_3G, na.rm=T),
    PSSR_LTE = mean(PSSR_LTE, na.rm=T),
    RAB_CS_BLOCKING_RATE_3G = mean(RAB_CS_BLOCKING_RATE_3G, na.rm=T),
    RAB_CS_DROP_RATE_3G = mean(RAB_CS_DROP_RATE_3G, na.rm=T),
    RAB_PS_BLOCKING_RATE_3G = mean(RAB_PS_BLOCKING_RATE_3G, na.rm=T),
    RAB_PS_DROP_RATE_3G = mean(RAB_PS_DROP_RATE_3G, na.rm=T),
    RBU_AVAIL_DL = mean(RBU_AVAIL_DL, na.rm=T)
    #RBU_AVAIL_DL_LTE = mean(RBU_AVAIL_DL_LTE, na.rm=T)               #пустая колонка
  )
  
  # aaa=as.data.frame(chnn3); for (ccc in colnames(aaa)) if (sum(!is.nan(aaa[,ccc]))==0) print(ccc)
  
  chnn2 = bs_chnn_kpi_2 %>% group_by(CELL_LAC_ID) %>% summarise(
    AVR_UL_USER_LTE = mean(AVR_UL_USER_LTE, na.rm=T),
    DL_AVR_THROUGHPUT_3G = mean(DL_AVR_THROUGHPUT_3G, na.rm=T),
    DL_AVR_THROUGHPUT_LTE = mean(DL_AVR_THROUGHPUT_LTE, na.rm=T),
    DL_AVR_THROUGHPUT_R99 = mean(DL_AVR_THROUGHPUT_R99, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_LTE = mean(DL_MEAN_USER_THROUGHPUT_LTE, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_DL_2G = mean(DL_MEAN_USER_THROUGHPUT_DL_2G, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_HSPA3G = mean(DL_MEAN_USER_THROUGHPUT_HSPA3G, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_PLTE = mean(DL_MEAN_USER_THROUGHPUT_PLTE, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_REL93G = mean(DL_MEAN_USER_THROUGHPUT_REL93G, na.rm=T),
    #HSDPA_USERS_3G = mean(HSDPA_USERS_3G, na.rm=T),    #пустая колонка
    #HSUPA_USERS_3G = mean(HSUPA_USERS_3G, na.rm=T),    #пустая колонка
    RBU_USED_DL = mean(RBU_USED_DL, na.rm=T)
  )
  saveRDS(avg2, 'cache/avg2', compress=F)
  saveRDS(chnn2, 'cache/chnn2', compress=F)
} else {
  avg2 = readRDS('cache/avg2')
  chnn2 = readRDS('cache/chnn2')
}

if (F) {
  bs_avg_kpi_3 = readRDS('cache/bs_avg_kpi_3')
  bs_avg_kpi_4 = readRDS('cache/bs_avg_kpi_4')
  bs_chnn_kpi_3 = readRDS('cache/bs_chnn_kpi_3')

  avg3 = bs_avg_kpi_3 %>% group_by(CELL_LAC_ID) %>% summarise(
    RBU_AVAIL_UL = mean(RBU_AVAIL_UL, na.rm=T),
    RBU_OTHER_DL = mean(RBU_OTHER_DL, na.rm=T),
    RBU_OTHER_UL = mean(RBU_OTHER_UL, na.rm=T),
    RBU_OWN_DL = mean(RBU_OWN_DL, na.rm=T),
    RBU_OWN_UL = mean(RBU_OWN_UL, na.rm=T),
    RRC_BLOCKING_RATE_3G = mean(RRC_BLOCKING_RATE_3G, na.rm=T),
    RRC_BLOCKING_RATE_LTE = mean(RRC_BLOCKING_RATE_LTE, na.rm=T),
    #RTWP_3G = mean(RTWP_3G, na.rm=T),            ####
    SHO_FACTOR = mean(SHO_FACTOR, na.rm=T),
    TBF_DROP_RATE_2G = mean(TBF_DROP_RATE_2G, na.rm=T),
    TCH_DROP_RATE_2G = mean(TCH_DROP_RATE_2G, na.rm=T)
    #UTIL_BRD_CPU_3G = mean(UTIL_BRD_CPU_3G, na.rm=T)  ####
  )
  
  avg4 = bs_avg_kpi_4 %>% group_by(CELL_LAC_ID) %>% summarise(
    UTIL_CE_DL_3G = mean(UTIL_CE_DL_3G, na.rm=T),
    #UTIL_CE_HW_DL_3G = mean(UTIL_CE_HW_DL_3G, na.rm=T),  #####
    #UTIL_CE_UL_3G = mean(UTIL_CE_UL_3G, na.rm=T),        #####
    #UTIL_SUBUNITS_3G = mean(UTIL_SUBUNITS_3G, na.rm=T),  #####
    UL_VOLUME_LTE = mean(UL_VOLUME_LTE, na.rm=T),
    DL_VOLUME_LTE = mean(DL_VOLUME_LTE, na.rm=T),
    TOTAL_DL_VOLUME_3G = mean(TOTAL_DL_VOLUME_3G, na.rm=T),
    TOTAL_UL_VOLUME_3G = mean(TOTAL_UL_VOLUME_3G, na.rm=T)
  )

  chnn3 = bs_chnn_kpi_3 %>% group_by(CELL_LAC_ID) %>% summarise(
    RBU_USED_UL = mean(RBU_USED_UL, na.rm=T),
    RELATIVE_RBU_USED_DL = mean(RELATIVE_RBU_USED_DL, na.rm=T),
    RELATIVE_RBU_USED_UL = mean(RELATIVE_RBU_USED_UL, na.rm=T),
    RELATIVE_TX_POWER_3G = mean(RELATIVE_TX_POWER_3G, na.rm=T),
    UL_AVR_THROUGHPUT_3G = mean(UL_AVR_THROUGHPUT_3G, na.rm=T),
    UL_AVR_THROUGHPUT_LTE = mean(UL_AVR_THROUGHPUT_LTE, na.rm=T),
    UL_AVR_THROUGHPUT_R99 = mean(UL_AVR_THROUGHPUT_R99, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_LTE = mean(UL_MEAN_USER_THROUGHPUT_LTE, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_HS3G = mean(UL_MEAN_USER_THROUGHPUT_HS3G, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_PLTE = mean(UL_MEAN_USER_THROUGHPUT_PLTE, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_REL93G = mean(UL_MEAN_USER_THROUGHPUT_REL93G, na.rm=T)
  )
  saveRDS(avg3, 'cache/avg3', compress=F)
  saveRDS(avg4, 'cache/avg4', compress=F)
  saveRDS(chnn3, 'cache/chnn3', compress=F)
} else {
  avg3 = readRDS('cache/avg3')
  avg4 = readRDS('cache/avg4')
  chnn3 = readRDS('cache/chnn3')
}

cons1_train = left_join(subs_bs_consumption_train, avg1, 'CELL_LAC_ID')
cons1_test = left_join(subs_bs_consumption_test, avg1, 'CELL_LAC_ID')

avg1 = left_join(avg1, avg2, 'CELL_LAC_ID')
avg1 = left_join(avg1, avg3, 'CELL_LAC_ID')
avg1 = left_join(avg1, avg4, 'CELL_LAC_ID')
chnn1 = left_join(chnn1, chnn2, 'CELL_LAC_ID')
chnn1 = left_join(chnn1, chnn3, 'CELL_LAC_ID')
  
avg1_d_train = left_join(subs_bs_data_session_train, avg1, 'CELL_LAC_ID')
avg1_d_test = left_join(subs_bs_data_session_test, avg1, 'CELL_LAC_ID')
avg1_v_train = left_join(subs_bs_voice_session_train, avg1, 'CELL_LAC_ID')
avg1_v_test = left_join(subs_bs_voice_session_test, avg1, 'CELL_LAC_ID')

chnn1_d_train = left_join(subs_bs_data_session_train, chnn1, 'CELL_LAC_ID')
chnn1_d_test = left_join(subs_bs_data_session_test, chnn1, 'CELL_LAC_ID')
chnn1_v_train = left_join(subs_bs_voice_session_train, chnn1, 'CELL_LAC_ID')
chnn1_v_test = left_join(subs_bs_voice_session_test, chnn1, 'CELL_LAC_ID')


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

create_features = function (subs_features, subs_csi, avg1_d, avg1_v, chnn1_d, chnn1_v, cons1) {
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
    INTERNET_TYPE_ID_1 = mean(INTERNET_TYPE_ID == 1, na.rm=T),
    INTERNET_TYPE_ID_2 = mean(INTERNET_TYPE_ID == 2 | is.na(INTERNET_TYPE_ID), na.rm=T),
    INTERNET_TYPE_ID_NOT_1 = mean(INTERNET_TYPE_ID != 1, na.rm=T),
    INTERNET_TYPE_ID_3 = mean(INTERNET_TYPE_ID == 3, na.rm=T),
    
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
    count = n()
  )
  
  for (x in 1:8)
    ss[, paste0('C1_', x)] = ss$C1_FREQ == x
  for (x in 1:17)
    ss[, paste0('C3_', x)] = ss$C3_FREQ == x

  dd = avg1_d %>% group_by(SK_ID) %>% summarise(
    DATA_VOL_MB_MEAN = mean(DATA_VOL_MB, na.rm=T),
    DATA_VOL_MB_SUM = sum(DATA_VOL_MB, na.rm=T),
    DATA_VOL_MB_MIN = min(DATA_VOL_MB, na.rm=T),
    DATA_VOL_MB_MAX = max(DATA_VOL_MB, na.rm=T),
    
    CELL_AVAILABILITY_2G_D_MEAN = mean(CELL_AVAILABILITY_2G, na.rm=T),
    CELL_AVAILABILITY_3G_D_MEAN = mean(CELL_AVAILABILITY_3G, na.rm=T),
    CELL_AVAILABILITY_4G_D_MEAN = mean(CELL_AVAILABILITY_4G, na.rm=T),
    CSSR_2G_D_MEAN = mean(CSSR_2G, na.rm=T),
    CSSR_3G_D_MEAN = mean(CSSR_3G, na.rm=T),
    ERAB_PS_BLOCKING_RATE_LTE_D_MEAN = mean(ERAB_PS_BLOCKING_RATE_LTE, na.rm=T),
    ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN = mean(ERAB_PS_BLOCKING_RATE_PLMN_LTE, na.rm=T),
    ERAB_PS_DROP_RATE_LTE_D_MEAN = mean(ERAB_PS_DROP_RATE_LTE, na.rm=T),
    
    PART_CQI_QPSK_LTE_D_MEAN = mean(PART_CQI_QPSK_LTE, na.rm=T),
    PART_MCS_QPSK_LTE_D_MEAN = mean(PART_MCS_QPSK_LTE, na.rm=T),
    PSSR_2G_D_MEAN = mean(PSSR_2G, na.rm=T),
    PSSR_3G_D_MEAN = mean(PSSR_3G, na.rm=T),
    PSSR_LTE_D_MEAN = mean(PSSR_LTE, na.rm=T),
    RAB_CS_BLOCKING_RATE_3G_D_MEAN = mean(RAB_CS_BLOCKING_RATE_3G, na.rm=T),
    RAB_CS_DROP_RATE_3G_D_MEAN = mean(RAB_CS_DROP_RATE_3G, na.rm=T),
    RAB_PS_BLOCKING_RATE_3G_D_MEAN = mean(RAB_PS_BLOCKING_RATE_3G, na.rm=T),
    RAB_PS_DROP_RATE_3G_D_MEAN = mean(RAB_PS_DROP_RATE_3G, na.rm=T),
    RBU_AVAIL_DL_D_MEAN = mean(RBU_AVAIL_DL, na.rm=T),
    
    RBU_AVAIL_UL_D_MEAN = mean(RBU_AVAIL_UL, na.rm=T),
    RBU_OTHER_DL_D_MEAN = mean(RBU_OTHER_DL, na.rm=T),
    RBU_OTHER_UL_D_MEAN = mean(RBU_OTHER_UL, na.rm=T),
    RBU_OWN_DL_D_MEAN = mean(RBU_OWN_DL, na.rm=T),
    RBU_OWN_UL_D_MEAN = mean(RBU_OWN_UL, na.rm=T),
    RRC_BLOCKING_RATE_3G_D_MEAN = mean(RRC_BLOCKING_RATE_3G, na.rm=T),
    RRC_BLOCKING_RATE_LTE_D_MEAN = mean(RRC_BLOCKING_RATE_LTE, na.rm=T),
    SHO_FACTOR_D_MEAN = mean(SHO_FACTOR, na.rm=T),
    TBF_DROP_RATE_2G_D_MEAN = mean(TBF_DROP_RATE_2G, na.rm=T),
    TCH_DROP_RATE_2G_D_MEAN = mean(TCH_DROP_RATE_2G, na.rm=T),
    UTIL_CE_DL_3G_D_MEAN = mean(UTIL_CE_DL_3G, na.rm=T),
    UL_VOLUME_LTE_D_MEAN = mean(UL_VOLUME_LTE, na.rm=T),
    DL_VOLUME_LTE_D_MEAN = mean(DL_VOLUME_LTE, na.rm=T),
    TOTAL_DL_VOLUME_3G_D_MEAN = mean(TOTAL_DL_VOLUME_3G, na.rm=T),
    TOTAL_UL_VOLUME_3G_D_MEAN = mean(TOTAL_UL_VOLUME_3G, na.rm=T),
    
    count_d = n()
  )
  
  ddd = chnn1_d %>% group_by(SK_ID) %>% summarise(
    AVEUSERNUMBER_D_MEAN = mean(AVEUSERNUMBER, na.rm=T),
    AVEUSERNUMBER_PLMN_D_MEAN = mean(AVEUSERNUMBER_PLMN, na.rm=T),
    AVR_DL_HSPA_USER_3G_D_MEAN = mean(AVR_DL_HSPA_USER_3G, na.rm=T),
    AVR_DL_R99_USER_3G_D_MEAN = mean(AVR_DL_R99_USER_3G, na.rm=T),
    AVR_DL_USER_3G_D_MEAN = mean(AVR_DL_USER_3G, na.rm=T),
    AVR_DL_USER_LTE_D_MEAN = mean(AVR_DL_USER_LTE, na.rm=T),
    AVR_TX_POWER_3G_D_MEAN = mean(AVR_TX_POWER_3G, na.rm=T),
    AVR_UL_HSPA_USER_D_MEAN = mean(AVR_UL_HSPA_USER, na.rm=T),
    AVR_UL_R99_USER_D_MEAN = mean(AVR_UL_R99_USER, na.rm=T),
    AVR_UL_USER_3G_D_MEAN = mean(AVR_UL_USER_3G, na.rm=T),
    
    AVR_UL_USER_LTE_D_MEAN = mean(AVR_UL_USER_LTE, na.rm=T),
    DL_AVR_THROUGHPUT_3G_D_MEAN = mean(DL_AVR_THROUGHPUT_3G, na.rm=T),
    DL_AVR_THROUGHPUT_LTE_D_MEAN = mean(DL_AVR_THROUGHPUT_LTE, na.rm=T),
    DL_AVR_THROUGHPUT_R99_D_MEAN = mean(DL_AVR_THROUGHPUT_R99, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_LTE_D_MEAN = mean(DL_MEAN_USER_THROUGHPUT_LTE, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_DL_2G_D_MEAN = mean(DL_MEAN_USER_THROUGHPUT_DL_2G, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN = mean(DL_MEAN_USER_THROUGHPUT_HSPA3G, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN = mean(DL_MEAN_USER_THROUGHPUT_PLTE, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_REL93G_D_MEAN = mean(DL_MEAN_USER_THROUGHPUT_REL93G, na.rm=T),
    RBU_USED_DL_D_MEAN = mean(RBU_USED_DL, na.rm=T),
    
    RBU_USED_UL_D_MEAN = mean(RBU_USED_UL, na.rm=T),
    RELATIVE_RBU_USED_DL_D_MEAN = mean(RELATIVE_RBU_USED_DL, na.rm=T),
    RELATIVE_RBU_USED_UL_D_MEAN = mean(RELATIVE_RBU_USED_UL, na.rm=T),
    RELATIVE_TX_POWER_3G_D_MEAN = mean(RELATIVE_TX_POWER_3G, na.rm=T),
    UL_AVR_THROUGHPUT_3G_D_MEAN = mean(UL_AVR_THROUGHPUT_3G, na.rm=T),
    UL_AVR_THROUGHPUT_LTE_D_MEAN = mean(UL_AVR_THROUGHPUT_LTE, na.rm=T),
    UL_AVR_THROUGHPUT_R99_D_MEAN = mean(UL_AVR_THROUGHPUT_R99, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_LTE_D_MEAN = mean(UL_MEAN_USER_THROUGHPUT_LTE, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_HS3G_D_MEAN = mean(UL_MEAN_USER_THROUGHPUT_HS3G, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN = mean(UL_MEAN_USER_THROUGHPUT_PLTE, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_REL93G_D_MEAN = mean(UL_MEAN_USER_THROUGHPUT_REL93G, na.rm=T)
  )
  
  vv = avg1_v %>% group_by(SK_ID) %>% summarise(
    VOICE_DUR_MIN_MEAN = mean(VOICE_DUR_MIN, na.rm=T),
    VOICE_DUR_MIN_SUM = sum(VOICE_DUR_MIN, na.rm=T),
    VOICE_DUR_MIN_MIN = min(VOICE_DUR_MIN, na.rm=T),
    VOICE_DUR_MIN_MAX = max(VOICE_DUR_MIN, na.rm=T),
    
    CELL_AVAILABILITY_2G_V_MEAN = mean(CELL_AVAILABILITY_2G, na.rm=T),
    CELL_AVAILABILITY_3G_V_MEAN = mean(CELL_AVAILABILITY_3G, na.rm=T),
    CELL_AVAILABILITY_4G_V_MEAN = mean(CELL_AVAILABILITY_4G, na.rm=T),
    CSSR_2G_V_MEAN = mean(CSSR_2G, na.rm=T),
    CSSR_3G_V_MEAN = mean(CSSR_3G, na.rm=T),
    ERAB_PS_BLOCKING_RATE_LTE_V_MEAN = mean(ERAB_PS_BLOCKING_RATE_LTE, na.rm=T),
    ERAB_PS_BLOCKING_RATE_PLMN_LTE_V_MEAN = mean(ERAB_PS_BLOCKING_RATE_PLMN_LTE, na.rm=T),
    ERAB_PS_DROP_RATE_LTE_V_MEAN = mean(ERAB_PS_DROP_RATE_LTE, na.rm=T),
    
    PART_CQI_QPSK_LTE_V_MEAN = mean(PART_CQI_QPSK_LTE, na.rm=T),
    PART_MCS_QPSK_LTE_V_MEAN = mean(PART_MCS_QPSK_LTE, na.rm=T),
    PSSR_2G_V_MEAN = mean(PSSR_2G, na.rm=T),
    PSSR_3G_V_MEAN = mean(PSSR_3G, na.rm=T),
    PSSR_LTE_V_MEAN = mean(PSSR_LTE, na.rm=T),
    RAB_CS_BLOCKING_RATE_3G_V_MEAN = mean(RAB_CS_BLOCKING_RATE_3G, na.rm=T),
    RAB_CS_DROP_RATE_3G_V_MEAN = mean(RAB_CS_DROP_RATE_3G, na.rm=T),
    RAB_PS_BLOCKING_RATE_3G_V_MEAN = mean(RAB_PS_BLOCKING_RATE_3G, na.rm=T),
    RAB_PS_DROP_RATE_3G_V_MEAN = mean(RAB_PS_DROP_RATE_3G, na.rm=T),
    RBU_AVAIL_DL_V_MEAN = mean(RBU_AVAIL_DL, na.rm=T),
    
    RBU_AVAIL_UL_V_MEAN = mean(RBU_AVAIL_UL, na.rm=T),
    RBU_OTHER_DL_V_MEAN = mean(RBU_OTHER_DL, na.rm=T),
    RBU_OTHER_UL_V_MEAN = mean(RBU_OTHER_UL, na.rm=T),
    RBU_OWN_DL_V_MEAN = mean(RBU_OWN_DL, na.rm=T),
    RBU_OWN_UL_V_MEAN = mean(RBU_OWN_UL, na.rm=T),
    RRC_BLOCKING_RATE_3G_V_MEAN = mean(RRC_BLOCKING_RATE_3G, na.rm=T),
    RRC_BLOCKING_RATE_LTE_V_MEAN = mean(RRC_BLOCKING_RATE_LTE, na.rm=T),
    SHO_FACTOR_V_MEAN = mean(SHO_FACTOR, na.rm=T),
    TBF_DROP_RATE_2G_V_MEAN = mean(TBF_DROP_RATE_2G, na.rm=T),
    TCH_DROP_RATE_2G_V_MEAN = mean(TCH_DROP_RATE_2G, na.rm=T),
    UTIL_CE_DL_3G_V_MEAN = mean(UTIL_CE_DL_3G, na.rm=T),
    UL_VOLUME_LTE_V_MEAN = mean(UL_VOLUME_LTE, na.rm=T),
    DL_VOLUME_LTE_V_MEAN = mean(DL_VOLUME_LTE, na.rm=T),
    TOTAL_DL_VOLUME_3G_V_MEAN = mean(TOTAL_DL_VOLUME_3G, na.rm=T),
    TOTAL_UL_VOLUME_3G_V_MEAN = mean(TOTAL_UL_VOLUME_3G, na.rm=T),
    
    count_v = n()
  )
  
  vvv = chnn1_v %>% group_by(SK_ID) %>% summarise(
    AVEUSERNUMBER_V_MEAN = mean(AVEUSERNUMBER, na.rm=T),
    AVEUSERNUMBER_PLMN_V_MEAN = mean(AVEUSERNUMBER_PLMN, na.rm=T),
    AVR_DL_HSPA_USER_3G_V_MEAN = mean(AVR_DL_HSPA_USER_3G, na.rm=T),
    AVR_DL_R99_USER_3G_V_MEAN = mean(AVR_DL_R99_USER_3G, na.rm=T),
    AVR_DL_USER_3G_V_MEAN = mean(AVR_DL_USER_3G, na.rm=T),
    AVR_DL_USER_LTE_V_MEAN = mean(AVR_DL_USER_LTE, na.rm=T),
    AVR_TX_POWER_3G_V_MEAN = mean(AVR_TX_POWER_3G, na.rm=T),
    AVR_UL_HSPA_USER_V_MEAN = mean(AVR_UL_HSPA_USER, na.rm=T),
    AVR_UL_R99_USER_V_MEAN = mean(AVR_UL_R99_USER, na.rm=T),
    AVR_UL_USER_3G_V_MEAN = mean(AVR_UL_USER_3G, na.rm=T),
    
    AVR_UL_USER_LTE_V_MEAN = mean(AVR_UL_USER_LTE, na.rm=T),
    DL_AVR_THROUGHPUT_3G_V_MEAN = mean(DL_AVR_THROUGHPUT_3G, na.rm=T),
    DL_AVR_THROUGHPUT_LTE_V_MEAN = mean(DL_AVR_THROUGHPUT_LTE, na.rm=T),
    DL_AVR_THROUGHPUT_R99_V_MEAN = mean(DL_AVR_THROUGHPUT_R99, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_LTE_V_MEAN = mean(DL_MEAN_USER_THROUGHPUT_LTE, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_DL_2G_V_MEAN = mean(DL_MEAN_USER_THROUGHPUT_DL_2G, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_HSPA3G_V_MEAN = mean(DL_MEAN_USER_THROUGHPUT_HSPA3G, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_PLTE_V_MEAN = mean(DL_MEAN_USER_THROUGHPUT_PLTE, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_REL93G_V_MEAN = mean(DL_MEAN_USER_THROUGHPUT_REL93G, na.rm=T),
    RBU_USED_DL_V_MEAN = mean(RBU_USED_DL, na.rm=T),
    
    RBU_USED_UL_V_MEAN = mean(RBU_USED_UL, na.rm=T),
    RELATIVE_RBU_USED_DL_V_MEAN = mean(RELATIVE_RBU_USED_DL, na.rm=T),
    RELATIVE_RBU_USED_UL_V_MEAN = mean(RELATIVE_RBU_USED_UL, na.rm=T),
    RELATIVE_TX_POWER_3G_V_MEAN = mean(RELATIVE_TX_POWER_3G, na.rm=T),
    UL_AVR_THROUGHPUT_3G_V_MEAN = mean(UL_AVR_THROUGHPUT_3G, na.rm=T),
    UL_AVR_THROUGHPUT_LTE_V_MEAN = mean(UL_AVR_THROUGHPUT_LTE, na.rm=T),
    UL_AVR_THROUGHPUT_R99_V_MEAN = mean(UL_AVR_THROUGHPUT_R99, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_LTE_V_MEAN = mean(UL_MEAN_USER_THROUGHPUT_LTE, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_HS3G_V_MEAN = mean(UL_MEAN_USER_THROUGHPUT_HS3G, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_PLTE_V_MEAN = mean(UL_MEAN_USER_THROUGHPUT_PLTE, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_REL93G_V_MEAN = mean(UL_MEAN_USER_THROUGHPUT_REL93G, na.rm=T)
  )
  
  cc = cons1 %>% group_by(SK_ID) %>% summarise(
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
  XL1 = left_join(XL1, vvv, 'SK_ID')
  XL1 = left_join(XL1, ddd, 'SK_ID')
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

XL = create_features(subs_features_train, subs_csi_train, avg1_d_train, avg1_v_train, chnn1_d_train, chnn1_v_train, cons1_train)


feats = c('C1_FREQ','ACT_MEAN','C7_FREQ','RENT_CHANNEL','C27','count_v','C30S','C29S','RENT_CHANNEL_M','CSSR_3G_V_MEAN',
          'SUM_DATA_MIN_SUM','SUM_DATA_MIN_MEAN','ITC_M','BASE_TYPE_MEAN','CSSR_2G_D_MEAN','DATA_VOL_MB_MEAN',
          'AVR_DL_USER_LTE_D_MEAN','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','C33','C23S','TOTAL_DL_VOLUME_3G_V_MEAN','C18',
          'RBU_OWN_DL_D_MEAN','SHO_FACTOR_V_MEAN','CSSR_3G_D_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','INTERNET_TYPE_ID_1',
          'C3','PART_CQI_QPSK_LTE_D_MEAN','RELATIVE_TX_POWER_3G_V_MEAN','C18S','ROAM_M','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN',
          'UL_VOLUME_LTE_D_MEAN')

feats_lm2 = c('ACT_MEAN','C7_FREQ','C27','C29S','RENT_CHANNEL_M','CSSR_3G_V_MEAN','SUM_DATA_MIN_SUM','SUM_DATA_MIN_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','RELATIVE_TX_POWER_3G_V_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','DL_AVR_THROUGHPUT_3G_D_MEAN','RRC_BLOCKING_RATE_LTE_D_MEAN','PSSR_2G_V_MEAN','C28','UL_VOLUME_LTE_D_MEAN','UL_AVR_THROUGHPUT_3G_D_MEAN','C29','RRC_BLOCKING_RATE_3G_V_MEAN','count_d','RAB_CS_DROP_RATE_3G_D_MEAN','RENT_CHANNEL','VOICE_DUR_MIN_MIN','DL_AVR_THROUGHPUT_LTE_D_MEAN','C17','C20','C17S','C19','C23','C20S','BASE_TYPE_MEAN','ROAM','C3','DL_MEAN_USER_THROUGHPUT_REL93G_V_MEAN','C32','C33','RBU_USED_UL_V_MEAN','ERAB_PS_DROP_RATE_LTE_V_MEAN','SUM_DATA_MB_SUM','AVR_TX_POWER_3G_D_MEAN','UL_AVR_THROUGHPUT_R99_D_MEAN','count','AVR_DL_USER_LTE_V_MEAN','RAB_CS_DROP_RATE_3G_V_MEAN','RBU_AVAIL_UL_D_MEAN','RBU_AVAIL_DL_D_MEAN')


feats_lmr = c('ACT_MEAN','C7_FREQ','RENT_CHANNEL','C27','C29S','SUM_DATA_MIN_SUM','BASE_TYPE_MEAN','C23S','CSSR_3G_D_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','RELATIVE_TX_POWER_3G_V_MEAN','count_d','DL_AVR_THROUGHPUT_LTE_D_MEAN','RRC_BLOCKING_RATE_3G_V_MEAN','COST_S','C29','RRC_BLOCKING_RATE_LTE_D_MEAN','AVR_UL_USER_LTE_V_MEAN','UL_AVR_THROUGHPUT_R99_D_MEAN','C22','RELATIVE_TX_POWER_3G_D_MEAN','UL_VOLUME_LTE_D_MEAN','ERAB_PS_DROP_RATE_LTE_V_MEAN','TOTAL_DL_VOLUME_3G_D_MEAN','RBU_USED_UL_V_MEAN','RBU_AVAIL_DL_D_MEAN','AVR_DL_USER_LTE_V_MEAN','C28','REVENUE','SUM_DATA_MIN_MEAN','VOICE_DUR_MIN_MIN','AVEUSERNUMBER_PLMN_V_MEAN','RRC_BLOCKING_RATE_LTE_V_MEAN','PSSR_LTE_V_MEAN','AVR_DL_HSPA_USER_3G_V_MEAN','AVR_TX_POWER_3G_V_MEAN','RAB_CS_DROP_RATE_3G_D_MEAN','UL_MEAN_USER_THROUGHPUT_LTE_D_MEAN','RAB_CS_DROP_RATE_3G_V_MEAN','SUM_DATA_MB_SUM','C17','C17S','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','ERAB_PS_BLOCKING_RATE_LTE_V_MEAN','UL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','UL_VOLUME_LTE_V_MEAN','RENT_CHANNEL_M','ROAM','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN','ROAM_M','CELL_AVAILABILITY_3G_V_MEAN','RBU_USED_UL_D_MEAN')

feats_lmr = c('ACT_MEAN','C7_FREQ','RENT_CHANNEL','C27','C29S','SUM_DATA_MIN_SUM','BASE_TYPE_MEAN','C23S','CSSR_3G_D_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','RELATIVE_TX_POWER_3G_V_MEAN','count_d','DL_AVR_THROUGHPUT_LTE_D_MEAN','RRC_BLOCKING_RATE_3G_V_MEAN','COST_S','C29','RRC_BLOCKING_RATE_LTE_D_MEAN','AVR_UL_USER_LTE_V_MEAN','UL_AVR_THROUGHPUT_R99_D_MEAN','C22','RELATIVE_TX_POWER_3G_D_MEAN','UL_VOLUME_LTE_D_MEAN','ERAB_PS_DROP_RATE_LTE_V_MEAN','TOTAL_DL_VOLUME_3G_D_MEAN','RBU_USED_UL_V_MEAN','AVR_DL_USER_LTE_V_MEAN','C28','REVENUE','SUM_DATA_MIN_MEAN','VOICE_DUR_MIN_MIN','AVEUSERNUMBER_PLMN_V_MEAN','RRC_BLOCKING_RATE_LTE_V_MEAN','PSSR_LTE_V_MEAN','AVR_DL_HSPA_USER_3G_V_MEAN','AVR_TX_POWER_3G_V_MEAN','RAB_CS_DROP_RATE_3G_D_MEAN','UL_MEAN_USER_THROUGHPUT_LTE_D_MEAN','RAB_CS_DROP_RATE_3G_V_MEAN','SUM_DATA_MB_SUM','C17','C17S','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','ERAB_PS_BLOCKING_RATE_LTE_V_MEAN','UL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','UL_VOLUME_LTE_V_MEAN','RENT_CHANNEL_M','ROAM','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN','ROAM_M','RBU_USED_UL_D_MEAN','C1_8','TCH_DROP_RATE_2G_V_MEAN')

feats_lm = c( 'C27','C29S','RENT_CHANNEL_M','AVR_DL_USER_3G_D_MEAN','C33','VAS_M','AVR_UL_USER_LTE_V_MEAN',
              'RAB_PS_BLOCKING_RATE_3G_D_MEAN','AVEUSERNUMBER_V_MEAN','C7_FREQ','VOICE_DUR_MIN_MIN','ACT_MEAN',
              'AVR_TX_POWER_3G_D_MEAN','RBU_AVAIL_DL_D_MEAN','C2_FREQ','DL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','BASE_TYPE_MEAN',
              'C28','AVR_UL_USER_3G_D_MEAN','RBU_USED_UL_V_MEAN','SUM_DATA_MIN_SUM','RAB_CS_DROP_RATE_3G_D_MEAN',
              'RAB_CS_DROP_RATE_3G_V_MEAN','ERAB_PS_DROP_RATE_LTE_V_MEAN','count_d','CELL_AVAILABILITY_3G_V_MEAN','C32',
              'RRC_BLOCKING_RATE_3G_V_MEAN','C17','C17S','C23S','C29','UL_AVR_THROUGHPUT_R99_D_MEAN','RELATIVE_TX_POWER_3G_V_MEAN',
              'RELATIVE_TX_POWER_3G_D_MEAN','REVENUE','C22','count','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN',
              'ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','SUM_DATA_MIN_MEAN','ROAM_M','SUM_DATA_MB_SUM', 'C1_7','C1_8','C3_7')

feats_lm = c('C27','C29S','RENT_CHANNEL_M','AVR_DL_USER_3G_D_MEAN','C33','VAS_M','AVR_UL_USER_LTE_V_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','AVEUSERNUMBER_V_MEAN','C7_FREQ','VOICE_DUR_MIN_MIN','ACT_MEAN','AVR_TX_POWER_3G_D_MEAN','RBU_AVAIL_DL_D_MEAN','DL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','BASE_TYPE_MEAN','C28','AVR_UL_USER_3G_D_MEAN','RBU_USED_UL_V_MEAN','SUM_DATA_MIN_SUM','RAB_CS_DROP_RATE_3G_D_MEAN','RAB_CS_DROP_RATE_3G_V_MEAN','ERAB_PS_DROP_RATE_LTE_V_MEAN','count_d','CELL_AVAILABILITY_3G_V_MEAN','C32','RRC_BLOCKING_RATE_3G_V_MEAN','C17','C17S','C23S','C29','UL_AVR_THROUGHPUT_R99_D_MEAN','RELATIVE_TX_POWER_3G_V_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','REVENUE','C22','count','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','SUM_DATA_MIN_MEAN','ROAM_M','SUM_DATA_MB_SUM',
             'C1_7','C1_8','C3_7','C1_1')
feats_lm = c('C27','C29S','RENT_CHANNEL_M','AVR_DL_USER_3G_D_MEAN','C33','VAS_M','AVR_UL_USER_LTE_V_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','C7_FREQ','VOICE_DUR_MIN_MIN','ACT_MEAN','AVR_TX_POWER_3G_D_MEAN','RBU_AVAIL_DL_D_MEAN','DL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','BASE_TYPE_MEAN','C28','AVR_UL_USER_3G_D_MEAN','RBU_USED_UL_V_MEAN','SUM_DATA_MIN_SUM','RAB_CS_DROP_RATE_3G_D_MEAN','ERAB_PS_DROP_RATE_LTE_V_MEAN','count_d','CELL_AVAILABILITY_3G_V_MEAN','C32','RRC_BLOCKING_RATE_3G_V_MEAN','C17','C17S','C23S','C29','UL_AVR_THROUGHPUT_R99_D_MEAN','RELATIVE_TX_POWER_3G_V_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','REVENUE','C22','count','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','SUM_DATA_MIN_MEAN','ROAM_M','SUM_DATA_MB_SUM','C1_8','C3_7','C1_1')


feats = c('ACT_MEAN','C30S','DATA_VOL_MB_MEAN','C23S','count_v','CSSR_3G_D_MEAN','RENT_CHANNEL','SUM_DATA_MIN_SUM','CELL_AVAILABILITY_4G_D_MEAN','C29S','CSSR_3G_V_MEAN','UL_VOLUME_LTE_D_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','C33','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','RBU_OWN_DL_D_MEAN','UL_AVR_THROUGHPUT_R99_V_MEAN','SHO_FACTOR_V_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','INTERNET_TYPE_ID_NOT_1','AVR_DL_USER_LTE_D_MEAN','AVR_UL_USER_LTE_D_MEAN','PART_CQI_QPSK_LTE_D_MEAN','AVR_DL_R99_USER_3G_D_MEAN','TOTAL_DL_VOLUME_3G_V_MEAN','C1_FREQ','SUM_DATA_MIN_MEAN','C27','CSSR_2G_D_MEAN','RENT_CHANNEL_M','C7_FREQ')

feats = c('ACT_MEAN','C30S','DATA_VOL_MB_MEAN','C23S','count_v','CSSR_3G_D_MEAN','RENT_CHANNEL','SUM_DATA_MIN_SUM','C29S','CSSR_3G_V_MEAN',
          'UL_VOLUME_LTE_D_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','C33','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','RBU_OWN_DL_D_MEAN',
          'UL_AVR_THROUGHPUT_R99_V_MEAN','SHO_FACTOR_V_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','INTERNET_TYPE_ID_NOT_1','AVR_DL_USER_LTE_D_MEAN',
          'AVR_UL_USER_LTE_D_MEAN','PART_CQI_QPSK_LTE_D_MEAN','AVR_DL_R99_USER_3G_D_MEAN','TOTAL_DL_VOLUME_3G_V_MEAN','C1_FREQ','SUM_DATA_MIN_MEAN',
          'C27','CSSR_2G_D_MEAN','RENT_CHANNEL_M','C7_FREQ','RBU_AVAIL_DL_D_MEAN','CELL_AVAILABILITY_2G_D_MEAN','C3_5')

###
feats_lm = c('C27','C29S','RENT_CHANNEL_M','AVR_TX_POWER_3G_D_MEAN','C28','RBU_USED_UL_V_MEAN','SUM_DATA_MIN_SUM','RAB_CS_DROP_RATE_3G_D_MEAN','CELL_AVAILABILITY_3G_V_MEAN','RRC_BLOCKING_RATE_3G_V_MEAN','C17S','C29','RELATIVE_TX_POWER_3G_V_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','C22','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','SUM_DATA_MIN_MEAN','ACT_CZ','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','C21','AVR_DL_R99_USER_3G_D_MEAN','C23','DATA_VOL_MB_SUM','ROAM_M','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN','CSSR_3G_V_MEAN','DL_AVR_THROUGHPUT_3G_V_MEAN','RBU_OWN_UL_D_MEAN','UTIL_CE_DL_3G_V_MEAN','C32','UTIL_CE_DL_3G_D_MEAN','C17','C33','DL_MEAN_USER_THROUGHPUT_HSPA3G_V_MEAN','PSSR_2G_V_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','INTERNET_TYPE_ID_1','RBU_AVAIL_DL_D_MEAN','RBU_USED_DL_D_MEAN','C3_7','UL_MEAN_USER_THROUGHPUT_REL93G_D_MEAN','UL_MEAN_USER_THROUGHPUT_REL93G_V_MEAN','C21S','VAS_M','C22S','C3_3', 'SUM_DATA_MIN_mult_CELL_AVAILABILITY_2G_MEAN','SUM_MINUTES_mult_CSSR_3G_MEAN')
feats_lm = c('C27','C29S','RENT_CHANNEL_M','AVR_TX_POWER_3G_D_MEAN','C28','RBU_USED_UL_V_MEAN','SUM_DATA_MIN_SUM','RAB_CS_DROP_RATE_3G_D_MEAN','CELL_AVAILABILITY_3G_V_MEAN','RRC_BLOCKING_RATE_3G_V_MEAN','C17S','C29','RELATIVE_TX_POWER_3G_V_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','C22','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','SUM_DATA_MIN_MEAN','ACT_CZ','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','C21','AVR_DL_R99_USER_3G_D_MEAN','C23','DATA_VOL_MB_SUM','ROAM_M','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN','CSSR_3G_V_MEAN','DL_AVR_THROUGHPUT_3G_V_MEAN','RBU_OWN_UL_D_MEAN','UTIL_CE_DL_3G_V_MEAN','C32','UTIL_CE_DL_3G_D_MEAN','C17','C33','DL_MEAN_USER_THROUGHPUT_HSPA3G_V_MEAN','PSSR_2G_V_MEAN','RBU_AVAIL_DL_D_MEAN','RBU_USED_DL_D_MEAN','C3_7','UL_MEAN_USER_THROUGHPUT_REL93G_D_MEAN','UL_MEAN_USER_THROUGHPUT_REL93G_V_MEAN','C21S','VAS_M','C22S','C3_3','SUM_DATA_MIN_mult_CELL_AVAILABILITY_2G_MEAN','SUM_MINUTES_mult_CSSR_3G_MEAN','AVR_DL_R99_USER_3G_V_MEAN','C31','INTERNET_TYPE_ID_2','C7_FREQ','ACT_M3')

feats = c('ACT_MEAN','C30S','DATA_VOL_MB_MEAN','count_v','CSSR_3G_D_MEAN','C29S','UL_VOLUME_LTE_D_MEAN','C33','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','INTERNET_TYPE_ID_NOT_1','AVR_DL_USER_LTE_D_MEAN','PART_CQI_QPSK_LTE_D_MEAN','AVR_DL_R99_USER_3G_D_MEAN','TOTAL_DL_VOLUME_3G_V_MEAN','C27','RENT_CHANNEL_M','C7_FREQ','CELL_AVAILABILITY_2G_D_MEAN','RBU_OWN_UL_D_MEAN','CELL_AVAILABILITY_4G_V_MEAN','AVEUSERNUMBER_D_MEAN','C1_1','C32','DL_VOLUME_LTE_D_MEAN','RAB_CS_DROP_RATE_3G_D_MEAN','UL_AVR_THROUGHPUT_R99_D_MEAN','UL_MEAN_USER_THROUGHPUT_HS3G_V_MEAN','RELATIVE_RBU_USED_DL_D_MEAN','UTIL_CE_DL_3G_V_MEAN','PSSR_LTE_D_MEAN','DL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','C1_4','C1_8','RBU_AVAIL_UL_D_MEAN','C1_2','DATA_VOL_MB_MIN','C1_FREQ','DL_MEAN_USER_THROUGHPUT_REL93G_V_MEAN','SUM_DATA_MB_PER_MIN_MEAN','BASE_TYPE_FREQ','C1_AVG_Y','UTIL_CE_DL_3G_D_MEAN','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','ROAM','INTERNET_TYPE_ID_1','C28')
feats = c('ACT_MEAN','C30S','DATA_VOL_MB_MEAN','count_v','CSSR_3G_D_MEAN','C29S','UL_VOLUME_LTE_D_MEAN','C33','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','INTERNET_TYPE_ID_NOT_1','AVR_DL_USER_LTE_D_MEAN','PART_CQI_QPSK_LTE_D_MEAN','AVR_DL_R99_USER_3G_D_MEAN','TOTAL_DL_VOLUME_3G_V_MEAN','C27','RENT_CHANNEL_M','C7_FREQ','CELL_AVAILABILITY_2G_D_MEAN','RBU_OWN_UL_D_MEAN','CELL_AVAILABILITY_4G_V_MEAN','AVEUSERNUMBER_D_MEAN','C1_1','C32','DL_VOLUME_LTE_D_MEAN','RAB_CS_DROP_RATE_3G_D_MEAN','UL_AVR_THROUGHPUT_R99_D_MEAN','UL_MEAN_USER_THROUGHPUT_HS3G_V_MEAN','RELATIVE_RBU_USED_DL_D_MEAN','UTIL_CE_DL_3G_V_MEAN','PSSR_LTE_D_MEAN','DL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','C1_4','C1_8','RBU_AVAIL_UL_D_MEAN','C1_2','DATA_VOL_MB_MIN','C1_FREQ','DL_MEAN_USER_THROUGHPUT_REL93G_V_MEAN','SUM_DATA_MB_PER_MIN_MEAN','BASE_TYPE_FREQ','C1_AVG_Y','UTIL_CE_DL_3G_D_MEAN','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','ROAM','INTERNET_TYPE_ID_1','C28','SUM_MINUTES_mult_CELL_AVAILABILITY_4G_MEAN','SUM_MINUTES_mult_CSSR_2G_MEAN')
feats = c('ACT_MEAN','C30S','DATA_VOL_MB_MEAN','count_v','CSSR_3G_D_MEAN','UL_VOLUME_LTE_D_MEAN','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','RELATIVE_TX_POWER_3G_D_MEAN','AVR_DL_USER_LTE_D_MEAN','AVR_DL_R99_USER_3G_D_MEAN','TOTAL_DL_VOLUME_3G_V_MEAN','C27','C7_FREQ','CELL_AVAILABILITY_4G_V_MEAN','AVEUSERNUMBER_D_MEAN','C1_1','C32','DL_VOLUME_LTE_D_MEAN','RAB_CS_DROP_RATE_3G_D_MEAN','UL_AVR_THROUGHPUT_R99_D_MEAN','UL_MEAN_USER_THROUGHPUT_HS3G_V_MEAN','RELATIVE_RBU_USED_DL_D_MEAN','DL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','C1_8','RBU_AVAIL_UL_D_MEAN','C1_2','DATA_VOL_MB_MIN','C1_FREQ','DL_MEAN_USER_THROUGHPUT_REL93G_V_MEAN','SUM_DATA_MB_PER_MIN_MEAN','BASE_TYPE_FREQ','C1_AVG_Y','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','INTERNET_TYPE_ID_1','SUM_MINUTES_mult_CSSR_2G_MEAN','SUM_DATA_MIN_mult_CELL_AVAILABILITY_4G_MEAN','SUM_MINUTES_mult_CSSR_3G_MEAN','count_c','RBU_OWN_DL_D_MEAN','BASE_TYPE_MEAN','C26','C3','C19','C7_3456','C3_8','ITC_M','DATA_VOL_MB_MAX','C32S','AVR_TX_POWER_3G_D_MEAN','RAB_PS_BLOCKING_RATE_3G_V_MEAN','C1_5','C33S','C1_6','C3_1','CELL_AVAILABILITY_3G_V_MEAN','CONTACT_DATE','DL_MEAN_USER_THROUGHPUT_HSPA3G_V_MEAN','C17S','RRC_BLOCKING_RATE_3G_V_MEAN')

feats_lmr = c('C7_FREQ','RENT_CHANNEL','C27','C29S','SUM_DATA_MIN_SUM','C23S','CSSR_3G_D_MEAN','RELATIVE_TX_POWER_3G_V_MEAN','count_d','RRC_BLOCKING_RATE_3G_V_MEAN','COST_S','C29','AVR_UL_USER_LTE_V_MEAN','UL_AVR_THROUGHPUT_R99_D_MEAN','C22','RELATIVE_TX_POWER_3G_D_MEAN','UL_VOLUME_LTE_D_MEAN','RBU_USED_UL_V_MEAN','C28','SUM_DATA_MIN_MEAN','RRC_BLOCKING_RATE_LTE_V_MEAN','PSSR_LTE_V_MEAN','RAB_CS_DROP_RATE_3G_D_MEAN','UL_MEAN_USER_THROUGHPUT_LTE_D_MEAN','SUM_DATA_MB_SUM','C17','C17S','UL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','RENT_CHANNEL_M','ROAM','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN','RBU_USED_UL_D_MEAN','ACT_CZ','DL_AVR_THROUGHPUT_3G_V_MEAN','C3_7','CELL_AVAILABILITY_3G_V_MEAN','C19S','C32','AVR_UL_R99_USER_D_MEAN','PSSR_2G_V_MEAN','C33S','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','DL_MEAN_USER_THROUGHPUT_HSPA3G_V_MEAN','AVEUSERNUMBER_PLMN_V_MEAN','INTERNET_TYPE_ID_2','DL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','DL_MEAN_USER_THROUGHPUT_LTE_D_MEAN','C2_FREQ','AVR_TX_POWER_3G_D_MEAN')
feats_lmr = c('C7_FREQ','RENT_CHANNEL','C27','C29S','SUM_DATA_MIN_SUM','C23S','CSSR_3G_D_MEAN','RELATIVE_TX_POWER_3G_V_MEAN','RRC_BLOCKING_RATE_3G_V_MEAN','COST_S','C29','AVR_UL_USER_LTE_V_MEAN','C22','RELATIVE_TX_POWER_3G_D_MEAN','RBU_USED_UL_V_MEAN','C28','SUM_DATA_MIN_MEAN','RRC_BLOCKING_RATE_LTE_V_MEAN','PSSR_LTE_V_MEAN','RAB_CS_DROP_RATE_3G_D_MEAN','SUM_DATA_MB_SUM','C17','C17S','RENT_CHANNEL_M','ROAM','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN','RBU_USED_UL_D_MEAN','ACT_CZ','DL_AVR_THROUGHPUT_3G_V_MEAN','C3_7','CELL_AVAILABILITY_3G_V_MEAN','C32','PSSR_2G_V_MEAN','C33S','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','AVEUSERNUMBER_PLMN_V_MEAN','INTERNET_TYPE_ID_2','DL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','DL_MEAN_USER_THROUGHPUT_LTE_D_MEAN','C2_FREQ','AVR_TX_POWER_3G_D_MEAN','SUM_DATA_MIN_mult_CELL_AVAILABILITY_2G_MEAN','SUM_MINUTES_mult_CELL_AVAILABILITY_2G_MEAN','SUM_MINUTES_mult_CELL_AVAILABILITY_3G_MEAN','SUM_MINUTES_mult_CSSR_2G_MEAN','UTIL_CE_DL_3G_D_MEAN','RBU_USED_DL_D_MEAN','RBU_AVAIL_DL_D_MEAN','C31','C1_1','SUM_MINUTES_MEAN','count_d_0','CELL_AVAILABILITY_2G_V_MEAN','AVR_UL_HSPA_USER_D_MEAN','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','AVR_DL_R99_USER_3G_D_MEAN','AVR_DL_R99_USER_3G_V_MEAN','CSSR_2G_V_MEAN','C1_3','COST','UTIL_CE_DL_3G_V_MEAN','C1_5')
feats_lmr = c('C7_FREQ','RENT_CHANNEL','C27','C29S','SUM_DATA_MIN_SUM','C23S','CSSR_3G_D_MEAN','RELATIVE_TX_POWER_3G_V_MEAN','RRC_BLOCKING_RATE_3G_V_MEAN','COST_S','C29','AVR_UL_USER_LTE_V_MEAN','C22','RELATIVE_TX_POWER_3G_D_MEAN','RBU_USED_UL_V_MEAN','C28','SUM_DATA_MIN_MEAN','RRC_BLOCKING_RATE_LTE_V_MEAN','PSSR_LTE_V_MEAN','RAB_CS_DROP_RATE_3G_D_MEAN','SUM_DATA_MB_SUM','C17','C17S','RENT_CHANNEL_M','ROAM','ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN','RBU_USED_UL_D_MEAN','ACT_CZ','DL_AVR_THROUGHPUT_3G_V_MEAN','C3_7','CELL_AVAILABILITY_3G_V_MEAN','C32','PSSR_2G_V_MEAN','C33S','DL_MEAN_USER_THROUGHPUT_HSPA3G_D_MEAN','AVEUSERNUMBER_PLMN_V_MEAN','INTERNET_TYPE_ID_2','DL_MEAN_USER_THROUGHPUT_PLTE_D_MEAN','RAB_PS_BLOCKING_RATE_3G_D_MEAN','DL_MEAN_USER_THROUGHPUT_LTE_D_MEAN','C2_FREQ','AVR_TX_POWER_3G_D_MEAN','SUM_DATA_MIN_mult_CELL_AVAILABILITY_2G_MEAN','SUM_MINUTES_mult_CELL_AVAILABILITY_2G_MEAN','SUM_MINUTES_mult_CELL_AVAILABILITY_3G_MEAN','SUM_MINUTES_mult_CSSR_2G_MEAN','UTIL_CE_DL_3G_D_MEAN','RBU_USED_DL_D_MEAN','RBU_AVAIL_DL_D_MEAN','C31','C1_1','SUM_MINUTES_MEAN','count_d_0','CELL_AVAILABILITY_2G_V_MEAN','AVR_UL_HSPA_USER_D_MEAN','ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','AVR_DL_R99_USER_3G_D_MEAN','AVR_DL_R99_USER_3G_V_MEAN','C1_3','COST','C1_5')

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
      my.train.lm(XL, params)
    }, aggr=median)
  }, feats_lm)
}
my.gridSearch(XL, algoLgb, expand.grid(lgbParams), verbose=T, folds=1:4, folds.mult=20, resample.seed=2708, algo.seed=442, timefolds=T)
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
  function (X) {
    p=0.45
    q=0.55
    r = function (x) order(order(x)) / length(x)
    r(m1(X))*p + r(m2(X))*q + r(m3(X))*(1-p-q)
  }
}
validation.tqfold.parallel(XL, algoBlend, folds=1:4, timefolds=T, folds.mult=20, resample.seed=2708, algo.seed=442)
stopCluster(cl)

stopCluster(cl)
cl = makeCluster(4)
registerDoParallel(cl)
addRemoveSelect(3000, XL, function (XL, params, features) {
  my.extendedColsTrain(XL, function (XL) {
    my.fillNasTrain(XL, function (XL) {
      my.train.lgb(XL, lgbParams)
    }, aggr=median)
  }, features)
}, startFeatures=unique(c(feats,'SUM_DATA_MB_mult_CELL_AVAILABILITY_2G_MEAN',
                   'SUM_DATA_MB_mult_CELL_AVAILABILITY_3G_MEAN',
                   'SUM_DATA_MB_mult_CELL_AVAILABILITY_4G_MEAN',
                   'SUM_DATA_MIN_mult_CELL_AVAILABILITY_2G_MEAN',
                   'SUM_DATA_MIN_mult_CELL_AVAILABILITY_3G_MEAN',
                   'SUM_DATA_MIN_mult_CELL_AVAILABILITY_4G_MEAN',
                   'SUM_MINUTES_mult_CELL_AVAILABILITY_2G_MEAN',
                   'SUM_MINUTES_mult_CELL_AVAILABILITY_3G_MEAN',
                   'SUM_MINUTES_mult_CELL_AVAILABILITY_4G_MEAN',
                   'SUM_MINUTES_mult_CSSR_2G_MEAN',
                   'SUM_MINUTES_mult_CSSR_3G_MEAN')))
stopCluster(cl)

# TODO: COST * 

stopCluster(cl)
cl = makeCluster(4)
registerDoParallel(cl)
addRemoveSelect(3000, XL, function (XL, params, features) {
  my.extendedColsTrain(XL, function (XL) {
    my.fillNasTrain(XL, function (XL) {
      my.train.lm(XL, lmrParams)
    }, aggr=median)
  }, features)
}, startFeatures=feats_lm)
stopCluster(cl)

normalize_test = function (XL, XX, cnames=NULL) {
  XL = as.data.frame(XL)
  XX = as.data.frame(XX)
  for (cname in colnames(XX)) {
    if (all(round(XL[!is.na(XL[, cname]), cname]) == XL[!is.na(XL[, cname]), cname])) {
      #cat(paste0('Skip ', cname, '\n'))
      next
    }
    if (min(XL[, cname], na.rm=T) == min(XX[, cname], na.rm=T) && max(XL[, cname], na.rm=T) == max(XX[, cname], na.rm=T)) {
      #cat(paste0('Skip2 ', cname, '\n'))
      next
    }
    train_mean = mean(XL[, cname], na.rm=T)
    train_sd = sd(XL[, cname], na.rm=T)
    test_mean = mean(XX[, cname], na.rm=T)
    test_sd = sd(XX[, cname], na.rm=T)
    train_min = min(XL[, cname], na.rm=T)
    train_max = max(XL[, cname], na.rm=T)
    test_min = min(XX[, cname], na.rm=T)
    test_max = max(XX[, cname], na.rm=T)
    if (is.null(cnames) || cname %in% cnames) {
      if (F) {
        XX[!is.na(XX[, cname]), cname] = (XX[!is.na(XX[, cname]), cname] - test_mean) / test_sd * train_sd + train_mean
      } else {
        XX[!is.na(XX[, cname]), cname] = (XX[!is.na(XX[, cname]), cname] - test_min) / (test_max - test_min) * (train_max - train_min) + train_min
      }
    }
  }
  XX
}

XL_1 = XL[XL$SK_ID %in% subs_csi_train_old$SK_ID, ]
XX_1 = XL[XL$SK_ID %in% subs_csi_test_old$SK_ID, ]
mm = algoBlend(XL_1)


XX = create_features(subs_features_test, subs_csi_test, avg1_d_test, avg1_v_test, chnn1_d_test, chnn1_v_test, cons1_test)
#model = algoBlend(XL)
model = algoBlend(XL, lgbParams)
#XX$Y = model(normalize_test(XL, XX))
XX$Y = model(XX)
#XX[XX$C2_FREQ==64, ]$Y = 0
resulted_table = left_join(subs_csi_test, XX, "SK_ID")
YY = resulted_table$Y
YY[is.na(YY)] = mean(YY[!is.na(YY)])
write(YY, file="res/result.txt", sep='\n')

#set.seed(888)
#validation.tqfold(XL, algo1, folds=7, iters=4, verbose=T, seed=2707); asdasd()