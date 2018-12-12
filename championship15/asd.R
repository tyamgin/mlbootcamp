a = left_join(subs_features_train, subs_bs_voice_session_train, 'SK_ID') %>% select(SK_ID, INTERNET_TYPE_ID, CELL_LAC_ID)


b = bd %>% group_by(CELL_LAC_ID) %>% summarise(
  i2G=any(!is.na(c(CSSR_2G, PSSR_2G, TBF_DROP_RATE_2G, TCH_DROP_RATE_2G, DL_MEAN_USER_THROUGHPUT_DL_2G))),
  i3G=any(!is.na(c(CSSR_3G, PSSR_3G, RAB_CS_BLOCKING_RATE_3G, RAB_CS_DROP_RATE_3G, RAB_PS_BLOCKING_RATE_3G, RAB_PS_DROP_RATE_3G,
                   RRC_BLOCKING_RATE_3G, SHO_FACTOR, UTIL_CE_DL_3G, TOTAL_DL_VOLUME_3G, TOTAL_UL_VOLUME_3G, AVR_DL_HSPA_USER_3G,
                   AVR_DL_R99_USER_3G, AVR_DL_USER_3G, AVR_TX_POWER_3G, AVR_UL_USER_3G, DL_AVR_THROUGHPUT_3G, DL_AVR_THROUGHPUT_R99,
                   DL_MEAN_USER_THROUGHPUT_HSPA3G, DL_MEAN_USER_THROUGHPUT_REL93G, RELATIVE_TX_POWER_3G, UL_AVR_THROUGHPUT_3G,
                   UL_AVR_THROUGHPUT_R99, UL_MEAN_USER_THROUGHPUT_HS3G, UL_MEAN_USER_THROUGHPUT_REL93G))),
  i4G=any(!is.na(c(ERAB_PS_BLOCKING_RATE_LTE, ERAB_PS_DROP_RATE_LTE, ERAB_PS_BLOCKING_RATE_PLMN_LTE, PSSR_LTE,
                   UL_MEAN_USER_THROUGHPUT_PLTE, UL_MEAN_USER_THROUGHPUT_LTE, UL_AVR_THROUGHPUT_LTE, DL_MEAN_USER_THROUGHPUT_PLTE,
                   DL_MEAN_USER_THROUGHPUT_LTE, DL_AVR_THROUGHPUT_LTE, AVR_UL_USER_LTE, AVR_DL_USER_LTE, DL_VOLUME_LTE,
                   UL_VOLUME_LTE, RRC_BLOCKING_RATE_LTE, PART_MCS_QPSK_LTE, PART_CQI_QPSK_LTE)))
)

print(sum(b$i2G & b$i3G))
print(sum(b$i2G & b$i4G))
print(sum(b$i3G & b$i4G))
print(sum(!b$i2G & !b$i3G & !b$i4G))

saveRDS(b, 'cache/G_data')

#b = bs_avg_kpi_1_12 %>% group_by(CELL_LAC_ID) %>% 
#  summarise(c2G=any(!is.na(CELL_AVAILABILITY_2G)),c3G=any(!is.na(CELL_AVAILABILITY_3G)),c4G=any(!is.na(CELL_AVAILABILITY_4G)))


ss = left_join(a,b,'CELL_LAC_ID')

ss %>% group_by(INTERNET_TYPE_ID) %>% summarise(a2G=sum(i2G,na.rm=T),a3G=sum(i3G,na.rm=T),a4G=sum(i4G,na.rm=T))

# 

print(XL %>% 
  select(SK_ID, SUM_DATA_MB_SUM, INTERNET_TYPE_2G) %>% 
  filter(INTERNET_TYPE_2G == 1) %>% #head(400) %>%
  left_join(subs_bs_consumption_train %>% select(SK_ID, CELL_LAC_ID, SUM_DATA_MB), 'SK_ID') %>% 
  left_join(bs_avai %>% select(CELL_LAC_ID, i2G, i3G, i4G), 'CELL_LAC_ID') %>%
  mutate(lol=1) %>%
  group_by(lol) %>% summarise(
    i2G=sum(i2G*SUM_DATA_MB,na.rm=T),
    i3G=sum(i3G*SUM_DATA_MB,na.rm=T),
    i4G=sum(i4G*SUM_DATA_MB,na.rm=T)
  )
)