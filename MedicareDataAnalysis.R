library(tidyverse)
data = read_delim("/media/akash/0C749DFF749DEBA8/Users/Akash/Documents/Data/Medicare_DoctorPayment/Medicare_Provider_Util_Payment_PUF_CY2013.txt", delim = "\t")
varClasses = sapply(data, class)
medicareData = read_delim("/media/akash/0C749DFF749DEBA8/Users/Akash/Documents/Data/Medicare_DoctorPayment/Medicare_Provider_Util_Payment_PUF_CY2013.txt", delim = "\t")
medicareData = medicareData[2:nrow(medicareData), ]
medicareData = mutate(medicareData, NPPES_PROVIDER_CITY = as.factor(NPPES_PROVIDER_CITY),  
                      NPPES_PROVIDER_STATE = as.factor(NPPES_PROVIDER_STATE),
                      NPPES_PROVIDER_COUNTRY = as.factor(NPPES_PROVIDER_COUNTRY),  
                      NPPES_ENTITY_CODE = as.factor(NPPES_ENTITY_CODE),
                      PROVIDER_TYPE = as.factor( PROVIDER_TYPE),  
                      MEDICARE_PARTICIPATION_INDICATOR = as.factor(MEDICARE_PARTICIPATION_INDICATOR), 
                      PLACE_OF_SERVICE = as.factor(PLACE_OF_SERVICE),
                      HCPCS_CODE = as.factor(HCPCS_CODE),
                      BENE_UNIQUE_CNT = as.numeric(BENE_UNIQUE_CNT),
                      BENE_DAY_SRVC_CNT = as.numeric(BENE_DAY_SRVC_CNT),
                      STDEV_MEDICARE_ALLOWED_AMT = as.numeric(STDEV_MEDICARE_ALLOWED_AMT),
                      STDEV_SUBMITTED_CHRG_AMT = as.numeric(STDEV_SUBMITTED_CHRG_AMT),
                      STDEV_MEDICARE_PAYMENT_AMT = as.numeric(STDEV_MEDICARE_PAYMENT_AMT))

careOrganization = medicareData %>% filter(NPPES_ENTITY_CODE == "O")
rm(medicareData)
write.csv(careOrganization,"/media/akash/0C749DFF749DEBA8/Users/Akash/Documents/Data/Medicare_DoctorPayment/MedicareClaimByOrganization.csv", row.names = F)
careOrganization = careOrganization%>%
  mutate(PaymentMin = AVERAGE_MEDICARE_PAYMENT_AMT - 1.96*STDEV_MEDICARE_PAYMENT_AMT,
        PaymentMax = AVERAGE_MEDICARE_PAYMENT_AMT + 1.96*STDEV_MEDICARE_PAYMENT_AMT,
        PaymentMean = AVERAGE_MEDICARE_PAYMENT_AMT)

careOrganization = mutate(careOrganization, TotalPayment = PaymentMean*LINE_SRVC_CNT)

#===Providers vs. Payment Claimed
providerPayment = careOrganization %>% select(NPI, TotalPayment) %>% group_by(NPI) %>% summarise(TotalProviderPayment = sum(TotalPayment)) %>% arrange(TotalProviderPayment)

providerPayment$OrgQuantile = cut(providerPayment$TotalProviderPayment, breaks = quantile(providerPayment$TotalProviderPayment, probs = seq(0, 1, 0.05)), include.lowest = T, ordered_result = T)

temp = providerPayment %>% group_by(OrgQuantile) %>% summarise(Payment = sum(TotalProviderPayment)) %>% mutate(CumAmtPayment = cumsum(Payment))%>% mutate(PercentAmtClaim = CumAmtPayment*100/sum(Payment))
levels(temp$OrgQuantile) = seq(5, 100, 5)

ggplot(data = temp) + geom_path(mapping = aes(x = as.numeric(as.character(OrgQuantile)), y = PercentAmtClaim)) + xlab("Healthcare Provider Ranking (Percentile)") + ylab("Percentage of Total Amount Payment")

#===Mapping of top 5% organization



#--------------------------------------