#=== Medical organization vs. total payment
library(tidyverse)
# Reading the data
medOrganization = read_csv("/media/akash/0C749DFF749DEBA8/Users/Akash/Documents/Data/Medicare_DoctorPayment/Medicare_MedOrganization_MeanPayments.csv")
# Evaluating the total medicare payment to each hospital
medOrgPayment = medOrganization %>% mutate(TotalPayment = Med_Service_Cnt*Med_Organization_Payment_Received)
medOrgPayment = medOrgPayment %>% select(Med_Organization_Identifier, Med_Organization_Name, Med_Organization_Zip, TotalPayment) %>% group_by(Med_Organization_Identifier, Med_Organization_Name, Med_Organization_Zip) %>% dplyr::summarise(TotalProviderPayment = sum(TotalPayment)) %>% arrange(desc(TotalProviderPayment))
# Divide medical organization based on total medicare claim payment
medOrgPayment$OrgQuantile = cut(medOrgPayment$TotalProviderPayment, breaks = quantile(medOrgPayment$TotalProviderPayment, probs = seq(0, 1, 0.05)), include.lowest = T, ordered_result = T)

temp = medOrgPayment %>% group_by(OrgQuantile) %>% summarise(Payment = sum(TotalProviderPayment)) %>% mutate(CumAmtPayment = cumsum(Payment))%>% mutate(PercentAmtClaim = CumAmtPayment*100/sum(Payment))
levels(temp$OrgQuantile) = seq(5, 100, 5)

ggplot(data = temp) + geom_path(mapping = aes(x = as.numeric(as.character(OrgQuantile)), y = PercentAmtClaim)) +
  labs(x = "Medicare Organization Ranking (Percentile)", y = "Percentage of Total Amount Payment", title = "Medicare Payment vs. Medical Organization", caption = "Created by R Café") + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Times New Roman"))


rm(temp, medOrgPayment)