#==R Cafe (A club supported by OSU Center for Health Systems Innovation)
#==Author: Akash Gupta
library(dplyr); library(ggmap); library(stringr); library(maps)
#=== Read the data file
medOrganization = read.csv("/media/akash/0C749DFF749DEBA8/Users/Akash/Documents/Data/Medicare_DoctorPayment/Medicare_MedOrganization_MeanPayments.csv")
#===Calculate the number of medical organization in each state
stateMedOrg = medOrganization %>% select(Med_Organization_Name, Med_Organization_State)
stateMedOrg = stateMedOrg[!duplicated(stateMedOrg), ]
statesMedOrg = as.data.frame(table(stateMedOrg$Med_Organization_State))
names(statesMedOrg) = c("State", "NumMedOrg")
statesMedOrg$NumMedOrg = as.numeric(statesMedOrg$NumMedOrg)


#===Extracting the longitude and latitude of US states bounderies
statesUSA = map_data("state") # Function of ggplot2; it uses map packages such as "maps", "mapdata"
statesUSA$StateAbb = str_to_title(statesUSA$region) # To transform the state names into abbreviation
statesUSA$StateAbb = state.abb[charmatch(statesUSA$StateAbb,  state.name)]
statesUSA$StateAbb = as.factor(statesUSA$StateAbb)
statesUSA = statesUSA %>% filter(region != "district of columbia")
statesUSA = merge(x = statesUSA, y = statesMedOrg, by.x = "StateAbb", by.y = "State")

#===Plot map
base1 = ggplot(data = statesUSA) + geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = NumMedOrg), color = "white") +theme_bw() + theme(axis.text = element_blank(),axis.line = element_blank(),axis.ticks = element_blank(),panel.border = element_blank(), panel.grid = element_blank(), axis.title = element_blank())

#===Providers vs. Payment Claimed

providerPayment = medOrganization %>% select(Med_Organization_Identifier, TotalPayment) %>% group_by(NPI) %>% summarise(TotalProviderPayment = sum(TotalPayment)) %>% arrange(TotalProviderPayment)

providerPayment$OrgQuantile = cut(providerPayment$TotalProviderPayment, breaks = quantile(providerPayment$TotalProviderPayment, probs = seq(0, 1, 0.05)), include.lowest = T, ordered_result = T)

temp = providerPayment %>% group_by(OrgQuantile) %>% summarise(Payment = sum(TotalProviderPayment)) %>% mutate(CumAmtPayment = cumsum(Payment))%>% mutate(PercentAmtClaim = CumAmtPayment*100/sum(Payment))
levels(temp$OrgQuantile) = seq(5, 100, 5)

ggplot(data = temp) + geom_path(mapping = aes(x = as.numeric(as.character(OrgQuantile)), y = PercentAmtClaim)) + xlab("Healthcare Provider Ranking (Percentile)") + ylab("Percentage of Total Amount Payment")
rm(temp)

#=== Medical organization payment
medOrgPayment = medOrganization %>% mutate(TotalPayment = Med_Service_Cnt*Med_Organization_Payment_Received)
medOrgPayment = medOrgPayment %>% select(Med_Organization_Identifier, Med_Organization_Name, Med_Organization_Zip, TotalPayment) %>% group_by(Med_Organization_Name, Med_Organization_Zip) %>% dplyr::summarise(TotalProviderPayment = sum(TotalPayment)) %>% arrange(TotalProviderPayment)

medOrgPayment$OrgQuantile = cut(medOrgPayment$TotalProviderPayment, breaks = quantile(medOrgPayment$TotalProviderPayment, probs = seq(0, 1, 0.05)), include.lowest = T, ordered_result = T)

temp = medOrgPayment %>% group_by(OrgQuantile) %>% summarise(Payment = sum(TotalProviderPayment)) %>% mutate(CumAmtPayment = cumsum(Payment))%>% mutate(PercentAmtClaim = CumAmtPayment*100/sum(Payment))
levels(temp$OrgQuantile) = seq(5, 100, 5)

ggplot(data = temp) + geom_path(mapping = aes(x = as.numeric(as.character(OrgQuantile)), y = PercentAmtClaim), color = "red") + xlab("Healthcare Provider Ranking (Percentile)") + ylab("Percentage of Total Amount Payment") + theme_bw()
rm(temp, medOrgPayment)
