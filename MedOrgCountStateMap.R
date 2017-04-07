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

