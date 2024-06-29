rm(list = ls())
setwd('/Users/hankhan688/Downloads')
#install required packages

library(ggplot2)
library(dplyr) 
library(stringr)

#1.Read the required data
data<-read.csv("StormEvents_details-ftp_v1.0_d1996_c20220425.csv")
#2.Limit the dataframe to the following columns: 
data1<- subset(data, select= c("BEGIN_YEARMONTH","EPISODE_ID","STATE",
                              "STATE_FIPS", "CZ_NAME","CZ_TYPE","CZ_FIPS", "EVENT_TYPE"))
#3.Arrange the data by the state name (STATE)
data2<-arrange(data1, STATE,by_group = TRUE)

#4.Change state and county names to title case 
data2$STATE<-str_to_title(data2$STATE)
data2$CZ_NAME<-str_to_title(data2$CZ_NAME)
#5.Limit to the events listed by county FIPS (CZ_TYPE of “C”) and then remove the CZ_TYPE column 
fil_data <- data2[data2$CZ_TYPE == "C", ] 
fil_data<- subset(fil_data, select= -CZ_TYPE)
#6.Pad the state and county FIPS with a “0” at the beginning 

com_data <- fil_data %>%  
  mutate(  
    STATE_FIPS = str_pad(STATE_FIPS, 2, side = "left", pad = "0"),  
    CZ_FIPS = str_pad(CZ_FIPS, 3, side = "left", pad = "0")  
  )  
#6.Unite them togetehr
com_data$state_county_FIPS<- paste0(com_data$STATE_FIPS, com_data$CZ_FIPS)


#7. lower case of variable name
com_data <- com_data %>%  
  rename_all(tolower)  

#8.select data
data("state")  
state_data <- data.frame(  
  state_name = state.name,  
  area = state.area,  
  region = state.region  
)  

#9. Get the number of event
tables <- table(com_data$state)  
state_table <- as.data.frame(tables)  
#merge based on dataframe in question 8
merged_data <- merge(state_data, state_table, by.x = "state_name", by.y = "Var1")  

#create plots
ggplot(merged_data, aes(x = area, y = Freq, color = region)) +  
  geom_point() + 
  labs(x = "Land area (square miles)", y = "# of stom event in 1996", fill = "Region")

