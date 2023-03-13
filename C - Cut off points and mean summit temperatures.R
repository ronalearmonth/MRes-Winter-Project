### APPENDIX E - CALCULATING MEAN TEMP AT TOP OF MOUNTAIN FOR QUEENS AND WORKERS ###

rm(list=ls())

#load required packages
library(dplyr)
library(lubridate)

#set working directory and load bee data
setwd("C:/Users/rona/OneDrive - Imperial College London/Rona_HV_2022/Data")
bees<-read.xlsx("Working_spreadsheet_BB_observ_18192122_v2.xlsx")

#remove parasitic species without q/w switch
bees<-bees%>%filter(Caste_Assigned_CT=="Q"|
                            Caste_Assigned_CT=="W")%>%
  filter(Social_parasitic=="Social")

#summarise proportion of workers 
CastesSampled<-bees%>%
  group_by(Year,Date)%>%
  summarise(Queens=sum(Caste_Assigned_CT=="Q"),
            Workers=sum(Caste_Assigned_CT=="W"))%>%
  mutate(Proportion_Workers=Workers/(Queens+Workers))

#separate years because they have different switch dates'
CastePhen2018 <- na.omit(CastesSampled %>% subset(Year == "2018"))

CastePhen2019 <- na.omit(CastesSampled %>% subset(Year == "2019"))

CastePhen2021 <- na.omit(CastesSampled %>% subset(Year == "2021"))

CastePhen2022 <- na.omit(CastesSampled %>% subset(Year == "2022"))

#these include infinite values

#calculate date as 'days since sampling began'
#earliest date in each year = sampling day 1
CastePhen2018$Since_sampling_begin<-(as.Date(CastePhen2018$Date, format = "%d/%m/%Y")-
                                       min(as.Date(CastePhen2018$Date, format="%d/%m/%Y"))) +1

CastePhen2019$Since_sampling_begin<-(as.Date(CastePhen2019$Date, format = "%d/%m/%Y")-
                                       min(as.Date(CastePhen2019$Date, format="%d/%m/%Y"))) +1 

CastePhen2021$Since_sampling_begin<-(as.Date(CastePhen2021$Date, format = "%d/%m/%Y")-
                                       min(as.Date(CastePhen2021$Date, format="%d/%m/%Y"))) +1

CastePhen2022$Since_sampling_begin<-(as.Date(CastePhen2022$Date, format = "%d/%m/%Y")-
                                       min(as.Date(CastePhen2022$Date, format="%d/%m/%Y"))) +1

#fit binomial models of proportion of workers against day of sampling
workerprop_2018 <- glm(Proportion_Workers ~ as.integer(Since_sampling_begin), 
                   family = binomial(link="logit"), CastePhen2018)

workerprop_2019 <- glm(Proportion_Workers ~ as.integer(Since_sampling_begin), 
                   family = binomial(link="logit"),CastePhen2019)

workerprop_2021 <- glm(Proportion_Workers ~ as.integer(Since_sampling_begin), 
                   family = binomial(link="logit"),CastePhen2021)

workerprop_2022 <- glm(Proportion_Workers ~ as.integer(Since_sampling_begin), 
                   family = binomial(link="logit"),CastePhen2022)

#solve equation of line for each to calculate cut off day in each year
  #(point where probability of a random sampled individual being a worker > probability of being a queen)
cutoff2018<-(0-Cutoff_2018[[1]][1])/(Cutoff_2018[[1]][2])
cutoff2019<-(0-Cutoff_2019[[1]][1])/(Cutoff_2019[[1]][2])
cutoff2021<-(0-Cutoff_2021[[1]][1])/(Cutoff_2021[[1]][2])
cutoff2022<-(0-Cutoff_2022[[1]][1])/(Cutoff_2022[[1]][2])


#save first day of sampling for each as day of year
start2018<-yday(min(as.Date(bees$Date, format="%d/%m/%Y"))) +1
start2019<-yday(min(as.Date(CastePhen2019$Date, format="%d/%m/%Y")))+1
start2021<-yday(min(as.Date(CastePhen2021$Date, format="%d/%m/%Y")))+1
start2022<-yday(min(as.Date(CastePhen2018$Date, format="%d/%m/%Y")))+1

#cutoffs represent number of days after sampling - convert to day of year (doy)
doycutoff18<-start2018+cutoff2018
doycutoff19<-start2019+cutoff2019
doycutoff21<-start2021+cutoff2021
doycutoff22<-start2022+cutoff2022

#join as new dataset
rbind(doycutoff18,doycutoff19,doycutoff21,doycutoff22)

#calculate beginning of sample season (earliestQ) and end (latestW)
earliestQ<-bees%>%filter(Caste_Assigned_CT=="Q")%>%group_by(Year)%>%summarise(firstsighting=min(DOY))
latestW<-bees%>%filter(Caste_Assigned_CT=="W")%>%group_by(Year)%>%summarise(lastsighting=max(DOY))

#load summit temperature data
summittemp<-read.csv("summittemp.csv")

#convert date to dmy format
summittemp$date<-dmy(summittemp$date)

#create column for day of year
summittemp$doy<-yday(summittemp$date)

#convert temp to daily mean
summittemp<-summittemp%>%group_by(year,doy)%>%summarise(airtemp=mean(airtemp))

#for each year, filter temperature for queens from first sighting to transition
#and for workers from transition to last sighting

#queens mean temp at summit 
q18temp<-na.omit(summittemp)%>%filter(year=="2018")%>%filter(doy>144&doy<179)
q19temp<-na.omit(summittemp)%>%filter(year=="2019")%>%filter(doy>138&doy<196)
q21temp<-na.omit(summittemp)%>%filter(year=="2021")%>%filter(doy>145&doy<183)
q22temp<-na.omit(summittemp)%>%filter(year=="2022")%>%filter(doy>142&doy<192)

#for workers, assume workers remain exposed to conditions until latest sampling period
w18temp<-na.omit(summittemp)%>%filter(year=="2018")%>%filter(doy>179&doy<245)
w19temp<-na.omit(summittemp)%>%filter(year=="2019")%>%filter(doy>196&doy<245)
w21temp<-na.omit(summittemp)%>%filter(year=="2021")%>%filter(doy>183&doy<245)
w22temp<-na.omit(summittemp)%>%filter(year=="2022")%>%filter(doy>192&doy<245)

#calculate mean temperature for each caste at summit
queentemp<-rbind(q18temp,q21temp)%>%ungroup()%>%summarise(mean=mean(airtemp),se=(sd(airtemp))/(sqrt(length(airtemp))))
workertemp<-rbind(w18temp,w19temp,w21temp,w22temp)%>%ungroup()%>%summarise(mean=mean(airtemp),se=(sd(airtemp))/(sqrt(length(airtemp))))


## COMPARE MEAN TEMPERATURE DURING COLLECTION PERIOD AT TOP AND BOTTOM OF THE MOUNTAIN

# SUMMIT #
smeantemp18<-na.omit(summittemp)%>%filter(year=="2018")%>%filter(doy>144&doy<201)
smeantemp19<-na.omit(summittemp)%>%filter(year=="2019")%>%filter(doy>138&doy<200)
smeantemp21<-na.omit(summittemp)%>%filter(year=="2021")%>%filter(doy>145&doy<245)
smeantemp22<-na.omit(summittemp)%>%filter(year=="2022")%>%filter(doy>142&doy<245)

#calculate mean (+/- SE) temperature at bottom of mountain for collection period across all 4 years
smeantemp<-rbind(smeantemp18,smeantemp19,smeantemp21,smeantemp22)%>%
  ungroup()%>%summarise(mean=mean(airtemp),se=(sd(airtemp))/(sqrt(length(airtemp))))

# BASE #
#import weather station data
temp<-read.csv("temperature_1985-2023.csv")

#convert date to date variable
temp$date<-dmy(temp$ï..Date)
temp$doy<-yday(temp$date)

#convert temp to daily mean
temp<-temp%>%group_by(Year,doy)%>%summarise(AirTemperature=mean(AirTemperature))

#filter for temp during collection period temp in each year
meantemp18<-na.omit(temp)%>%filter(Year=="2018")%>%filter(doy>144&doy<201)
meantemp19<-na.omit(temp)%>%filter(Year=="2019")%>%filter(doy>138&doy<200)
meantemp21<-na.omit(temp)%>%filter(Year=="2021")%>%filter(doy>145&doy<245)
meantemp22<-na.omit(temp)%>%filter(Year=="2022")%>%filter(doy>142&doy<245)

#calculate mean (+/-) temperature at bottom of mountain for collection period across all 4 years
meanoveralltemp<-rbind(meantemp18,meantemp19,meantemp21,meantemp22)%>%
  ungroup()%>%summarise(mean=mean(AirTemperature), se=(sd(AirTemperature))/(sqrt(length(AirTemperature))))

### end script ###