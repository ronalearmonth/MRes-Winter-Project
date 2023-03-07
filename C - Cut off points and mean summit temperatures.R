### APPENDIX E - CALCULATING MEAN TEMP AT TOP OF MOUNTAIN FOR QUEENS AND WORKERS ###

#early vs late = transition from queen-dominated to worker-dominated
setwd("C:/Users/rona/OneDrive - Imperial College London/Rona_HV_2022/Data")
bees<-read.xlsx("Working_spreadsheet_BB_observ_18192122_v2.xlsx")
#filter out all non-caught bees

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
CastePhen2018 <- CastesSampled %>% subset(Year == "2018")
CastePhen2018 <- na.omit(CastePhen2018)

CastePhen2019 <- CastesSampled %>% subset(Year == "2019")
CastePhen2019 <- na.omit(CastePhen2019)

CastePhen2021 <- CastesSampled %>% subset(Year == "2021")
CastePhen2021 <- na.omit(CastePhen2021)

CastePhen2022 <- CastesSampled %>% subset(Year == "2022")
CastePhen2022 <- na.omit(CastePhen2022)

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

#fit binomial models of proportoin of workers against day of sampling
Cutoff_2018 <- glm(Proportion_Workers ~ as.integer(Since_sampling_begin), 
                   family = binomial(link="logit"), CastePhen2018)

Cutoff_2019 <- glm(Proportion_Workers ~ as.integer(Since_sampling_begin), 
                   family = binomial(link="logit"),CastePhen2019)

Cutoff_2021 <- glm(Proportion_Workers ~ as.integer(Since_sampling_begin), 
                   family = binomial(link="logit"),CastePhen2021)

Cutoff_2022 <- glm(Proportion_Workers ~ as.integer(Since_sampling_begin), 
                   family = binomial(link="logit"),CastePhen2022)

#solve equation of line for each to calculate cut off day
cutoff2018<-(0-Cutoff_2018[[1]][1])/(Cutoff_2018[[1]][2])
cutoff2019<-(0-Cutoff_2019[[1]][1])/(Cutoff_2019[[1]][2])
cutoff2021<-(0-Cutoff_2021[[1]][1])/(Cutoff_2021[[1]][2])
cutoff2022<-(0-Cutoff_2022[[1]][1])/(Cutoff_2022[[1]][2])


#save first day of sampling for each as day of year
start2018<-yday(min(as.Date(bees$Date, format="%d/%m/%Y"))) +1
start2019<-yday(min(as.Date(CastePhen2019$Date, format="%d/%m/%Y")))+1
start2021<-yday(min(as.Date(CastePhen2021$Date, format="%d/%m/%Y")))+1
start2022<-yday(min(as.Date(CastePhen2018$Date, format="%d/%m/%Y")))+1

#cutoffs represent number of days after sampling - convert to doy
doycutoff18<-start2018+cutoff2018
doycutoff19<-start2019+cutoff2019
doycutoff21<-start2021+cutoff2021
doycutoff22<-start2022+cutoff2022

rbind(doycutoff18,doycutoff19,doycutoff21,doycutoff22)

latestW<-bees%>%filter(Caste_Assigned_CT=="W")%>%group_by(Year)%>%summarise(lastsighting=max(DOY))
earliestQ<-bees%>%filter(Caste_Assigned_CT=="Q")%>%group_by(Year)%>%summarise(firstsighting=min(DOY))


#load summit temperature data
summittemp<-read.csv("summittemp.csv")

#convert date to date variable
summittemp$date<-dmy(summittemp$date)
summittemp$doy<-yday(summittemp$date)
str(summittemp)

#initial visualisation of data
head(summittemp)

#for each year, filter temperature for queens from first sighting to transition
#and for workers from transition to last sighting

#queens mean temp at summit 
q18temp<-na.omit(temp)%>%filter(year=="2018")%>%filter(doy>144&doy<179)
q19temp<-na.omit(temp)%>%filter(year=="2019")%>%filter(doy>138&doy<196)
q21temp<-na.omit(temp)%>%filter(year=="2021")%>%filter(doy>145&doy<183)
q22temp<-na.omit(temp)%>%filter(year=="2022")%>%filter(doy>142&doy<192)

#for workers, assume workers remain exposed to conditions until latest sampling period
w18temp<-na.omit(temp)%>%filter(year=="2018")%>%filter(doy>179&doy<245)
w19temp<-na.omit(temp)%>%filter(year=="2019")%>%filter(doy>196&doy<245)
w21temp<-na.omit(temp)%>%filter(year=="2021")%>%filter(doy>183&doy<245)
w22temp<-na.omit(temp)%>%filter(year=="2022")%>%filter(doy>192&doy<245)

#calculate mean temperature for each caste at summit
queentemp<-rbind(q18temp,q19temp,q21temp,q22temp)%>%summarise(mean=mean(airtemp),se=(sd(airtemp))/(sqrt(length(airtemp))))
workertemp<-rbind(w18temp,w19temp,w21temp,w22temp)%>%summarise(mean=mean(airtemp),se=(sd(airtemp))/(sqrt(length(airtemp))))
