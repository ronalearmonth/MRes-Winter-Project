#### HYPERVOLUME GENERATION AND PAIRWISE COMPARISONS ####

#RUN SECOND
library(tidyr)
library(dplyr)
library(cowplot)
library(ggplot2)
library(tidyverse)
library(hypervolume)
library(glue)
library(viridis)
library(lubridate)
library(openxlsx)
library(alphahull)
library(rgl)
library(ggthemes)
library(gt)
library(report)
library(data.table)
library(ggpubr)


## COMMUNITY HYPERVOLUMES HIGH VS LOW ##

HLbees<-z.hvbees%>%select(pca,wing_resid,head_resid,Caste_Assigned_CT,highlow)

HLbees=split(HLbees,paste(HLbees$Caste_Assigned_CT,
                          HLbees$highlow,sep=""))

#calculate hv for each element of splitted data
chHV=mapply(function(x,y)
  hypervolume_gaussian(x[,c("pca","wing_resid","head_resid")],
                       samples.per.point=100,name=y),
  x=HLbees,
  y=names(HLbees))

#save as total hypervolume list for year community highlow HV
chHVtotal=hypervolume_join(chHV)


# PLOTTING HYPERVOLUMES 

#plot all 4
hvplot_scaled(chHVtotal)
legend3d("topright",legend=c('High worker','Low worker',
                             'High queen','Low queen'),pch=16,col=colourstbc,cex=1.3)

#high has more outliers?

#try only joining highlow
mfrow3d(1,2,sharedMouse=T)
HVsHigh<-hypervolume_join(chHV$QH,chHV$QL)
hvplot_scaled(HVsHigh)
mtext3d("Queens",'x-',line=4,cex=1.4,las=3)
HVsLow<-hypervolume_join(chHV$WH,chHV$WL)
hvplot_scaled(HVsLow)
mtext3d("Workers",'x-',line=4,cex=1.4,las=3)
legend3d("topleft",legend=c('Above','Below'),pch=16,col=colourstbc,cex=1.3)

#save screenshot of hypervolume
#rgl.snapshot(filename="communityHL_HV.png",fmt="png")

# ANALYSIS
overlap.stat <- as.data.frame(NULL)
distance.stat <- as.data.frame(NULL)
min.dist.stat <- as.data.frame(NULL)
jaccard.stat <- as.data.frame(NULL)

## adding HV pairwise stats to dataframes - complete stats dataframe
#for all possible combinations of year/caste/highlow
for (i in 1:4) {
  for (j in 1:4) {
    # pair of HVs
    hv.set <- hypervolume_set(chHVtotal[[i]], chHVtotal[[j]], check.memory = FALSE)
    # overlap stats
    Analysis <- hypervolume_overlap_statistics(hv.set)
    overlap.stat[i,j] <- Analysis[2]
    jaccard.stat[i,j] <- Analysis[1]
    # distance stats
    distance.stat[i,j] <- hypervolume_distance(chHVtotal[[i]], chHVtotal[[j]])
    min.dist.stat[i,j] <- hypervolume_distance(chHVtotal[[i]], chHVtotal[[j]], type = "minimum", check.memory = FALSE)
  }
}

rownames<-c("QH","QL","WH","WL")
colnames<-c("QH","QL","WH","WL")

#set all analyses to correct row/column names
rownames(overlap.stat)<-rownames
colnames(overlap.stat)<-colnames
rownames(distance.stat)<-rownames
colnames(distance.stat)<-colnames
rownames(min.dist.stat)<-rownames
colnames(min.dist.stat)<-colnames
rownames(jaccard.stat)<-rownames
colnames(jaccard.stat)<-colnames

#visualise comparisons
statvisualise<-function(df){
  df%>%gt()%>%data_color(columns=c(QH,QL,WH,WL),
                         colors=scales::col_numeric(
                           palette = paletteer::paletteer_c(
                             palette = "ggthemes::Red-Gold",30) %>%
                             as.character(),
                           domain = NULL))}

statvisualise(overlap.stat)
statvisualise(jaccard.stat)
statvisualise(distance.stat)
statvisualise(min.dist.stat)






## COMMUNITY HYPERVOLUMES BY YEAR ##

# MAKE HYPERVOLUMES

#split by caste, year
tempbeesyc<-z.hvbees%>%select(pca,wing_resid,head_resid,Caste_Assigned_CT,Year)

tempbeesyc=split(tempbeesyc,paste(tempbeesyc$Caste_Assigned_CT,tempbeesyc$Year,sep=""))
ycHV=mapply(function(x,y)
  hypervolume_gaussian(x[,c("pca","wing_resid","head_resid")],
                       samples.per.point=100,name=y),
  x=tempbeesyc,
  y=names(tempbeesyc))

#save as total hypervolume list for year community highlow HV
ycHVtotal=hypervolume_join(ycHV)

# ANALYSE HYPERVOLUMES 

#make empty data frames to store stats in
overlap.stat <- as.data.frame(NULL)
distance.stat <- as.data.frame(NULL)
min.dist.stat <- as.data.frame(NULL)
jaccard.stat <- as.data.frame(NULL)

## adding HV pairwise stats to dataframes - complete stats dataframe
#for all possible combinations of year/caste/highlow
for (i in 1:8) {
  for (j in 1:8) {
    # pair of HVs
    hv.set <- hypervolume_set(ycHVtotal[[i]], ycHVtotal[[j]], check.memory = FALSE)
    # overlap stats
    Analysis <- hypervolume_overlap_statistics(hv.set)
    overlap.stat[i,j] <- Analysis[2]
    jaccard.stat[i,j] <- Analysis[1]
    # distance stats
    distance.stat[i,j] <- hypervolume_distance(ycHVtotal[[i]], ycHVtotal[[j]])
    min.dist.stat[i,j] <- hypervolume_distance(ycHVtotal[[i]], ycHVtotal[[j]], type = "minimum", check.memory = FALSE)
  }
}

#save rownames and column names
rownames<-c("Q2018","Q2019","Q2021","Q2022",
            "W2018","W2019","W2021","W2022")
colnames<-c("Q2018","Q2019","Q2021","Q2022",
            "W2018","W2019","W2021","W2022")

#set all analyses to correct row/column names
rownames(overlap.stat)<-rownames
colnames(overlap.stat)<-colnames
rownames(distance.stat)<-rownames
colnames(distance.stat)<-colnames
rownames(min.dist.stat)<-rownames
colnames(min.dist.stat)<-colnames
rownames(jaccard.stat)<-rownames
colnames(jaccard.stat)<-colnames
#create function to visualise which HVs are most similar/distant
statvisualise<-function(df){
  df%>%gt()%>%data_color(columns=c(Q2018,Q2019,Q2021,Q2022,
                                   W2018,W2019,W2021,W2022),
                         colors=scales::col_numeric(
                           palette = paletteer::paletteer_c(
                             palette = "ggthemes::Red-Gold",30) %>%
                             as.character(),
                           domain = NULL))}

#visualise each set of stats with heat map
statvisualise(overlap.stat)
statvisualise(distance.stat)
statvisualise(min.dist.stat)
statvisualise(jaccard.stat)


# save HYPERVOLUMES

#save hypervolumes for each height in each year
hv18<-hypervolume_join(ycHV$Q2018,ycHV$W2018)
hv19<-hypervolume_join(ycHV$Q2019,ycHV$W2019)
hv21<-hypervolume_join(ycHV$Q2021,ycHV$W2021)
hv22<-hypervolume_join(ycHV$Q2022,ycHV$W2022)


## TEMPERATURE DATA ##

temp<-read.csv("temperature_1985-2023.csv")

#filter to include 2017-2022
temp<-temp%>%filter(Year>2000&Year<2023)

#convert date to date variable
temp$date<-dmy(temp$ï..Date)
temp$doy<-yday(temp$date)
str(temp)

#initial visualisation of data
head(temp)
summary(temp)
str(temp)
names(temp)

#workers DEVELOPMENT period

#for each year, calculate the earliest date a worker was seen
earliestW<-bees%>%filter(Caste_Assigned_CT=="W")%>%group_by(Year)%>%summarise(firstsighting=min(DOY))
#take the 30 day period before first sighting as the 'development temperature' period

#worker development phase temperatures for each year
w18devtemp<-temp%>%filter(Year=="2018" & doy>=128 & doy<=158)
w19devtemp<-temp%>%filter(Year=="2019" & doy>=133 & doy<=163)
w21devtemp<-temp%>%filter(Year=="2021" & doy>=143 & doy<=173)
w22devtemp<-temp%>%filter(Year=="2022" & doy>=131 & doy<=161)

#join into new temperature dataset with all years
workerdevelopment_temp<-rbind(w18devtemp,w19devtemp,w21devtemp,w22devtemp)%>%
  group_by(Year)%>%summarise(meandevtemp=mean(AirTemperature))

#then assign years as A B C D based on highest to lowest temperature
#ranked years - 2022 - 2019 - 2018 - 2021

#assign ranks for temp in current year to bees data
bees<-bees%>%mutate(devyearW=case_when(
  Year=="2018"~"C",
  Year=="2019"~"B",
  Year=="2021"~"D",
  Year=="2022"~"A"))

#plot workers only by increasing year
wA<-hypervolume_join(ycHV$W2022)
wB<-hypervolume_join(ycHV$W2019)
wC<-hypervolume_join(ycHV$W2018)
wD<-hypervolume_join(ycHV$W2021)

#workers in blue - new plot function blue
hvplot_w<-function(x){
  plot.HypervolumeList(x,show.3d=TRUE,plot.3d.axes.id=NULL,
                       show.axes=TRUE, show.frame=FALSE,
                       cex.data=10,cex.random=2,
                       show.density=FALSE,show.data=FALSE,
                       names=NULL, show.legend=FALSE, colors="#88CCEE", 
                       show.contour=TRUE, contour.lwd=1.5, 
                       contour.type='kde',add=FALSE,limits=list(c(-4,2),c(-4,4),c(-4,6)))}


#queens DEVELOPMENT period - need to do some reading and figure out when this is - gerard 2020b?

#take queen temperature as summer temperature from the previous year? - day 140 to 240
qdevtemps<-temp%>%group_by(Year)%>%filter(doy>=182&doy<=244)%>%summarise(meanqdevtemp=mean(AirTemperature))%>%mutate(y2=Year+1)

#note:
#2018 - 10.5
#2019 - 12.8
#2021 - 10.6
#2022 - 11.2

#assign ranks for temp in current year to bees data
bees<-bees%>%mutate(devyearQ=case_when(
  Year=="2018"~"A",
  Year=="2019"~"D",
  Year=="2021"~"B",
  Year=="2022"~"C"))

#plot queens only by increasing year
qA<-hypervolume_join(ycHV$Q2018)
qB<-hypervolume_join(ycHV$Q2021)
qC<-hypervolume_join(ycHV$Q2022)
qD<-hypervolume_join(ycHV$Q2019)

#INCREASING DEVELOPMENT TEMPERATURE PLOT

mfrow3d(2,4,sharedMouse=TRUE)

#plot queens
hvplot_scaled(qA)
mtext3d("Queens",'z-',line=4,cex=1.2,las=3)
mtext3d("A: 2018",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_scaled(qB)
mtext3d("B: 2021",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_scaled(qC)
mtext3d("C: 2022",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_scaled(qD)
mtext3d("D: 2019",'x--',outer=TRUE,line=3,cex=1.2,las=1)
legend3d("topright",legend=c('Queen','Worker'),pch=16,col=colourstbc,cex=1.3)

#plot workers
hvplot_w(wA)
mtext3d("Workers",'z-',line=4,cex=1.2,las=3)
mtext3d("A: 2022",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_w(wB)
mtext3d("B: 2019",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_w(wC)
mtext3d("C: 2018",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_w(wD)
mtext3d("D: 2021",'x--',outer=TRUE,line=3,cex=1.2,las=1)

#rgl.snapshot(filename="devtemp_community.png",fmt="png")


##plot years together in separate caste figures?

queenshv<-hypervolume_join(ycHV$Q2018,ycHV$Q2019,ycHV$Q2021,ycHV$Q2022)
workershv<-hypervolume_join(ycHV$W2018,ycHV$W2019,ycHV$W2021,ycHV$W2022)
mfrow3d(1,2,sharedMouse=TRUE)
hvplot_scaled(queenshv)
mtext3d("Queens",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_scaled(workershv)
mtext3d("Workers",'x--',outer=TRUE,line=3,cex=1.2,las=1)

legend3d("topright",legend=c("2018","2019","2021","2022"),pch=16,col=colourstbc,cex=1.3)





## COMMUNITY HYPERVOLUMES BY YEAR AND HIGH VS LOW ##

# MAKE HYPERVOLUMES 

#split by highlow, caste, altitude
tempbees<-z.hvbees%>%select(pca,wing_resid,head_resid,Caste_Assigned_CT,highlow,Year)

tempbees=split(tempbees,paste(tempbees$Caste_Assigned_CT,
                              tempbees$highlow,tempbees$Year,sep=""))

#calculate hv for each element of splitted data
ychHV=mapply(function(x,y)
  hypervolume_gaussian(x[,c("pca","wing_resid","head_resid")],
                       samples.per.point=100,name=y),
  x=tempbees,
  y=names(tempbees))

#save as total hypervolume list for year community highlow HV
ychHVtotal=hypervolume_join(ychHV)

# ANALYSIS

#make empty data frames to store stats in
overlap.stat <- as.data.frame(NULL)
distance.stat <- as.data.frame(NULL)
min.dist.stat <- as.data.frame(NULL)
jaccard.stat <- as.data.frame(NULL)


## adding HV pairwise stats to dataframes - complete stats dataframe
#for all possible combinations of year/caste/highlow
for (i in 1:16) {
  for (j in 1:16) {
    # pair of HVs
    hv.set <- hypervolume_set(ychHVtotal[[i]], ychHVtotal[[j]], check.memory = FALSE)
    # overlap stats
    Analysis <- hypervolume_overlap_statistics(hv.set)
    overlap.stat[i,j] <- Analysis[2]
    jaccard.stat[i,j] <- Analysis[1]
    # distance stats
    distance.stat[i,j] <- hypervolume_distance(ychHVtotal[[i]], ychHVtotal[[j]])
    min.dist.stat[i,j] <- hypervolume_distance(ychHVtotal[[i]], ychHVtotal[[j]], type = "minimum", check.memory = FALSE)
  }
}

#save rownames and column names
rownames<-c("HQ2018","HQ2019","HQ2021","HQ2022",
            "LQ2018","LQ2019","LQ2021","LQ2022",
            "HW2018","HW2019","HW2021","HW2022",
            "LW2018","LW2019","LW2021","LW2022")
colnames<-c("HQ2018","HQ2019","HQ2021","HQ2022",
            "LQ2018","LQ2019","LQ2021","LQ2022",
            "HW2018","HW2019","HW2021","HW2022",
            "LW2018","LW2019","LW2021","LW2022")

#set all analyses to correct row/column names
rownames(overlap.stat)<-rownames
colnames(overlap.stat)<-colnames
rownames(distance.stat)<-rownames
colnames(distance.stat)<-colnames
rownames(min.dist.stat)<-rownames
colnames(min.dist.stat)<-colnames
rownames(jaccard.stat)<-rownames
colnames(jaccard.stat)<-colnames

#create function to visualise which HVs are most similar/distant
statvisualise<-function(df){
  df%>%gt()%>%data_color(columns=c(HQ2018,HQ2019,HQ2021,HQ2022,
                                   LQ2018,LQ2019,LQ2021,LQ2022,
                                   HW2018,HW2019,HW2021,HW2022,
                                   LW2018,LW2019,LW2021,LW2022),
                         colors=scales::col_numeric(
                           palette = paletteer::paletteer_c(
                             palette = "ggthemes::Red-Gold",30) %>%
                             as.character(),
                           domain = NULL))}

#visualise each set of stats with heat map
statvisualise(overlap.stat)
statvisualise(distance.stat)
statvisualise(min.dist.stat)
statvisualise(jaccard.stat)

# PLOT HYPERVOLUMES

#save hypervolumes for each height in each year
H18<-hypervolume_join(ychHV$QH2018,ychHV$WH2018)
H19<-hypervolume_join(ychHV$QH2019,ychHV$WH2019)
H21<-hypervolume_join(ychHV$QH2021,ychHV$WH2021)
H22<-hypervolume_join(ychHV$QH2022,ychHV$WH2022)
L18<-hypervolume_join(ychHV$QL2018,ychHV$WL2018)
L19<-hypervolume_join(ychHV$QL2019,ychHV$WL2019)
L21<-hypervolume_join(ychHV$QL2021,ychHV$WL2021)
L22<-hypervolume_join(ychHV$QL2022,ychHV$WL2022)


# end script
