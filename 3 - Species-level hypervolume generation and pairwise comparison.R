##### SPECIES HYPERVOLUME GENERATION AND PAIRWISE COMPARISON #####

#run "1 - Data preparation.R" first

#load required packages
library(tidyr)
library(dplyr)
library(cowplot)
library(ggplot2)
library(tidyverse)
library(hypervolume)
library(glue)
library(viridis)
library(paletteer)
library(lubridate)
library(lme4)
library(openxlsx)
library(alphahull)
library(rgl)
library(ggthemes)
library(gt)
library(report)
library(data.table)

## SPECIES BY HIGH VS LOW ##

#group by species, caste, highlow
samplesize<-z.hvbees%>%group_by(Bombus_Species,highlow,Caste_Assigned_CT)%>%summarise(count=n())
#include only species with sample size >= 30 in each group
samplesp<-samplesize%>%filter(count>=30)

#use monticola, laponicus, balteatus (borderline)
sp.beesvegzone<-z.hvbees%>%filter(Bombus_Species=="monticola"|Bombus_Species=="balteatus"|Bombus_Species=="lapponicus")


#create new temporary df
spvegszone_split<-sp.beesvegzone%>%select(pca,wing_resid,head_resid,Bombus_Species,Caste_Assigned_CT,highlow)

#split by species, caste, altitude
spvegszone_split=split(spvegszone_split,paste(spvegszone_split$Bombus_Species,
                                spvegszone_split$Caste_Assigned_CT,
                                spvegszone_split$highlow,sep=""))

#calculate hv for each element of split data
spchHV=mapply(function(x,y)
  hypervolume_gaussian(x[,c("pca","wing_resid","head_resid")],
                       samples.per.point=100,name=y),
  x=spvegszone_split,
  y=names(spvegszone_split))

#save all hypervolumes as a total hypervolume list
spchHVtotal=hypervolume_join(spchHV)


#ANALYSE SP/CASTE/HL hypervolumes
spoverlap.stat <- as.data.frame(NULL)
spdistance.stat <- as.data.frame(NULL)
spmin.dist.stat <- as.data.frame(NULL)
spjaccard.stat <- as.data.frame(NULL)

## adding HV pairwise stats to dataframes - complete stats dataframe
#for all possible combinations of year/caste/highlow
for (i in 1:12) {
  for (j in 1:12) {
    # pair of HVs
    sp.hv.set <- hypervolume_set(spchHVtotal[[i]], spchHVtotal[[j]], check.memory = FALSE)
    # overlap stats
    sp.analysis <- hypervolume_overlap_statistics(sp.hv.set)
    spoverlap.stat[i,j] <- sp.analysis[2]
    spjaccard.stat[i,j] <- sp.analysis[1]
    # distance stats
    spdistance.stat[i,j] <- hypervolume_distance(spchHVtotal[[i]], spchHVtotal[[j]])
    spmin.dist.stat[i,j] <- hypervolume_distance(spchHVtotal[[i]], spchHVtotal[[j]], type = "minimum", check.memory = FALSE)
  }}

#save rownames and column names
rownames<-c("HQbalteatus","LQbalteatus","HWbalteatus","LWbalteatus",
            "HQlapponicus","LQlapponicus","HWlapponicus","LWlapponicus",
            "HQmonticola","LQmonticola","HWmonticola","LWmonticola")
colnames<-c("HQbalteatus","LQbalteatus","HWbalteatus","LWbalteatus",
            "HQlapponicus","LQlapponicus","HWlapponicus","LWlapponicus",
            "HQmonticola","LQmonticola","HWmonticola","LWmonticola")

#set all analyses to correct row/column names
rownames(spoverlap.stat)<-rownames
colnames(spoverlap.stat)<-colnames
rownames(spdistance.stat)<-rownames
colnames(spdistance.stat)<-colnames
rownames(spmin.dist.stat)<-rownames
colnames(spmin.dist.stat)<-colnames
rownames(spjaccard.stat)<-rownames
colnames(spjaccard.stat)<-colnames

#create function visualise how similarity/distance varies with species
sp.statvisualise<-function(df){
  df%>%gt()%>%data_color(columns=c(HQbalteatus,LQbalteatus,HWbalteatus,LWbalteatus,
                                   HQlapponicus,LQlapponicus,HWlapponicus,LWlapponicus,
                                   HQmonticola,LQmonticola,HWmonticola,LWmonticola),
                         colors=scales::col_numeric(
                           palette = paletteer::paletteer_c(
                             palette = "ggthemes::Red-Gold",30) %>%
                             as.character(),
                           domain = NULL))}

#visualise each stat calculated
sp.statvisualise(spoverlap.stat)
sp.statvisualise(spdistance.stat)
sp.statvisualise(spmin.dist.stat)
sp.statvisualise(spjaccard.stat)

#save each caste/height combination to plot
QH<-hypervolume_join(spchHV$balteatusQH,spchHV$lapponicusQH,spchHV$monticolaQH)
WH<-hypervolume_join(spchHV$balteatusWH,spchHV$lapponicusWH,spchHV$monticolaWH)
QL<-hypervolume_join(spchHV$balteatusQL,spchHV$lapponicusQL,spchHV$monticolaQL)
WL<-hypervolume_join(spchHV$balteatusWL,spchHV$lapponicusWL,spchHV$monticolaWL)

#set plotting grid 
mfrow3d(2,2,sharedMouse=TRUE)

#plot each hypervolume in grid
hvplot_scaled(QH)
mtext3d("Above",'z-',outer=TRUE,line=4,cex=1.4,las=3)
hvplot_scaled(WH)
legend3d("topright",legend=c('Balteatus','Lapponicus','Monticola'),pch=16,col=colourpalette,cex=1)
hvplot_scaled(QL)
mtext3d("Queens",'x--',outer=TRUE,line=3,cex=1.4,las=1)
mtext3d("Below",'z-',outer=TRUE,line=4,cex=1.4,las=3)
hvplot_scaled(WL)
mtext3d("Workers",'x--',outer=TRUE,line=3,cex=1.4,las=1)

#save as snapshot
#rgl.snapshot(filename="sp_height.png",fmt="png")



### SPECIES HYPERVOLUMES BY YEAR ##

#is there enough data to look at year/caste/species/highlow?
spych<-z.hvbees%>%group_by(Bombus_Species,Caste_Assigned_CT,highlow,Year)%>%
  summarise(count=n())%>%filter(count>20)
#no species with sample size > 30 for all years

#as high vs low does not appear to have significant impact
#is there enough data to look at year/caste/species?
spyc<-z.hvbees%>%group_by(Bombus_Species,Caste_Assigned_CT,Year)%>%
  summarise(count=n())%>%filter(count>20)
#lapponicus, monticola, pratorum if we lower standards to sample size to 20


sp.beesyear<-z.hvbees%>%filter(Bombus_Species=="lapponicus"|Bombus_Species=="monticola"|Bombus_Species=="pratorum")

#create year/species/caste df
spyear_split<-sp.beesyear%>%select(pca,wing_resid,head_resid,Bombus_Species,Caste_Assigned_CT,Year)

#split by species, caste, altitude
spyear_split=split(spyear_split,paste(spyear_split$Bombus_Species,
                                spyear_split$Caste_Assigned_CT,
                                spyear_split$Year,sep=""))

#calculate hv for each element of split data
spycHV=mapply(function(x,y)
  hypervolume_gaussian(x[,c("pca","wing_resid","head_resid")],
                       samples.per.point=100,name=y),
  x=spyear_split,
  y=names(spyear_split))

#save all hypervolumes as a total hypervolume list
spycHVtotal=hypervolume_join(spycHV)


# ANALYSE SP/CASTE/HL hypervolumes
spyoverlap.stat <- as.data.frame(NULL)
spydistance.stat <- as.data.frame(NULL)
spymin.dist.stat <- as.data.frame(NULL)
spyjaccard.stat <- as.data.frame(NULL)

## adding HV pairwise stats to dataframes - complete stats dataframe
#for all possible combinations of year/caste/highlow
for (i in 1:24) {
  for (j in 1:24) {
    # pair of HVs
    spy.hv.set <- hypervolume_set(spycHVtotal[[i]], spycHVtotal[[j]], check.memory = FALSE)
    # overlap stats
    spy.analysis <- hypervolume_overlap_statistics(spy.hv.set)
    spyoverlap.stat[i,j] <- spy.analysis[2]
    spyjaccard.stat[i,j] <- spy.analysis[1]
    # distance stats
    spydistance.stat[i,j] <- hypervolume_distance(spycHVtotal[[i]], spycHVtotal[[j]])
    spymin.dist.stat[i,j] <- hypervolume_distance(spycHVtotal[[i]], spycHVtotal[[j]], type = "minimum", check.memory = FALSE)
  }}

#save rownames and column names
rownames<-c("lapponicusQ2018","lapponicusQ2019","lapponicusQ2021","lapponicusQ2022",
            "lapponicusW2018","lapponicusW2019","lapponicusW2021","lapponicusW2022",
            "monticolaQ2018","monticolaQ2019","monticolaQ2021","monticolaQ2022",
            "monticolaW2018","monticolaW2019","monticolaW2021","monticolaW2022",
            "pratorumQ2018","pratorumQ2019","pratorumQ2021","pratorumQ2022",
            "pratorumW2018","pratorumW2019","pratorumW2021","pratorumW2022")
colnames<-c("lapponicusQ2018","lapponicusQ2019","lapponicusQ2021","lapponicusQ2022",
            "lapponicusW2018","lapponicusW2019","lapponicusW2021","lapponicusW2022",
            "monticolaQ2018","monticolaQ2019","monticolaQ2021","monticolaQ2022",
            "monticolaW2018","monticolaW2019","monticolaW2021","monticolaW2022",
            "pratorumQ2018","pratorumQ2019","pratorumQ2021","pratorumQ2022",
            "pratorumW2018","pratorumW2019","pratorumW2021","pratorumW2022")

#set all analyses to correct row/column names
rownames(spyoverlap.stat)<-rownames
colnames(spyoverlap.stat)<-colnames
rownames(spydistance.stat)<-rownames
colnames(spydistance.stat)<-colnames
rownames(spymin.dist.stat)<-rownames
colnames(spymin.dist.stat)<-colnames
rownames(spyjaccard.stat)<-rownames
colnames(spyjaccard.stat)<-colnames

#create function visualise how similarity/distance varies with species
spy.statvisualise<-function(df){
  df%>%gt()%>%data_color(columns=c(lapponicusQ2018,lapponicusQ2019,lapponicusQ2021,lapponicusQ2022,
                                   lapponicusW2018,lapponicusW2019,lapponicusW2021,lapponicusW2022,
                                   monticolaQ2018,monticolaQ2019,monticolaQ2021,monticolaQ2022,
                                   monticolaW2018,monticolaW2019,monticolaW2021,monticolaW2022,
                                   pratorumQ2018,pratorumQ2019,pratorumQ2021,pratorumQ2022,
                                   pratorumW2018,pratorumW2019,pratorumW2021,pratorumW2022),
                         colors=scales::col_numeric(
                           palette = paletteer::paletteer_c(
                             palette = "ggthemes::Red-Gold",30) %>%
                             as.character(),
                           domain = NULL))}


#visualise each stat calculated
spy.statvisualise(spyoverlap.stat)
spy.statvisualise(spydistance.stat)
spy.statvisualise(spymin.dist.stat)
spy.statvisualise(spyjaccard.stat)

#save each caste/year combination 
#queens
Q2018<-hypervolume_join(spycHV$pratorumQ2018,spycHV$lapponicusQ2018,spycHV$monticolaQ2018)
Q2019<-hypervolume_join(spycHV$pratorumQ2019,spycHV$lapponicusQ2019,spycHV$monticolaQ2019)
Q2021<-hypervolume_join(spycHV$pratorumQ2021,spycHV$lapponicusQ2021,spycHV$monticolaQ2021)
Q2022<-hypervolume_join(spycHV$pratorumQ2022,spycHV$lapponicusQ2022,spycHV$monticolaQ2022)

#workers
W2018<-hypervolume_join(spycHV$pratorumW2018,spycHV$lapponicusW2018,spycHV$monticolaW2018)
W2019<-hypervolume_join(spycHV$pratorumW2019,spycHV$lapponicusW2019,spycHV$monticolaW2019)
W2021<-hypervolume_join(spycHV$pratorumW2021,spycHV$lapponicusW2021,spycHV$monticolaW2021)
W2022<-hypervolume_join(spycHV$pratorumW2022,spycHV$lapponicusW2022,spycHV$monticolaW2022)


#set plotting grid 
mfrow3d(2,4,sharedMouse=TRUE)

#plot each hv by mean development temperature
hvplot_scaled(Q2018)
mtext3d("A: 2018",'x--',outer=TRUE,line=3,cex=1.2,las=1)
mtext3d("Queens",'z-',outer=TRUE,line=4,cex=1.2,las=3)
hvplot_scaled(Q2021)
mtext3d("B: 2021",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_scaled(Q2022)
mtext3d("C: 2022",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_scaled(Q2019)
mtext3d("D: 2019",'x--',outer=TRUE,line=3,cex=1.2,las=1)
legend3d("topright",legend=c('Lapponicus','Monticola','Pratorum'),pch=16,col=colourpalette,cex=1)

hvplot_scaled(W2022)
mtext3d("Workers",'z-',outer=TRUE,line=4,cex=1.2,las=3)
mtext3d("A: 2022",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_scaled(W2019)
mtext3d("B: 2019",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_scaled(W2018)
mtext3d("C: 2018",'x--',outer=TRUE,line=3,cex=1.2,las=1)
hvplot_scaled(W2021)
mtext3d("D: 2021",'x--',outer=TRUE,line=3,cex=1.4,las=1)

#save snapshot of figure
#rgl.snapshot(filename="sp_year.png",fmt="png")


## SPECIES HYPERVOLUMES (no year, no high vs low) ##
#not included in final project - data exploration

spc<-z.hvbees%>%group_by(Bombus_Species,Caste_Assigned_CT)%>%
  summarise(count=n())%>%filter(count>30)
#7 if we lower standards to sample size to 20


sp.bees3<-z.hvbees%>%filter(Bombus_Species=="balteatus"|Bombus_Species=="lapponicus"|
                              Bombus_Species=="lucorum"|Bombus_Species=="monticola"|
                              Bombus_Species=="pratorum")

#create year/species/caste df
tempbees4<-sp.bees3%>%select(pca,wing_resid,head_resid,Bombus_Species,Caste_Assigned_CT)

#split by species, caste, altitude
tempbees4=split(tempbees4,paste(tempbees4$Bombus_Species,
                                tempbees4$Caste_Assigned_CT,sep=""))

#calculate hv for each element of split data
spHV=mapply(function(x,y)
  hypervolume_gaussian(x[,c("pca","wing_resid","head_resid")],
                       samples.per.point=100,name=y),
  x=tempbees4,
  y=names(tempbees4))

#save all hypervolumes as a total hypervolume list
spHVtotal=hypervolume_join(spHV)


#ANALYSE SP/CASTE/HL hypervolumes
sp_overlap.stat <- as.data.frame(NULL)
sp_distance.stat <- as.data.frame(NULL)
sp_min.dist.stat <- as.data.frame(NULL)
sp_jaccard.stat <- as.data.frame(NULL)

## adding HV pairwise stats to dataframes - complete stats dataframe
#for all possible combinations of year/caste/highlow
for (i in 1:10) {
  for (j in 1:10) {
    # pair of HVs
    sp.hv.set <- hypervolume_set(spHVtotal[[i]], spHVtotal[[j]], check.memory = FALSE)
    # overlap stats
    sp_analysis <- hypervolume_overlap_statistics(sp.hv.set)
    sp_overlap.stat[i,j] <- sp_analysis[2]
    sp_jaccard.stat[i,j] <- sp_analysis[1]
    # distance stats
    sp_distance.stat[i,j] <- hypervolume_distance(spHVtotal[[i]], spHVtotal[[j]])
    sp_min.dist.stat[i,j] <- hypervolume_distance(spHVtotal[[i]], spHVtotal[[j]], type = "minimum", check.memory = FALSE)
  }}

#save rownames and column names
rownames<-c("balteatusQ","balteatusW","lapponicusQ","lapponicusW",
            "lucorumQ","lucorumW","monticolaQ","monticolaW",
            "pratorumQ","pratorumW")
colnames<-c("balteatusQ","balteatusW","lapponicusQ","lapponicusW",
            "lucorumQ","lucorumW","monticolaQ","monticolaW",
            "pratorumQ","pratorumW")

#set all analyses to correct row/column names
rownames(sp_overlap.stat)<-rownames
colnames(sp_overlap.stat)<-colnames
rownames(sp_distance.stat)<-rownames
colnames(sp_distance.stat)<-colnames
rownames(sp_min.dist.stat)<-rownames
colnames(sp_min.dist.stat)<-colnames
rownames(sp_jaccard.stat)<-rownames
colnames(sp_jaccard.stat)<-colnames

#create function visualise how similarity/distance varies with species
sp_statvisualise<-function(df){
  df%>%gt()%>%data_color(columns=c(balteatusQ,balteatusW,lapponicusQ,lapponicusW,
                                   lucorumQ,lucorumW,monticolaQ,monticolaW,
                                   pratorumQ,pratorumW),
                         colors=scales::col_numeric(
                           palette = paletteer::paletteer_c(
                             palette = "ggthemes::Red-Gold",30) %>%
                             as.character(),
                           domain = NULL))}


#visualise each stat calculated
sp_statvisualise(sp_overlap.stat)
sp_statvisualise(sp_distance.stat)
sp_statvisualise(sp_min.dist.stat)
sp_statvisualise(sp_jaccard.stat)

# PLOT SPECIES (no year, no high/low)
queens<-hypervolume_join(spHV$balteatusQ,spHV$jonellusQ,spHV$lapponicusQ,spHV$lucorumQ,
                         spHV$monticolaQ,spHV$pascuorumQ,spHV$pratorumQ)
workers<-hypervolume_join(spHV$balteatusW,spHV$jonellusW,spHV$lapponicusW,spHV$lucorumW,
                          spHV$monticolaW,spHV$pascuorumW,spHV$pratorumW)

mfrow3d(1,2,sharedMouse = TRUE)
hvplot_scaled(queens)
#mtext3d("Queens",'z--',outer=TRUE,line=3,cex=1.4,las=1)
hvplot_scaled(workers)
#mtext3d("Workers",'z--',outer=TRUE,line=3,cex=1.4,las=1)
legend3d("topleft",legend=c("balteatus","jonellus","lapponicus",
                            "lucorum","monticola","pascuorum","pratorum"),pch=16,col=colourpalette,cex=1)

### end script ###