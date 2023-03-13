#### HYPERVOLUME DATA PREPARATION ####

#RUN FIRST 

rm(list=ls())

#load required packages
library(tidyr)
library(tidyverse)
library(dplyr)
library(glue)
library(openxlsx)
library(lme4)

#create colour palette
colourpalette <- c( "#CC6677", "#88CCEE","#DDCC77", "#117733", "#332288", "#AA4499", 
                          "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
                          
#create scaled hypervolume plotting function
hvplot_scaled<-function(x){
  plot.HypervolumeList(x,show.3d=TRUE,plot.3d.axes.id=NULL,
                       show.axes=TRUE, show.frame=FALSE,
                       cex.data=10,cex.random=2.5,show.centroid=TRUE,cex.centroid=80,
                       show.density=FALSE,show.data=FALSE,
                       names=NULL, show.legend=FALSE, colors=colourpalette, 
                       show.contour=TRUE, contour.lwd=1.5, 
                       contour.type='kde',add=FALSE,limits=list(c(-4,2),c(-4,4),c(-4,6)))}


#set working directory and load bee data
setwd("C:/Users/rona/OneDrive - Imperial College London/Rona_HV_2022/Data")
bees<-read.xlsx("Working_spreadsheet_BB_observ_18192122_v2.xlsx")

#filter out all non-caught bees
bees<-bees%>%filter(Catch_Y_N=="Y")

#initial visualisation of data
head(bees)
summary(bees)
str(bees)
names(bees)

#remove missing values of itd
bees<-bees%>%drop_na(Intertegular_distance_cm)

#remove any observations with uncertainty on species - keep alpinus/polaris, remove the rest
for (i in 1:length(bees$Bombus_Species)) {
  if(bees$Bombus_Species[i] == "alpinus/polaris") {
    bees$Bombus_Species[i] <- "alpinus_polaris"}}
bees<-bees%>% 
  filter(!grepl("/",Bombus_Species))%>%
  filter(Caste_Assigned_CT!="Unknown")

#remove NA values from altitude column
bees<-bees %>% drop_na(Altitude)
bees$Altitude<-as.numeric(bees$Altitude)
#create new column showing whether each location is above or below treeline (800 m.a.s.l)
bees$highlow<-bees$Altitude
bees$highlow[bees$Altitude>=800]<-"H" 
bees$highlow[bees$Altitude<800]<-"L"

#remove na values for traits and save as new dataset hvbees
hvbees<-bees%>%drop_na(Intertegular_distance_cm)%>%
  drop_na(Wing_length_cm)%>%drop_na(Head_width_cm)%>%
  drop_na(Estimated_prementum_length_cm)%>%
  drop_na(Est_total_tongue_length_cm)

#convert all trait values to numeric
hvbees$Intertegular_distance_cm<-as.numeric(hvbees$Intertegular_distance_cm)
hvbees$Head_width_cm<-as.numeric(hvbees$Head_width_cm)
hvbees$Wing_length_cm<-as.numeric(hvbees$Wing_length_cm)
hvbees$Est_total_tongue_length_cm<-as.numeric(hvbees$Est_total_tongue_length_cm)
hvbees$Estimated_prementum_length_cm<-as.numeric(hvbees$Estimated_prementum_length_cm)

#create df of only trait values
traits<-hvbees%>%select(Intertegular_distance_cm,Head_width_cm,
                        Wing_length_cm,Est_total_tongue_length_cm,
                        Estimated_prementum_length_cm)
#check colinearity of traits
pairs(traits)

#all traits highly colinear - body size strongly correlated with all traits

#get value of each trait independent of ITD
#create lmm of each trait (with caste and species as random factors) then take residuals as new values
#lmm of relationship between itd and head with
headmod<-lmer(Head_width_cm~Intertegular_distance_cm+
                (1|Bombus_Species)+(1|Caste_Assigned_CT),
              data=hvbees)

#extract residuals
head_resid<-resid(headmod)

#add residuals to data as new column
hvbees$head_resid<-NA
hvbees$head_resid[which(rownames(hvbees) %in%
                          names(head_resid))] <- head_resid

#repeat for wing length
#lmm
wingmod<-lmer(Wing_length_cm~Intertegular_distance_cm+
                (1|Bombus_Species)+(1|Caste_Assigned_CT),
              data=hvbees)

#extract residuals
wing_resid<-resid(wingmod)

#add residuals to data as new column
hvbees$wing_resid<-NA
hvbees$wing_resid[which(rownames(hvbees) %in%
                          names(wing_resid))]<-wing_resid

#create PCA of ITD/tongue length
principal.component <- prcomp(hvbees[,c(59,68)], scale = TRUE, center = TRUE)
components <- data.frame(principal.component$x)

#extract trait values, caste and species to new df 
hvbees$pca<-components$PC1
hvbees<-hvbees%>%select(pca,wing_resid,head_resid,Bombus_Species,Caste_Assigned_CT,
                        highlow,Year)

#z-scale all relative traits
z.hvbees<-hvbees%>%mutate(pca=scale(pca),wing_resid=scale(wing_resid),
                          head_resid=scale(head_resid))

#remove drones from df
z.hvbees<-z.hvbees%>%filter(Caste_Assigned_CT!="D")

### end script ###