#### HYPERVOLUME DATA PREPARATION CUT OFF AT JULY ####

#RUN FIRST 

rm(list=ls())

library(tidyr)
library(tidyverse)
library(dplyr)
library(glue)
library(openxlsx)
library(lme4)

#create colour palette
colourstbc <- c( "#CC6677", "#88CCEE","#DDCC77", "#117733", "#332288", "#AA4499", 
                          "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
                          
#create scaled hv plotting function
hvplot_scaled<-function(x){
  plot.HypervolumeList(x,show.3d=TRUE,plot.3d.axes.id=NULL,
                       show.axes=TRUE, show.frame=FALSE,
                       cex.data=10,cex.random=2.5,show.centroid=TRUE,cex.centroid=80,
                       show.density=FALSE,show.data=FALSE,
                       names=NULL, show.legend=FALSE, colors=colourstbc, 
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

#cut off sampling at July 1st
bees<-bees%>%filter(DOY<202)

#remove any observations with uncertainty on species - keep alpinus/polaris, remove the rest
for (i in 1:length(bees$Bombus_Species)) {
  if(bees$Bombus_Species[i] == "alpinus/polaris") {
    bees$Bombus_Species[i] <- "alpinus_polaris"}}
bees<-bees%>% 
  filter(!grepl("/",Bombus_Species))%>%
  filter(Caste_Assigned_CT!="Unknown")


#identify treeline and change number
#remove NA values from altitude column
bees<-bees %>% drop_na(Altitude)
bees$Altitude<-as.numeric(bees$Altitude)
#create new column showing whether each location is above 
#or below the mean altitude
bees$highlow<-bees$Altitude
bees$highlow[bees$Altitude>=800]<-"H" 
bees$highlow[bees$Altitude<800]<-"L"

#remove na values for traits and save as new dataset
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


#repeat for est prementum length
#lmm
premod<-lmer(Estimated_prementum_length_cm~Intertegular_distance_cm+
               (1|Bombus_Species)+(1|Caste_Assigned_CT),
             data=hvbees)

#extract residuals
prementum_resid<-resid(premod)

#add residuals to data as new column
hvbees$prementum_resid<-NA
hvbees$prementum_resid[which(rownames(hvbees) %in%
                               names(prementum_resid))]<-prementum_resid


#create PCA of ITD/tongue length
principal.component <- prcomp(hvbees[,c(59,68)], scale = TRUE, center = TRUE)
components <- data.frame(principal.component$x)

#extract trait values, caste and species to new df - NOTE this should include vegetation zone - new highlows
hvbees$pca<-components$PC1
hvbees<-hvbees%>%select(pca,wing_resid,head_resid,Bombus_Species,Caste_Assigned_CT,
                        highlow,Year)


#add ITD/tongue pca to traits df



#z-scale all relative traits
z.hvbees<-hvbees%>%mutate(pca=scale(pca),wing_resid=scale(wing_resid),
                          head_resid=scale(head_resid),
                          prementum_resid=scale(prementum_resid))


z.hvbees<-z.hvbees%>%filter(Caste_Assigned_CT!="D")


### resampling and volume calculations

#hypervolume_n_resample(name="yearcommunityresamplecutoff",ycHVtotal,n=500,points_per_resample = "120",cores=1,verbose=TRUE)


#create for loop for all groups
names<-c("Q2018","Q2019","Q2021","Q2022","W2018","W2019","W2021","W2022")

#create new hv list for each group
Q2018_rs<-new("HypervolumeList")
Q2019_rs<-new("HypervolumeList")
Q2021_rs<-new("HypervolumeList")
Q2022_rs<-new("HypervolumeList")
W2018_rs<-new("HypervolumeList")
W2019_rs<-new("HypervolumeList")
W2021_rs<-new("HypervolumeList")
W2022_rs<-new("HypervolumeList")

#create loop to fill hv list for each group
for (i in names){
  filepath<-list.files(path=paste("Objects/yearcommunityresamplecutoff/",i,sep=""),
                       pattern="*.rds",all.files=TRUE)
  filenames<-str_remove(filepath,pattern=".rds")
  for (j in filenames){
    filenames<-paste(i,j,sep="")
    test<-readRDS(paste0("Objects/yearcommunityresamplecutoff/",i,"/",j,".rds"))
    if (i == "Q2018"){Q2018_rs<-hypervolume_join(Q2018_rs,test)}
    if (i == "Q2019"){Q2019_rs<-hypervolume_join(Q2019_rs,test)}
    if (i == "Q2021"){Q2021_rs<-hypervolume_join(Q2021_rs,test)}
    if (i == "Q2022"){Q2022_rs<-hypervolume_join(Q2022_rs,test)}
    if (i == "W2018"){W2018_rs<-hypervolume_join(W2018_rs,test)}
    if (i == "W2019"){W2019_rs<-hypervolume_join(W2019_rs,test)}
    if (i == "W2021"){W2021_rs<-hypervolume_join(W2021_rs,test)}
    if (i == "W2022"){W2022_rs<-hypervolume_join(W2022_rs,test)}
  }}

#get_volume creates named vector of volumes for each

Volume<-unname(get_volume(Q2018_rs))
Q18vol<-as.data.frame(Volume)
Q18vol['Year']='2018'
Q18vol['Caste']='Q'
Volume<-unname(get_volume(Q2019_rs))
Q19vol<-as.data.frame(Volume)
Q19vol['Year']='2019'
Q19vol['Caste']='Q'
Volume<-unname(get_volume(Q2021_rs))
Q21vol<-as.data.frame(Volume)
Q21vol['Year']='2021'
Q21vol['Caste']='Q'
Volume<-unname(get_volume(Q2022_rs))
Q22vol<-as.data.frame(Volume)
Q22vol['Year']='2022'
Q22vol['Caste']='Q'
Volume<-unname(get_volume(W2018_rs))
W18vol<-as.data.frame(Volume)
W18vol['Year']='2018'
W18vol['Caste']='W'
Volume<-unname(get_volume(W2019_rs))
W19vol<-as.data.frame(Volume)
W19vol['Year']='2019'
W19vol['Caste']='W'
Volume<-unname(get_volume(W2021_rs))
W21vol<-as.data.frame(Volume)
W21vol['Year']='2021'
W21vol['Caste']='W'
Volume<-unname(get_volume(W2022_rs))
W22vol<-as.data.frame(Volume)
W22vol['Year']='2022'
W22vol['Caste']='W'

#create new variable with all the years for each caste
#create df of all hvs
yearcommvolume<-rbind(Q18vol,Q19vol,Q21vol,Q22vol,
                      W18vol,W19vol,W21vol,W22vol)

yearcommvolumeQ<-yearcommvolume%>%filter(Caste=="Q")
yearcommvolumeW<-yearcommvolume%>%filter(Caste=="W")

#calculate development year by factor and numerical
yearcommvolumeQ<-yearcommvolumeQ%>%mutate(devyear=case_when(
  Year=="2018"~"A.2018",
  Year=="2019"~"D.2019",
  Year=="2021"~"B.2021",
  Year=="2022"~"C.2022"))
yearcommvolumeQ$devyear<-as.factor(yearcommvolumeQ$devyear)

yearcommvolumeQ<-yearcommvolumeQ%>%mutate(devyearc=case_when(
  Year=="2018"~"10.5",
  Year=="2019"~"12.8",
  Year=="2021"~"10.6",
  Year=="2022"~"11.2"))
yearcommvolumeQ$devyearc<-as.numeric(yearcommvolumeQ$devyearc)

yearcommvolumeW<-yearcommvolumeW%>%mutate(devyear=case_when(
  Year=="2018"~"C.2018",
  Year=="2019"~"B.2019",
  Year=="2021"~"D.2021",
  Year=="2022"~"A.2022"))
yearcommvolumeW$devyear<-as.factor(yearcommvolumeW$devyear)

yearcommvolumeW<-yearcommvolumeW%>%mutate(devyearc=case_when(
  Year=="2018"~"6.74",
  Year=="2019"~"6.65",
  Year=="2021"~"8.94",
  Year=="2022"~"6.42"))
yearcommvolumeW$devyearc<-as.numeric(yearcommvolumeW$devyearc)


#create separate models for each caste for effects of devyear
yearcomm_modQ<-lm(Volume~devyear,data=yearcommvolumeQ)
yearcomm_modW<-lm(Volume~devyear,data=yearcommvolumeW)
yearcomm_modQc<-lm(Volume~as.numeric(devyearc),data=yearcommvolumeQ)
yearcomm_modWc<-lm(Volume~as.numeric(devyearc),data=yearcommvolumeW)

par(mfrow=c(2,2))
plot(yearcomm_modQ)
plot(yearcomm_modW)
par(mfrow=c(1,1))

summary(yearcomm_modQ)
summary(yearcomm_modW)
summary(yearcomm_modQc)
summary(yearcomm_modWc)

commvol_aovQ <- aov(Volume~devyear,data=yearcommvolumeQ)
TukeyHSD(commvol_aovQ)
commvol_aovW <- aov(Volume~devyear,data=yearcommvolumeW)
TukeyHSD(commvol_aovW)

#plot queen and worker volume by increasing development temperature
yearcommdevplotQ2<-ggplot(data=yearcommvolumeQ)+geom_boxplot(aes(x=devyear,y=Volume),fill="#CC6677")+
  ggtitle("Queens")+theme_classic()+ylim(7,144)+
  labs(y="Hypervolume volume",x="Year by increasing development temperature")
yearcommdevplotW2<-ggplot(data=yearcommvolumeW)+geom_boxplot(aes(x=devyear,y=Volume),fill="#88CCEE")+
  ggtitle("Workers")+theme_classic()+ylim(7,144)+
  labs(y="Hypervolume volume",x="Year by increasing development temperature")

yearcommdevplots2<-plot_grid(yearcommdevplotQ2,yearcommdevplotW2)











## SPECIES YEAR VOLUME ##

#set sample draw as minimum sample size
#50 bees minimum - this is where the sample size converges in rarefaction

#simulate 1000 hypervolumes per group using bootstrapping

#hypervolume_n_resample(name="yearspeciesresample",spycHVtotal, n=500,points_per_resample = "30",cores=1,verbose=TRUE)
#hypervolume_n_resample(name="yearspeciesresamplecutoff",spycHVtotal, n=500,points_per_resample = "30",cores=1,verbose=TRUE)

#create new hv list for each group

lapponicusQ18_rs<-new("HypervolumeList")
lapponicusQ19_rs<-new("HypervolumeList")
lapponicusQ21_rs<-new("HypervolumeList")
lapponicusQ22_rs<-new("HypervolumeList")
monticolaQ18_rs<-new("HypervolumeList")
monticolaQ19_rs<-new("HypervolumeList")
monticolaQ21_rs<-new("HypervolumeList")
monticolaQ22_rs<-new("HypervolumeList")
pratorumQ18_rs<-new("HypervolumeList")
pratorumQ19_rs<-new("HypervolumeList")
pratorumQ21_rs<-new("HypervolumeList")
pratorumQ22_rs<-new("HypervolumeList")
lapponicusW18_rs<-new("HypervolumeList")
lapponicusW19_rs<-new("HypervolumeList")
lapponicusW21_rs<-new("HypervolumeList")
lapponicusW22_rs<-new("HypervolumeList")
monticolaW18_rs<-new("HypervolumeList")
monticolaW19_rs<-new("HypervolumeList")
monticolaW21_rs<-new("HypervolumeList")
monticolaW22_rs<-new("HypervolumeList")
pratorumW18_rs<-new("HypervolumeList")
pratorumW19_rs<-new("HypervolumeList")
pratorumW21_rs<-new("HypervolumeList")
pratorumW22_rs<-new("HypervolumeList")

names<-c("lapponicusQ2018","lapponicusQ2019","lapponicusQ2021","lapponicusQ2022",
         "lapponicusW2018","lapponicusW2019","lapponicusW2021","lapponicusW2022",
         "monticolaQ2018","monticolaQ2019","monticolaQ2021","monticolaQ2022",
         "monticolaW2018","monticolaW2019","monticolaW2021","monticolaW2022",
         "pratorumQ2018","pratorumQ2019","pratorumQ2021","pratorumQ2022",
         "pratorumW2018","pratorumW2019","pratorumW2021","pratorumW2022")

#create loop to fill hv list for each group
for (i in names){
  filepath<-list.files(path=paste("Objects/yearspeciesresamplecutoff/",i,sep=""),
                       pattern="*.rds",all.files=TRUE)
  filenames<-str_remove(filepath,pattern=".rds")
  for (j in filenames){
    filenames<-paste(i,j,sep="")
    test<-readRDS(paste0("Objects/yearspeciesresamplecutoff/",i,"/",j,".rds"))
    if (i == "lapponicusQ2018"){lapponicusQ18_rs<-hypervolume_join(lapponicusQ18_rs,test)}
    if (i == "lapponicusQ2019"){lapponicusQ19_rs<-hypervolume_join(lapponicusQ19_rs,test)}
    if (i == "lapponicusQ2021"){lapponicusQ21_rs<-hypervolume_join(lapponicusQ21_rs,test)}
    if (i == "lapponicusQ2022"){lapponicusQ22_rs<-hypervolume_join(lapponicusQ22_rs,test)}
    if (i == "monticolaQ2018"){monticolaQ18_rs<-hypervolume_join(monticolaQ18_rs,test)}
    if (i == "monticolaQ2019"){monticolaQ19_rs<-hypervolume_join(monticolaQ19_rs,test)}
    if (i == "monticolaQ2021"){monticolaQ21_rs<-hypervolume_join(monticolaQ21_rs,test)}
    if (i == "monticolaQ2022"){monticolaQ22_rs<-hypervolume_join(monticolaQ22_rs,test)}
    if (i == "pratorumQ2018"){pratorumQ18_rs<-hypervolume_join(pratorumQ18_rs,test)}
    if (i == "pratorumQ2019"){pratorumQ19_rs<-hypervolume_join(pratorumQ19_rs,test)}
    if (i == "pratorumQ2021"){pratorumQ21_rs<-hypervolume_join(pratorumQ21_rs,test)}
    if (i == "pratorumQ2022"){pratorumQ22_rs<-hypervolume_join(pratorumQ22_rs,test)}
    if (i == "lapponicusW2018"){lapponicusW18_rs<-hypervolume_join(lapponicusW18_rs,test)}
    if (i == "lapponicusW2019"){lapponicusW19_rs<-hypervolume_join(lapponicusW19_rs,test)}
    if (i == "lapponicusW2021"){lapponicusW21_rs<-hypervolume_join(lapponicusW21_rs,test)}
    if (i == "lapponicusW2022"){lapponicusW22_rs<-hypervolume_join(lapponicusW22_rs,test)}
    if (i == "monticolaW2018"){monticolaW18_rs<-hypervolume_join(monticolaW18_rs,test)}
    if (i == "monticolaW2019"){monticolaW19_rs<-hypervolume_join(monticolaW19_rs,test)}
    if (i == "monticolaW2021"){monticolaW21_rs<-hypervolume_join(monticolaW21_rs,test)}
    if (i == "monticolaW2022"){monticolaW22_rs<-hypervolume_join(monticolaW22_rs,test)}
    if (i == "pratorumW2018"){pratorumW18_rs<-hypervolume_join(pratorumW18_rs,test)}
    if (i == "pratorumW2019"){pratorumW19_rs<-hypervolume_join(pratorumW19_rs,test)}
    if (i == "pratorumW2021"){pratorumW21_rs<-hypervolume_join(pratorumW21_rs,test)}
    if (i == "pratorumW2022"){pratorumW22_rs<-hypervolume_join(pratorumW22_rs,test)}
  }
}

#get_volume creates named vector of volumes for each
Volume<-unname(get_volume(lapponicusQ18_rs))
lapponicusQ2018v<-as.data.frame(Volume)
lapponicusQ2018v['Year']='2018'
lapponicusQ2018v['Caste']='Q'
lapponicusQ2018v['Species']='lapponicus'

Volume<-unname(get_volume(lapponicusQ19_rs))
lapponicusQ2019v<-as.data.frame(Volume)
lapponicusQ2019v['Year']='2019'
lapponicusQ2019v['Caste']='Q'
lapponicusQ2019v['Species']='lapponicus'

Volume<-unname(get_volume(lapponicusQ21_rs))
lapponicusQ2021v<-as.data.frame(Volume)
lapponicusQ2021v['Year']='2021'
lapponicusQ2021v['Caste']='Q'
lapponicusQ2021v['Species']='lapponicus'

Volume<-unname(get_volume(lapponicusQ22_rs))
lapponicusQ2022v<-as.data.frame(Volume)
lapponicusQ2022v['Year']='2022'
lapponicusQ2022v['Caste']='Q'
lapponicusQ2022v['Species']='lapponicus'

monticolaQ2018v<-as.data.frame(Volume)
monticolaQ2018v['Year']='2018'
monticolaQ2018v['Caste']='Q'
monticolaQ2018v['Species']='monticola'

Volume<-unname(get_volume(monticolaQ19_rs))
monticolaQ2019v<-as.data.frame(Volume)
monticolaQ2019v['Year']='2019'
monticolaQ2019v['Caste']='Q'
monticolaQ2019v['Species']='monticola'

Volume<-unname(get_volume(monticolaQ21_rs))
monticolaQ2021v<-as.data.frame(Volume)
monticolaQ2021v['Year']='2021'
monticolaQ2021v['Caste']='Q'
monticolaQ2021v['Species']='monticola'

Volume<-unname(get_volume(monticolaQ22_rs))
monticolaQ2022v<-as.data.frame(Volume)
monticolaQ2022v['Year']='2022'
monticolaQ2022v['Caste']='Q'
monticolaQ2022v['Species']='monticola'

pratorumQ2018v<-as.data.frame(Volume)
pratorumQ2018v['Year']='2018'
pratorumQ2018v['Caste']='Q'
pratorumQ2018v['Species']='pratorum'

Volume<-unname(get_volume(pratorumQ19_rs))
pratorumQ2019v<-as.data.frame(Volume)
pratorumQ2019v['Year']='2019'
pratorumQ2019v['Caste']='Q'
pratorumQ2019v['Species']='pratorum'

Volume<-unname(get_volume(pratorumQ21_rs))
pratorumQ2021v<-as.data.frame(Volume)
pratorumQ2021v['Year']='2021'
pratorumQ2021v['Caste']='Q'
pratorumQ2021v['Species']='pratorum'

Volume<-unname(get_volume(pratorumQ21_rs))
pratorumQ2022v<-as.data.frame(Volume)
pratorumQ2022v['Year']='2022'
pratorumQ2022v['Caste']='Q'
pratorumQ2022v['Species']='pratorum'


#get_volume creates named vector of volumes for each
Volume<-unname(get_volume(lapponicusW18_rs))
lapponicusW2018v<-as.data.frame(Volume)
lapponicusW2018v['Year']='2018'
lapponicusW2018v['Caste']='W'
lapponicusW2018v['Species']='lapponicus'

Volume<-unname(get_volume(lapponicusW19_rs))
lapponicusW2019v<-as.data.frame(Volume)
lapponicusW2019v['Year']='2019'
lapponicusW2019v['Caste']='W'
lapponicusW2019v['Species']='lapponicus'

Volume<-unname(get_volume(lapponicusW21_rs))
lapponicusW2021v<-as.data.frame(Volume)
lapponicusW2021v['Year']='2021'
lapponicusW2021v['Caste']='W'
lapponicusW2021v['Species']='lapponicus'

Volume<-unname(get_volume(lapponicusW22_rs))
lapponicusW2022v<-as.data.frame(Volume)
lapponicusW2022v['Year']='2022'
lapponicusW2022v['Caste']='W'
lapponicusW2022v['Species']='lapponicus'

monticolaW2018v<-as.data.frame(Volume)
monticolaW2018v['Year']='2018'
monticolaW2018v['Caste']='W'
monticolaW2018v['Species']='monticola'

Volume<-unname(get_volume(monticolaW19_rs))
monticolaW2019v<-as.data.frame(Volume)
monticolaW2019v['Year']='2019'
monticolaW2019v['Caste']='W'
monticolaW2019v['Species']='monticola'

Volume<-unname(get_volume(monticolaW21_rs))
monticolaW2021v<-as.data.frame(Volume)
monticolaW2021v['Year']='2021'
monticolaW2021v['Caste']='W'
monticolaW2021v['Species']='monticola'

Volume<-unname(get_volume(monticolaW22_rs))
monticolaW2022v<-as.data.frame(Volume)
monticolaW2022v['Year']='2022'
monticolaW2022v['Caste']='W'
monticolaW2022v['Species']='monticola'

pratorumW2018v<-as.data.frame(Volume)
pratorumW2018v['Year']='2018'
pratorumW2018v['Caste']='W'
pratorumW2018v['Species']='pratorum'

Volume<-unname(get_volume(pratorumW19_rs))
pratorumW2019v<-as.data.frame(Volume)
pratorumW2019v['Year']='2019'
pratorumW2019v['Caste']='W'
pratorumW2019v['Species']='pratorum'

Volume<-unname(get_volume(pratorumW21_rs))
pratorumW2021v<-as.data.frame(Volume)
pratorumW2021v['Year']='2021'
pratorumW2021v['Caste']='W'
pratorumW2021v['Species']='pratorum'

Volume<-unname(get_volume(pratorumW21_rs))
pratorumW2022v<-as.data.frame(Volume)
pratorumW2022v['Year']='2022'
pratorumW2022v['Caste']='W'
pratorumW2022v['Species']='pratorum'


#create df of all hvs
yearspeciesvolume<-rbind(lapponicusQ2018v,lapponicusQ2019v,lapponicusQ2021v,lapponicusQ2022v,
                         lapponicusW2018v,lapponicusW2019v,lapponicusW2021v,lapponicusW2022v,
                         monticolaQ2018v,monticolaQ2019v,monticolaQ2021v,monticolaQ2022v,
                         monticolaW2018v,monticolaW2019v,monticolaW2021v,monticolaW2022v,
                         pratorumQ2018v,pratorumQ2019v,pratorumQ2021v,pratorumQ2022v,
                         pratorumW2018v,pratorumW2019v,pratorumW2021v,pratorumW2022v)

yearspeciesvolumeQ<-yearspeciesvolume%>%filter(Caste=="Q")
yearspeciesvolumeW<-yearspeciesvolume%>%filter(Caste=="W")

## LINEAR MODEL with SPECIES as fixed effect ##

yearspeciesvolumeQ<-yearspeciesvolumeQ%>%mutate(devyear=case_when(
  Year=="2018"~"A.2018",
  Year=="2019"~"D.2019",
  Year=="2021"~"B.2021",
  Year=="2022"~"C.2022"))
yearspeciesvolumeQ$devyear<-as.factor(yearspeciesvolumeQ$devyear)

yearspeciesvolumeQ<-yearspeciesvolumeQ%>%mutate(devyearc=case_when(
  Year=="2018"~"10.5",
  Year=="2019"~"12.8",
  Year=="2021"~"10.6",
  Year=="2022"~"11.2"))
yearspeciesvolumeW$devyear<-as.numeric(yearspeciesvolumeW$devyear)


yearspeciesvolumeW<-yearspeciesvolumeW%>%mutate(devyear=case_when(
  Year=="2018"~"C.2018",
  Year=="2019"~"B.2019",
  Year=="2021"~"D.2021",
  Year=="2022"~"A.2022"))
yearspeciesvolumeW$devyear<-as.factor(yearspeciesvolumeW$devyear)

yearspeciesvolumeW<-yearspeciesvolumeQ%>%mutate(devyearc=case_when(
  Year=="2018"~"6.74",
  Year=="2019"~"6.65",
  Year=="2021"~"8.94",
  Year=="2022"~"6.42"))
yearspeciesvolumeW$devyearc<-as.numeric(yearspeciesvolumeW$devyearc)

#create separate models for each caste for effects of devyear
yearsp_modQ<-lm(Volume~devyear*Species,data=yearspeciesvolumeQ)
yearsp_modW<-lm(Volume~devyear*Species,data=yearspeciesvolumeW)

#and models with devyear as a continuous
yearsp_modQc<-lm(Volume~as.numeric(devyearc)*Species,data=yearspeciesvolumeQ)
yearsp_modWc<-lm(Volume~as.numeric(devyearc)*Species,data=yearspeciesvolumeW)


par(mfrow=c(2,2))
plot(yearsp_modQ)
plot(yearsp_modW)
par(mfrow=c(1,1))

summary(yearsp_modQ)
summary(yearsp_modW)
summary(yearsp_modQc)
summary(yearsp_modWc)

spvol_aovQ <- aov(Volume~devyear*Species,data=yearspeciesvolumeQ)
TukeyHSD(spvol_aovQ)
spvol_aovW <- aov(Volume~devyear*Species,data=yearspeciesvolumeW)
TukeyHSD(spvol_aovW)


#plot queen and worker volume by increasing development temperature
yearspdevplotQ<-ggplot(data=yearspeciesvolumeQ)+geom_boxplot(aes(devyear,Volume,fill=Species))+
  theme_classic()+ggtitle("Queens")+ 
  scale_fill_manual(values=c("#CC6677", "#88CCEE","#DDCC77"))+ylim(4,48)+
  theme_classic()+labs(y="Hypervolume volume",x="Year by increasing development temperature")
yearspdevplotW<-ggplot(data=yearspeciesvolumeW)+geom_boxplot(aes(devyear,Volume,fill=Species))+
  theme_classic()+ggtitle("Workers")+ 
  scale_fill_manual(values=c("#CC6677", "#88CCEE","#DDCC77"))+ylim(0,48)+
  theme_classic()+labs(y="Hypervolume volume",x="Year by increasing development temperature")

yearspdevplots<-ggarrange(yearspdevplotQ,yearspdevplotW,legend="right",common.legend=T)


