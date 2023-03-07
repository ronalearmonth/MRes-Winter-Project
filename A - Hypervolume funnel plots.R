### HYPERVOLUME FUNNEL PLOTS ###

#use rarefaction to determine minimum sample size

library(tidyr)
library(dplyr)
library(cowplot)
library(ggplot2)
library(tidyverse)
library(hypervolume)

#run data preparation first
setwd("C:/Users/rona/OneDrive - Imperial College London/Rona_HV_2022/Data")

#separate by caste
qbees<-z.hvbees%>%filter(Caste_Assigned_CT=="Q")%>%
  select(pca,head_resid,wing_resid)
wbees<-z.hvbees%>%filter(Caste_Assigned_CT=="W")%>%
  select(pca,head_resid,wing_resid)

#queen funnel plot
qfunnel=hypervolume(qbees,verbose=FALSE)
resample_seq_pathq=
  hypervolume_resample("funnel_hvq",qfunnel,method="bootstrap seq",
                       n=30,seq=c(10,20,30,40,50,60,70,90,110,130,150),cores=20)
qfunnelplot<-hypervolume_funnel(resample_seq_pathq,title="Volume of queen hypervolumes at different resample sizes")+
  ylab("Volume")

#worker funnel plot
wfunnel=hypervolume(wbees,verbose=FALSE)
resample_seq_pathw=
  hypervolume_resample("funnel_hvw",wfunnel,method="bootstrap seq",
                       n=30,seq=c(10,30,50,70,90,110,130,150,170,190),cores=20)
wfunnelplot<-hypervolume_funnel(resample_seq_pathw,title="Volume of worker hypervolumes at different resample sizes")+
  ylab("Volume")



