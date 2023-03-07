#### INTERACTION NETWORKS FOR WHOLE COMMUNITY ####


rm(list=ls())
library(dplyr)
library(tidyr)
library(ggpubr)
library(dismo)
library(geometry)
library(ggplot2)
library(factoextra)
library(stringr)
library(vegan)
library(ggrepel)
library(MASS)
library(viridis)
library(bipartite)
library(openxlsx)
library(igraph)
library(ggthemes)
library(gt)
library(devtools)
library(bootstrapnet)
library(cowplot)

#set working directory and load bee data
setwd("C:/Users/rona/OneDrive - Imperial College London/Rona_HV_2022/Data")
bees<-read.xlsx("Working_spreadsheet_BB_observ_18192122_v2.xlsx")

#combine alpinus/polaris as one species
bees$Bombus_Species[bees$Bombus_Species == "alpinus/polaris"] <- "alpinus_polaris"

#remove NAs and unknowns
bees<-bees%>% 
  filter(!grepl("/",Bombus_Species))%>%
  filter(Caste_Assigned_CT!="Unknown")%>%
  filter(Bombus_Species!="Unknown"&Corrected_Flower_Species_ID!="Unknown"&
           Corrected_Flower_Species_ID!="<NA>")

#change any salix species to genus salix
bees$Corrected_Flower_Species_ID[grepl("Salix",bees$Corrected_Flower_Species_ID)]<-"Salix"

#split bees by year and caste
bees<-bees%>%filter(Caste_Assigned_CT=="Q"|Caste_Assigned_CT=="W")
networkbees=split(bees,paste(bees$Caste_Assigned_CT,bees$Year,sep=""))

#create function to select our correct data
selectinteractiondata<-function(x){
  x<-subset(x,select=c(Bombus_Species,Corrected_Flower_Species_ID))
}
#apply function to list
networkbees<-lapply(networkbees,selectinteractiondata)


#separate queens and workers 
#create split dataframes for queens and workers
Qbees<-bees%>%filter(Caste_Assigned_CT=="Q")
networkbeesQ=split(Qbees,paste(Qbees$Caste_Assigned_CT,Qbees$Year,sep=""))

Wbees<-bees%>%filter(Caste_Assigned_CT=="W")
networkbeesW=split(Wbees,paste(Wbees$Caste_Assigned_CT,Wbees$Year,sep=""))

#apply function to list
networkbeesQ<-lapply(networkbeesQ,selectinteractiondata)
networkbeesW<-lapply(networkbeesW,selectinteractiondata)

#create function to convert to matrix/web
make_matrix<-function(x){
  same_group <- numeric(nrow(x))
  
  #join blank column onto dataset
  bipartite.cols <- cbind(x, same_group)
  
  ## count number of interactions between each bee species and each plant species
  x <- frame2webs(bipartite.cols,varnames = c("Bombus_Species", "Corrected_Flower_Species_ID", "same_group"))
  
  x <- x[[1]]
  print(x)}

#apply function to Q and W lists
networkbeeswebsQ<-lapply(networkbeesQ,make_matrix)
networkbeeswebsW<-lapply(networkbeesW,make_matrix)



## GREATE FUNCTION TO GENERATE NETWORKS AND ANALYSES FOR EACH YEAR/CASTE COMBINATION ##
make_qnetwork<-function(x){
  same_group <- numeric(nrow(x))
  
  #join blank column onto dataset
  bipartite.cols <- cbind(x, same_group)
  
  ## count number of interactions between each bee species and each plant species
  bipartite.web <- frame2webs(bipartite.cols, 
                              varnames = c("Bombus_Species", "Corrected_Flower_Species_ID", "same_group"))
  bipartite.web <- bipartite.web[[1]]
  
  dpar<-par
  par(font = 3)
  plotweb(bipartite.web, col.interaction = "#CC6677",
          col.low=ifelse(rownames(bipartite.web)==
                           "pratorum","grey",
                         ifelse(rownames(bipartite.web)=="lapponicus",
                                "grey",
                                ifelse(rownames(bipartite.web)==
                                         "monticola",
                                       "grey","black"))),
          text.rot = 90, 
          labsize = 1.2, ybig = 1.2)
  par(dpar)
  #visweb(bipartite.web)
  
  #analyses 
  networklevel(bipartite.web,index=c("ALLBUTDD"),
               level="both")
}

#make network for each year for queens
#par(mfrow=c(1,4))
Q18<-make_qnetwork(networkbees$Q2018)
title("Queens 2018")
Q19<-make_qnetwork(networkbees$Q2019)
title("Queens 2019")
Q21<-make_qnetwork(networkbees$Q2021)
title("Queens 2021")
Q22<-make_qnetwork(networkbees$Q2022)
title("Queens 2022")

#convert to rows
Q18temp<-c(t(unname(Q18)))
Q19temp<-c(t(unname(Q19)))
Q21temp<-c(t(unname(Q21)))
Q22temp<-c(t(unname(Q22)))


#repeat but make worker networks blue to distinguish
make_wnetwork<-function(x){
  same_group <- numeric(nrow(x))
  
  #join blank column onto dataset
  bipartite.cols <- cbind(x, same_group)
  
  ## count number of interactions between each bee species and each plant species
  bipartite.web <- frame2webs(bipartite.cols, 
                              varnames = c("Bombus_Species", "Corrected_Flower_Species_ID", "same_group"))
  bipartite.web <- bipartite.web[[1]]
  
  dpar<-par
  par(font = 3)
  plotweb(bipartite.web, 
          col.interaction="#88CCEE", 
  col.low=ifelse(rownames(bipartite.web)==
                       "pratorum","grey",
                 ifelse(rownames(bipartite.web)=="lapponicus",
                        "grey",
                        ifelse(rownames(bipartite.web)==
                                 "monticola",
                               "grey","black"))),
  text.rot = 90,labsize = 1.2, ybig = 1.2)
  par(dpar)
  #visweb(bipartite.web)
  
  #analyses 
  networklevel(bipartite.web,index=c("ALLBUTDD"),
               level="both")
}

#apply function to each group
w18<-make_wnetwork(networkbees$W2018)
title("Workers 2018")
w19<-make_wnetwork(networkbees$W2019)
title("Workers 2019")
w21<-make_wnetwork(networkbees$W2021)
title("Workers 2021")
w22<-make_wnetwork(networkbees$W2022)
title("Workers 2022")
par(mfrow=c(1,1))

#convert to rows
w18temp<-c(t(unname(w18)))
w19temp<-c(t(unname(w19)))
w21temp<-c(t(unname(w21)))
w22temp<-c(t(unname(w22)))

#join queen and worker rows to create new df of interaction network metrics
indices<-as.data.frame(rbind(Q18temp,Q19temp,Q21temp,Q22temp,
                             w18temp,w19temp,w21temp,w22temp))
colnames(indices)<-c("H2","connectance","interaction.nestedness")
indices$year<-c("2018","2019","2021","2022","2018","2019","2021","2022")
indices$caste<-c("Q","Q","Q","Q","W","W","W","W")


#quick look at stats so far to get an idea of patterns
indicesQ<-indices%>%filter(caste=="Q")
summary(indicesQ)
indicesW<-indices%>%filter(caste=="W")
summary(indicesW)




## BOOTSTRAPPING H2 FOR EACH NETWORK ##

bootstrappedH2<-function(web1,web2,web3,web4){
  H2bootstrap=list(mat1=web1,mat2=web2,mat3=web3,mat4=web4)%>%
    lapply(web_matrix_to_df)%>%
    boot_networklevel(col_lower = "lower", # column name for plants
                      col_higher = "higher", # column name for insects
                      level = "both",
                      index = "H2", # select network metric
                      start = 20,
                      step = 10,
                      n_boot = 1000,  # number of bootstraps
                      n_cpu = 4)
}

queensH2<-bootstrappedH2(networkbeeswebsQ$Q2018,networkbeeswebsQ$Q2019,
                         networkbeeswebsQ$Q2021,networkbeeswebsQ$Q2022)

#queenH2_plot<- gg_networklevel(queensH2)
#queenH2_plot$H2 + theme_classic() + 
 # xlab("Sample size") + scale_color_discrete(c("#CC6677", "#88CCEE","#DDCC77", "#117733"),labels=c('2018', '2019','2021','2022'))+
  #ylab("H2") +theme(legend.title=element_blank())+ggtitle("Queens")

#bootstrap H2 for sp worker networks
workersH2<-bootstrappedH2(networkbeeswebsW$W2018,networkbeeswebsW$W2019,
                          networkbeeswebsW$W2021,networkbeeswebsW$W2022)

#workerH2_plot<- gg_networklevel(workersH2)
#workerH2_plot$H2 + theme_classic() + 
  #xlab("Sample size") + scale_color_discrete(c("#CC6677", "#88CCEE","#DDCC77", "#117733"),labels=c('2018', '2019','2021','2022'))+
  #ylab("H2") +theme(legend.title=element_blank())+ggtitle("Workers")


#now need to run stats on H2 for each caste between years

#add row for year to queen stats df
queensH2$H2$stats_df$year<-queensH2$H2$stats_df$web
queensH2$H2$stats_df$year[queensH2$H2$stats_df$year=="mat1"]<-"2018" 
queensH2$H2$stats_df$year[queensH2$H2$stats_df$year=="mat2"]<-"2019" 
queensH2$H2$stats_df$year[queensH2$H2$stats_df$year=="mat3"]<-"2021" 
queensH2$H2$stats_df$year[queensH2$H2$stats_df$year=="mat4"]<-"2022" 
queensH2$H2$stats_df$year<-as.factor(queensH2$H2$stats_df$year)

#repeat for workers
workersH2$H2$stats_df$year<-workersH2$H2$stats_df$web
workersH2$H2$stats_df$year[workersH2$H2$stats_df$year=="mat1"]<-"2018" 
workersH2$H2$stats_df$year[workersH2$H2$stats_df$year=="mat2"]<-"2019" 
workersH2$H2$stats_df$year[workersH2$H2$stats_df$year=="mat3"]<-"2021" 
workersH2$H2$stats_df$year[workersH2$H2$stats_df$year=="mat4"]<-"2022" 
workersH2$H2$stats_df$year<-as.factor(workersH2$H2$stats_df$year)

#create combined df of both castes
workersH2$H2$stats_df["caste"]='W'
queensH2$H2$stats_df["caste"]='Q'
beesH2<-rbind(workersH2$H2$stats_df,queensH2$H2$stats_df)

#add column for increasing devtemp
devqueensH2<-queensH2$H2$stats_df%>%mutate(devyear=case_when(
  year=="2018"~"A.2018",
  year=="2019"~"D.2019",
  year=="2021"~"B.2021",
  year=="2022"~"C.2022"))
devqueensH2$devyear<-as.factor(devqueensH2$devyear)

devqueensH2<-devqueensH2%>%mutate(devyearc=case_when(
  year=="2018"~"10.5",
  year=="2019"~"12.8",
  year=="2021"~"10.6",
  year=="2022"~"11.2"))
devqueensH2$devyearc<-as.numeric(devqueensH2$devyearc)


devworkersH2<-workersH2$H2$stats_df%>%mutate(devyear=case_when(
  year=="2018"~"C.2018",
  year=="2019"~"B.2019",
  year=="2021"~"D.2021",
  year=="2022"~"A.2022"))
devworkersH2$devyear<-as.factor(devworkersH2$devyear)

devworkersH2<-devworkersH2%>%mutate(devyearc=case_when(
  year=="2018"~"6.74",
  year=="2019"~"6.65",
  year=="2021"~"8.94",
  year=="2022"~"6.42"))
devworkersH2$devyearc<-as.numeric(devworkersH2$devyearc)


#run linear model of mean H2 by year for each caste
H2q_moddev<-lm(mean~devyear,data=devqueensH2)
H2w_moddev<-lm(mean~devyear,data=devworkersH2)
H2q_moddevc<-lm(mean~devyearc,data=devqueensH2)
H2w_moddevc<-lm(mean~devyearc,data=devworkersH2)

#check assumptions - slightly wobbly
par(mfrow=c(2,2))
plot(H2q_moddev)
plot(H2w_moddev)
par(mfrow=c(1,1))

summary(H2q_moddev)
summary(H2w_moddev)
summary(H2q_moddevc)
summary(H2w_moddevc)

#use Tukey test to get year comparisons
H2q_aovmod <- aov(mean~year,data=queensH2$H2$stats_df)
TukeyHSD(H2q_aovmod)

H2w_aovmod <- aov(mean~year,data=workersH2$H2$stats_df)
TukeyHSD(H2w_aovmod)

#plot queen and worker H2 by increasing development temperature
H2qplotdev<-ggplot(data=devqueensH2)+geom_boxplot(aes(x=devyear,y=mean),fill="#CC6677")+
  ggtitle("Queens H2")+theme_classic()+theme(legend.position="none")+
  labs(x="Years by increasing development temperature",y="Bootstrapped mean H2")+
  ylim(0.12,0.52) 

H2wplotdev<-ggplot(data=devworkersH2)+geom_boxplot(aes(x=devyear,y=mean),fill="#88CCEE")+
  ggtitle("Workers H2")+theme_classic()+theme(legend.position="none")+
  labs(x="Years by increasing development temperature",y="Bootstrapped mean H2")+
  ylim(0.12,0.52)

#plot both together
H2devbyyear<-plot_grid(H2qplotdev,H2wplotdev)





## BOOTSTRAPPING CONNECTANCE FOR EACH NETWORK ##

bootstrappedconnectance<-function(web1,web2,web3,web4){
  connectancebootstrap=list(mat1=web1,mat2=web2,mat3=web3,mat4=web4)%>%
    lapply(web_matrix_to_df)%>%
    boot_networklevel(col_lower = "lower", # column name for plants
                      col_higher = "higher", # column name for insects
                      level = "both",
                      index = "connectance", # select network metric
                      start = 20,
                      step = 10,
                      n_boot = 1000,  # number of bootstraps
                      n_cpu = 4)
}

queenscon<-bootstrappedconnectance(networkbeeswebsQ$Q2018,networkbeeswebsQ$Q2019,
                                   networkbeeswebsQ$Q2021,networkbeeswebsQ$Q2022)

#queenconnectance_plot<- gg_networklevel(queenscon)
#queenconnectance_plot$connectance + theme_classic() + 
 # xlab("Sample size") + scale_color_discrete(c("#CC6677", "#88CCEE","#DDCC77", "#117733"),labels=c('2018', '2019','2021','2022'))+
  #ylab("Connectance") +theme(legend.title=element_blank())+ggtitle("Queens")

#bootstrap H2 for worker networks
workerscon<-bootstrappedconnectance(networkbeeswebsW$W2018,networkbeeswebsW$W2019,
                                    networkbeeswebsW$W2021,networkbeeswebsW$W2022)

#workerconnectance_plot<- gg_networklevel(workerscon)
#workerconnectance_plot$connectance + theme_classic() + 
  #xlab("Sample size") + scale_color_discrete(c("#CC6677", "#88CCEE","#DDCC77", "#117733"),labels=c('2018', '2019','2021','2022'))+
  #ylab("Connectance") +theme(legend.title=element_blank())+ggtitle("Workers")


#now need to run stats on connectance for each caste between years

#add row for year to queen stats df
queenscon$connectance$stats_df$year<-queenscon$connectance$stats_df$web
queenscon$connectance$stats_df$year[queenscon$connectance$stats_df$year=="mat1"]<-"2018" 
queenscon$connectance$stats_df$year[queenscon$connectance$stats_df$year=="mat2"]<-"2019" 
queenscon$connectance$stats_df$year[queenscon$connectance$stats_df$year=="mat3"]<-"2021" 
queenscon$connectance$stats_df$year[queenscon$connectance$stats_df$year=="mat4"]<-"2022" 
queenscon$connectance$stats_df$year<-as.factor(queenscon$connectance$stats_df$year)

#repeat for workers
workerscon$connectance$stats_df$year<-workerscon$connectance$stats_df$web
workerscon$connectance$stats_df$year[workerscon$connectance$stats_df$year=="mat1"]<-"2018" 
workerscon$connectance$stats_df$year[workerscon$connectance$stats_df$year=="mat2"]<-"2019" 
workerscon$connectance$stats_df$year[workerscon$connectance$stats_df$year=="mat3"]<-"2021" 
workerscon$connectance$stats_df$year[workerscon$connectance$stats_df$year=="mat4"]<-"2022" 
workerscon$connectance$stats_df$year<-as.factor(workerscon$connectance$stats_df$year)

#add column for increasing devtemp
devqueenscon<-queenscon$con$stats_df%>%mutate(devyear=case_when(
  year=="2018"~"A.2018",
  year=="2019"~"D.2019",
  year=="2021"~"B.2021",
  year=="2022"~"C.2022"))
devqueenscon$devyear<-as.factor(devqueenscon$devyear)

devqueenscon<-devqueenscon%>%mutate(devyearc=case_when(
  year=="2018"~"10.5",
  year=="2019"~"12.8",
  year=="2021"~"10.6",
  year=="2022"~"11.2"))
devqueenscon$devyearc<-as.numeric(devqueenscon$devyearc)


devworkerscon<-workerscon$con$stats_df%>%mutate(devyear=case_when(
  year=="2018"~"C.2018",
  year=="2019"~"B.2019",
  year=="2021"~"D.2021",
  year=="2022"~"A.2022"))
devworkerscon$devyear<-as.factor(devworkerscon$devyear)

devworkerscon<-devworkerscon%>%mutate(devyearc=case_when(
  year=="2018"~"6.74",
  year=="2019"~"6.65",
  year=="2021"~"8.94",
  year=="2022"~"6.42"))
devworkerscon$devyearc<-as.numeric(devworkerscon$devyearc)


#run linear model of mean con by year for each caste
conq_moddev<-lm(mean~devyear,data=devqueenscon)
conw_moddev<-lm(mean~devyear,data=devworkerscon)
conq_moddevc<-lm(mean~devyearc,data=devqueenscon)
conw_moddevc<-lm(mean~devyearc,data=devworkerscon)

#check assumptions - slightly wobbly
par(mfrow=c(2,2))
plot(conq_moddev)
plot(conw_moddev)
par(mfrow=c(1,1))

summary(conq_moddev)
summary(conw_moddev)
summary(conq_moddevc)
summary(conw_moddevc)

#use Tukey test to get year comparisons
conq_aovmod <- aov(mean~year,data=queenscon$con$stats_df)
TukeyHSD(conq_aovmod)

conw_aovmod <- aov(mean~year,data=workerscon$con$stats_df)
TukeyHSD(conw_aovmod)

#plot queen and worker con by increasing development temperature
conqplotdev<-ggplot(data=devqueenscon)+geom_boxplot(aes(x=devyear,y=mean),fill="#CC6677")+
  ggtitle("Queens connectance")+theme_classic()+theme(legend.position="none")+
  labs(x="Years by increasing development temperature",y="Bootstrapped mean connectance")+
  ylim(0.23,0.38) 

conwplotdev<-ggplot(data=devworkerscon)+geom_boxplot(aes(x=devyear,y=mean),fill="#88CCEE")+
  ggtitle("Workers connectance")+theme_classic()+theme(legend.position="none")+
  labs(x="Years by increasing development temperature",y="Bootstrapped mean connectance")+
  ylim(0.23,0.38) 

condevbyyear<-plot_grid(conqplotdev,conwplotdev)



## BOOTSTRAPPING INTERACTION EVENNESS FOR EACH NETWORK ##

bootstrappedevenness<-function(web1,web2,web3,web4){
  evennessbootstrap=list(mat1=web1,mat2=web2,mat3=web3,mat4=web4)%>%
    lapply(web_matrix_to_df)%>%
    boot_networklevel(col_lower = "lower", # column name for plants
                      col_higher = "higher", # column name for insects
                      level = "both",
                      index = "interaction evenness", # select network metric
                      start = 20,
                      step = 10,
                      n_boot = 1000,  # number of bootstraps
                      n_cpu = 4)
}

queenseven<-bootstrappedevenness(networkbeeswebsQ$Q2018,networkbeeswebsQ$Q2019,
                                 networkbeeswebsQ$Q2021,networkbeeswebsQ$Q2022)
queenseven$evenness<-queenseven$`interaction evenness`


#queenevenness_plot<- gg_networklevel(queenseven)
#queenevenness_plot$evenness + theme_classic() + 
 # xlab("Sample size") + scale_color_discrete(c("#CC6677", "#88CCEE","#DDCC77", "#117733"),labels=c('2018', '2019','2021','2022'))+
  #ylab("Evenness") +theme(legend.title=element_blank())+ggtitle("Queens")

#bootstrap evenness for worker networks
workerseven<-bootstrappedevenness(networkbeeswebsW$W2018,networkbeeswebsW$W2019,
                                  networkbeeswebsW$W2021,networkbeeswebsW$W2022)
workerseven$evenness<-workerseven$`interaction evenness`

#workerevenness_plot<- gg_networklevel(workerseven)
#workerevenness_plot$evenness + theme_classic() + 
 # xlab("Sample size") + scale_color_discrete(c("#CC6677", "#88CCEE","#DDCC77", "#117733"),labels=c('2018', '2019','2021','2022'))+
  #ylab("Evenness") +theme(legend.title=element_blank())+ggtitle("Workers")


#now need to run stats on H2 for each caste between years

#add row for year to queen stats df
queenseven$evenness$stats_df$year<-queenseven$evenness$stats_df$web
queenseven$evenness$stats_df$year[queenseven$evenness$stats_df$year=="mat1"]<-"2018" 
queenseven$evenness$stats_df$year[queenseven$evenness$stats_df$year=="mat2"]<-"2019" 
queenseven$evenness$stats_df$year[queenseven$evenness$stats_df$year=="mat3"]<-"2021" 
queenseven$evenness$stats_df$year[queenseven$evenness$stats_df$year=="mat4"]<-"2022" 
queenseven$evenness$stats_df$year<-as.factor(queenseven$evenness$stats_df$year)

#repeat for workers
workerseven$evenness$stats_df$year<-workerseven$evenness$stats_df$web
workerseven$evenness$stats_df$year[workerseven$evenness$stats_df$year=="mat1"]<-"2018" 
workerseven$evenness$stats_df$year[workerseven$evenness$stats_df$year=="mat2"]<-"2019" 
workerseven$evenness$stats_df$year[workerseven$evenness$stats_df$year=="mat3"]<-"2021" 
workerseven$evenness$stats_df$year[workerseven$evenness$stats_df$year=="mat4"]<-"2022" 
workerseven$evenness$stats_df$year<-as.factor(workerseven$evenness$stats_df$year)

#add column for increasing devtemp
devqueenseven<-queenseven$evenness$stats_df%>%mutate(devyear=case_when(
  year=="2018"~"A.2018",
  year=="2019"~"D.2019",
  year=="2021"~"B.2021",
  year=="2022"~"C.2022"))
devqueenseven$devyear<-as.factor(devqueenseven$devyear)

devworkerseven<-workerseven$evenness$stats_df%>%mutate(devyear=case_when(
  year=="2018"~"C.2018",
  year=="2019"~"B.2019",
  year=="2021"~"D.2021",
  year=="2022"~"A.2022"))
devworkerseven$devyear<-as.factor(devworkerseven$devyear)

#change with development temp as numeric
devworkerseven<-devworkerseven%>%mutate(devyearc=case_when(
  year=="2018"~6.74,
  year=="2019"~6.65,
  year=="2021"~8.44,
  year=="2022"~6.42))

devqueenseven<-devqueenseven%>%mutate(devyearc=case_when(
  year=="2018"~"10.5",
  year=="2019"~"12.8",
  year=="2021"~"10.6",
  year=="2022"~"11.2"))
devqueenseven$devyearc<-as.numeric(devqueenseven$devyearc)


#run linear model of mean evenness by year for each caste
evennessq_mod<-lm(mean~devyear,data=devqueenseven)
evennessw_mod<-lm(mean~devyear,data=devworkerseven)
evennessq_modc<-lm(mean~as.numeric(devyearc),data=devqueenseven)
evennessw_modc<-lm(mean~as.numeric(devyearc),data=devworkerseven)

#check assumptions - slightly wobbly
par(mfrow=c(2,2))
plot(evennessq_mod)
plot(evennessw_mod)
par(mfrow=c(1,1))

summary(evennessq_mod)
summary(evennessw_mod)
summary(evennessq_modc)
summary(evennessw_modc)

#use Tukey test to get comparisons
evennessq_aovmod <- aov(mean~devyear,data=devqueenseven)
TukeyHSD(evennessq_aovmod)

evennessw_aovmod <- aov(mean~devyear,data=devworkerseven)
TukeyHSD(evennessw_aovmod)

#plot connectance by development temperature year
evenqplotdev<-ggplot(data=devqueenseven,aes(x=devyear,y=mean))+geom_boxplot(fill="#CC6677")+
  theme_classic()+theme(legend.position="none")+ggtitle("Queens interaction evenness")+
  ylim(0.49,0.7)

evenwplotdev<-ggplot(data=devworkerseven,aes(x=devyear,y=mean))+geom_boxplot(fill="#88CCEE")+
  theme_classic()+theme(legend.position="none")+ggtitle("Workers interaction evenness")+
  labs(y="Bootstrapped mean evenness",x="Year by increasing development temperature")+
  ylim(0.49,0.7)

evenplotdev<-plot_grid(evenqplotdev,evenwplotdev)







## BOOTSTRAPPING NESTEDNESS FOR EACH NETWORK ##

bootstrappednestedness<-function(web1,web2,web3,web4){
  nestednessbootstrap=list(mat1=web1,mat2=web2,mat3=web3,mat4=web4)%>%
    lapply(web_matrix_to_df)%>%
    boot_networklevel(col_lower = "lower", # column name for plants
                      col_higher = "higher", # column name for insects
                      level = "both",
                      index = "NODF", # select network metric
                      start = 20,
                      step = 10,
                      n_boot = 1000,  # number of bootstraps
                      n_cpu = 4)
}

queensnested<-bootstrappednestedness(networkbeeswebsQ$Q2018,networkbeeswebsQ$Q2019,
                                 networkbeeswebsQ$Q2021,networkbeeswebsQ$Q2022)


#queennestedness_plot<- gg_networklevel(queensnested)
#queennestedness_plot$NODF + theme_classic() + 
 #xlab("Sample size") + scale_color_discrete(c("#CC6677", "#88CCEE","#DDCC77", "#117733"),labels=c('2018', '2019','2021','2022'))+
  #ylab("NODF") +theme(legend.title=element_blank())+ggtitle("Queens")

#bootstrap nestedness for worker networks
workersnested<-bootstrappednestedness(networkbeeswebsW$W2018,networkbeeswebsW$W2019,
                                  networkbeeswebsW$W2021,networkbeeswebsW$W2022)

workernestedness_plot<- gg_networklevel(workersnested)
workernestedness_plot$NODF + theme_classic() + 
 xlab("Sample size") + scale_color_discrete(c("#CC6677", "#88CCEE","#DDCC77", "#117733"),labels=c('2018', '2019','2021','2022'))+
  ylab("NODF") +theme(legend.title=element_blank())+ggtitle("Workers")


#now need to run stats on H2 for each caste between years

#add row for year to queen stats df
queensnested$NODF$stats_df$year<-queensnested$NODF$stats_df$web
queensnested$NODF$stats_df$year[queensnested$NODF$stats_df$year=="mat1"]<-"2018" 
queensnested$NODF$stats_df$year[queensnested$NODF$stats_df$year=="mat2"]<-"2019" 
queensnested$NODF$stats_df$year[queensnested$NODF$stats_df$year=="mat3"]<-"2021" 
queensnested$NODF$stats_df$year[queensnested$NODF$stats_df$year=="mat4"]<-"2022" 
queensnested$NODF$stats_df$year<-as.factor(queensnested$NODF$stats_df$year)

#repeat for workers
workersnested$NODF$stats_df$year<-workersnested$NODF$stats_df$web
workersnested$NODF$stats_df$year[workersnested$NODF$stats_df$year=="mat1"]<-"2018" 
workersnested$NODF$stats_df$year[workersnested$NODF$stats_df$year=="mat2"]<-"2019" 
workersnested$NODF$stats_df$year[workersnested$NODF$stats_df$year=="mat3"]<-"2021" 
workersnested$NODF$stats_df$year[workersnested$NODF$stats_df$year=="mat4"]<-"2022" 
workersnested$NODF$stats_df$year<-as.factor(workersnested$NODF$stats_df$year)

#add column for increasing devtemp
devqueensnested<-queensnested$NODF$stats_df%>%mutate(devyear=case_when(
  year=="2018"~"A.2018",
  year=="2019"~"D.2019",
  year=="2021"~"B.2021",
  year=="2022"~"C.2022"))
devqueensnested$devyear<-as.factor(devqueensnested$devyear)

devworkersnested<-workersnested$NODF$stats_df%>%mutate(devyear=case_when(
  year=="2018"~"C.2018",
  year=="2019"~"B.2019",
  year=="2021"~"D.2021",
  year=="2022"~"A.2022"))
devworkersnested$devyear<-as.factor(devworkersnested$devyear)

#change with development temp as numeric
devworkersnested<-devworkersnested%>%mutate(devyearc=case_when(
  year=="2018"~6.74,
  year=="2019"~6.65,
  year=="2021"~8.44,
  year=="2022"~6.42))

devqueensnested<-devqueensnested%>%mutate(devyearc=case_when(
  year=="2018"~"10.5",
  year=="2019"~"12.8",
  year=="2021"~"10.6",
  year=="2022"~"11.2"))
devqueensnested$devyearc<-as.numeric(devqueensnested$devyearc)


#run linear model of mean NODF by year for each caste
NODFq_mod<-lm(mean~devyear,data=devqueensnested)
NODFw_mod<-lm(mean~devyear,data=devworkersnested)
NODFq_modc<-lm(mean~as.numeric(devyearc),data=devqueensnested)
NODFw_modc<-lm(mean~as.numeric(devyearc),data=devworkersnested)

#check assumptions - slightly wobbly
par(mfrow=c(2,2))
plot(NODFq_mod)
plot(NODFw_mod)
par(mfrow=c(1,1))

summary(NODFq_mod)
summary(NODFw_mod)
summary(NODFq_modc)
summary(NODFw_modc)

#use Tukey test to get comparisons
NODFq_aovmod <- aov(mean~devyear,data=devqueensnested)
TukeyHSD(NODFq_aovmod)

NODFw_aovmod <- aov(mean~devyear,data=devworkersnested)
TukeyHSD(NODFw_aovmod)

#plot connectance by development temperature year
nestedqplotdev<-ggplot(data=devqueensnested,aes(x=devyear,y=mean))+geom_boxplot(fill="#CC6677")+
  theme_classic()+theme(legend.position="none")+ggtitle("Queens NODF")+
  labs(y="Bootstrapped mean NODF",x="Year by increasing development temperature")+
  ylim(27,58)

nestedwplotdev<-ggplot(data=devworkersnested,aes(x=devyear,y=mean))+geom_boxplot(fill="#88CCEE")+
  theme_classic()+theme(legend.position="none")+ggtitle("Workers NODF")+
  labs(y="Bootstrapped mean NODF",x="Year by increasing development temperature")+
  ylim(27,58)

nestedplotdev<-plot_grid(nestedqplotdev,nestedwplotdev)




#view combined plots for each metric
H2devbyyear
condevbyyear
evenplotdev
nestedplotdev



## NETWORK DISSIMILARITY AND REWIRING ##
#betalinkr
#convert webs to array
networkbeesarrayQ<-webs2array(networkbeeswebsQ)
networkbeesarrayW<-webs2array(networkbeeswebsW)

#apply betalinkr to array 
Qbetalinkr<-betalinkr_multi(networkbeesarrayQ,index="bray", binary=FALSE, partitioning="commondenom",
                            function.dist="vegdist", distofempty="zero",
                            partition.st=FALSE, partition.rr=FALSE)
Qbetalinkr$OSvsST<-ifelse(Qbetalinkr$OS>Qbetalinkr$ST,"OS","ST")

Wbetalinkr<-betalinkr_multi(networkbeesarrayW,index="bray", binary=FALSE, partitioning="commondenom",
                            function.dist="vegdist", distofempty="zero",
                            partition.st=FALSE, partition.rr=FALSE)
Wbetalinkr$OSvsST<-ifelse(Wbetalinkr$OS>Wbetalinkr$ST,"OS","ST") #rewiring > species turnover

#generates matrix of pairwise comparisons
#S = dissimilarity in species composition
#OS = dissimilarity component explained by rewiring
#WN = dissimilarity between networks
#ST = dissimilarity components explained by differences in species composition

bothbetalinkr<-rbind(Qbetalinkr,Wbetalinkr) #create joint df
bothbetalinkr<-bothbetalinkr[order(bothbetalinkr$OS),] #sort lowest to highest

#end script

