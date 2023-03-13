###### INTERACTION NETWORK METRICS BY HYPERVOLUME VOLUME ######

#run after community interaction networks generation and volume calculation

#calculate annual mean volume
meanycvolumeQ<-yearcommvolumeQ%>%group_by(Year)%>%summarise(meanvolume=mean(Volume))
meanycvolumeW<-yearcommvolumeW%>%group_by(Year)%>%summarise(meanvolume=mean(Volume))

#### H2 ####

#add column for mean volume
devqueensH2<-devqueensH2%>%mutate(volume=case_when(
  year=="2018"~"62.7",
  year=="2019"~"50.8",
  year=="2021"~"101.0",
  year=="2022"~"63.3"))
#2019 - 2018 - 2022 - 2021

devworkersH2<-devworkersH2%>%mutate(volume=case_when(
  year=="2018"~"20.9",
  year=="2019"~"12.2",
  year=="2021"~"35.4",
  year=="2022"~"31.5"))
#2019 - 2018 - 2022 - 2021

#run linear model of mean H2 by volume for each caste
H2q_modvol<-lm(mean~as.numeric(volume),data=devqueensH2)
H2w_modvol<-lm(mean~as.numeric(volume),data=devworkersH2)

#check assumptions
par(mfrow=c(2,2))
plot(H2q_modvol)
plot(H2w_modvol)
par(mfrow=c(1,1))

#check outputs
summary(H2q_modvol)
summary(H2w_modvol)

#use Tukey tests to get year comparisons
H2q_aovmodvol <- aov(mean~volume,data=devqueensH2)
TukeyHSD(H2q_aovmodvol)

H2w_aovmodvol <- aov(mean~volume,data=devworkersH2)
TukeyHSD(H2w_aovmodvol)

#plot queen and worker H2 by increasing development temperature
H2qplotvol<-ggplot(data=devqueensH2)+geom_boxplot(aes(x=volume,y=mean),fill="#CC6677")+
  ggtitle("Queens H2")+theme_classic()+theme(legend.position="none")+
  labs(x="Annual hypervolume volume",y="Bootstrapped mean H2")+
  ylim(0.12,0.52) 

H2wplotvol<-ggplot(data=devworkersH2)+geom_boxplot(aes(x=volume,y=mean),fill="#88CCEE")+
  ggtitle("Workers H2")+theme_classic()+theme(legend.position="none")+
  labs(x="Annual hypervolume volume",y="Bootstrapped mean H2")+
  ylim(0.12,0.52)

#plot both together
H2devbyyear<-plot_grid(H2qplotvol,H2wplotvol)




#### CONNECTANCE ####

devqueenscon<-devqueenscon%>%mutate(volume=case_when(
  year=="2018"~"62.7",
  year=="2019"~"50.8",
  year=="2021"~"101.0",
  year=="2022"~"63.3"))
#2019 - 2018 - 2022 - 2021

devworkerscon<-devworkerscon%>%mutate(volume=case_when(
  year=="2018"~"20.9",
  year=="2019"~"12.2",
  year=="2021"~"35.4",
  year=="2022"~"31.5"))
#2019 - 2018 - 2022 - 2021

#run linear model of mean con by volume for each caste
conq_modvol<-lm(mean~as.numeric(volume),data=devqueenscon)
conw_modvol<-lm(mean~as.numeric(volume),data=devworkerscon)

#check assumptions 
par(mfrow=c(2,2))
plot(conq_modvol)
plot(conw_modvol)
par(mfrow=c(1,1))

#check outputs
summary(conq_modvol)
summary(conw_modvol)

#use Tukey test to get year comparisons
conq_aovmodvol <- aov(mean~volume,data=devqueenscon)
TukeyHSD(conq_aovmodvol)

conw_aovmodvol <- aov(mean~volume,data=devworkerscon)
TukeyHSD(conw_aovmodvol)

#plot queen and worker con by increasing development temperature
conqplotvol<-ggplot(data=devqueenscon)+geom_boxplot(aes(x=volume,y=mean),fill="#CC6677")+
  ggtitle("Queens connectance")+theme_classic()+theme(legend.position="none")+
  labs(x="Annual hypervolume volume",y="Bootstrapped mean connectance")+
  ylim(0.12,0.52) 

conwplotvol<-ggplot(data=devworkerscon)+geom_boxplot(aes(x=volume,y=mean),fill="#88CCEE")+
  ggtitle("Workers connectance")+theme_classic()+theme(legend.position="none")+
  labs(x="Annual hypervolume volume",y="Bootstrapped mean connectance")+
  ylim(0.12,0.52)

#plot both together
condevbyyear<-plot_grid(conqplotvol,conwplotvol)



#### INTERACTION EVENNESS ####

devqueenseven<-devqueenseven%>%mutate(volume=case_when(
  year=="2018"~"62.7",
  year=="2019"~"50.8",
  year=="2021"~"101.0",
  year=="2022"~"63.3"))
#2019 - 2018 - 2022 - 2021

devworkerseven<-devworkerseven%>%mutate(volume=case_when(
  year=="2018"~"20.9",
  year=="2019"~"12.2",
  year=="2021"~"35.4",
  year=="2022"~"31.5"))
#2019 - 2018 - 2022 - 2021

#run linear model of mean evenness by volume for each caste
evenq_modvol<-lm(mean~as.numeric(volume),data=devqueenseven)
evenw_modvol<-lm(mean~as.numeric(volume),data=devworkerseven)

#check assumptions 
par(mfrow=c(2,2))
plot(evenq_modvol)
plot(evenw_modvol)
par(mfrow=c(1,1))

#check outputs
summary(evenq_modvol)
summary(evenw_modvol)

#use Tukey tests to get year comparisons
evenq_aovmodvol <- aov(mean~volume,data=devqueenseven)
TukeyHSD(evenq_aovmodvol)

evenw_aovmodvol <- aov(mean~volume,data=devworkerseven)
TukeyHSD(evenw_aovmodvol)

#plot queen and worker even by increasing development temperature
evenqplotvol<-ggplot(data=devqueenseven)+geom_boxplot(aes(x=volume,y=mean),fill="#CC6677")+
  ggtitle("Queens evenness")+theme_classic()+theme(legend.position="none")+
  labs(x="Annual hypervolume volume",y="Bootstrapped mean evenness")+
  ylim(0.48,0.69)

evenwplotvol<-ggplot(data=devworkerseven)+geom_boxplot(aes(x=volume,y=mean),fill="#88CCEE")+
  ggtitle("Workers evenness")+theme_classic()+theme(legend.position="none")+
  labs(x="Annual hypervolume volume",y="Bootstrapped mean evenness")+
  ylim(0.48,0.69)

#plot both together
evendevbyyear<-plot_grid(evenqplotvol,evenwplotvol)


#### NESTEDNESS ####

devqueensnested<-devqueensnested%>%mutate(volume=case_when(
  year=="2018"~"62.7",
  year=="2019"~"50.8",
  year=="2021"~"101.0",
  year=="2022"~"63.3"))
#2019 - 2018 - 2022 - 2021

devworkersnested<-devworkersnested%>%mutate(volume=case_when(
  year=="2018"~"20.9",
  year=="2019"~"12.2",
  year=="2021"~"35.4",
  year=="2022"~"31.5"))
#2019 - 2018 - 2022 - 2021

#run linear model of mean nested by volume for each caste
nestedq_modvol<-lm(mean~as.numeric(volume),data=devqueensnested)
nestedw_modvol<-lm(mean~as.numeric(volume),data=devworkersnested)

#check assumptions 
par(mfrow=c(2,2))
plot(nestedq_modvol)
plot(nestedw_modvol)
par(mfrow=c(1,1))

#check outputs
summary(nestedq_modvol)
summary(nestedw_modvol)

#use Tukey tests to get year comparisons
nestedq_aovmodvol <- aov(mean~volume,data=devqueensnested)
TukeyHSD(nestedq_aovmodvol)

nestedw_aovmodvol <- aov(mean~volume,data=devworkersnested)
TukeyHSD(nestedw_aovmodvol)

#sort order queens
devqueensnested<-devqueensnested[order(as.numeric(devqueensnested$volume)),]

#plot queen and worker nested by increasing development temperature
nestedqplotvol<-ggplot(data=devqueensnested)+geom_boxplot(aes(x=volume,y=mean),fill="#CC6677")+
  ggtitle("Queens nestedness")+theme_classic()+theme(legend.position="none")+
  labs(x="Annual hypervolume volume",y="Bootstrapped mean nestedness")+
  ylim(15,34)

nestedwplotvol<-ggplot(data=devworkersnested)+geom_boxplot(aes(x=volume,y=mean),fill="#88CCEE")+
  ggtitle("Workers nestedness")+theme_classic()+theme(legend.position="none")+
  labs(x="Annual hypervolume volume",y="Bootstrapped mean nestedness")+
  ylim(15,34)

#plot both together
nesteddevbyyear<-plot_grid(nestedqplotvol,nestedwplotvol)

### end script ###
