setwd("~/Desktop/")
#### Load packages

library(ggplot2)
library(dplyr)
library(plyr)
library(MuMIn)
library(lme4)
library(RColorBrewer)
library(grid)
library(gridExtra)

#### Load the data

frog<-read.csv('demodat1.1.csv',header=T)

head(frog)

nrow(frog)

### Subset to complete cases for age

agedata <- frog %>% filter(age != "NA")

nrow(agedata)

agedata$age <- as.factor(agedata$age)

###### Data Exploration and Plots ######

#### Number of individuals per disease status ####

with(agedata,table(site,status))
colSums(with(agedata,table(site,status)))

#### Create infected and uninfected dataframe ###
inf <- agedata[which(agedata$status=="1"),]
uninf <- agedata[which(agedata$status=="0"),] 

#### Create infected age by status dataframe ####

infagebystatus <- with(inf,table(age,status))
infagebystatus <- as.data.frame(infagebystatus)

#### Create uninfected age by status dataframe ####

uninfagebystatus <- with(uninf,table(age,status))
uninfagebystatus <- as.data.frame(uninfagebystatus)

#### Combine age by status datasframes ####
comboagebystatus <- rbind(infagebystatus, uninfagebystatus)

#### Raw age by status histogram of age strucutres (males and females combined) ####

hist <- ggplot(comboagebystatus,aes(age,Freq,fill=status))+
  geom_bar(stat="identity",position='dodge') + 
  ylab("Number of individuals") + 
  xlab("Age (years)")

P1 <- hist + scale_fill_manual(values=c("#B2182B", "#2166AC"), 
                                name="Population Disease\n            Status",
                                breaks=c("0", "1"),
                                labels=c("Negative (n=140) ", "Positive (n=134)")) + 
  theme_bw(base_size = 16, base_family = 'Helvetica') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

P1 

#####  Statistical Models ####

  ####means####

with(agedata,tapply(agemat,status,mean,na.rm=T))

with(agedata,tapply(agemat,status,range,na.rm=T))

  #### summary statistics#### 

glimpse(agedata)

agedata$age <- as.numeric(agedata$age)
agedata$site <- as.factor(agedata$site)

frogstats <- agedata %>% 
  group_by(site) %>% 
  dplyr::summarise(
  ntotal=length(age),
  over5=length(which(age>5)),
  meanage=mean(age),
  status=unique(status)
)

frogstats

with(frogstats,tapply(meanage,status,mean))

with(frogstats,tapply(meanage,status,median))

  #### Probability of being over 5 by disease status ####

with(agedata,tapply(age>5,status,mean,na.rm=T))

  #### Binomial Model of Prob of Being Over 5 ####

b1<-glm(cbind(over5,ntotal-over5) ~ status,data=frogstats,family=binomial)
summary(b1)
plogis(1.13) #75%
plogis(1.13-2.02) #29%

################
# Ordinal Age Models
################

library(MCMCglmm)

#### Define uninformative priors ####

ordinalprior=list(R=list(V=1, fix=1), G=list(G1=list(V=1, nu=2)))

#### Single Age Response ####

o1<-MCMCglmm(age~status,random=~site,family="ordinal",data=agedata,verbose=F,prior=ordinalprior,burnin=100000,nitt=600000,thin=500)

summary(o1)

summary(o1$Sol)

#### MCMC Diagnostics ####

autocorr(o1$Sol)

autocorr(o1$VCV)

geweke.diag(o1$Sol)

#### Diagnostic plots ####

par(mfrow=c(2,2), mar=c(2,2,1,0))
plot(o1$Sol, auto.layout=F)

plot.acfs <- function(x) {
  n <- dim(x)[2]
  par(mfrow=c(ceiling(n/2),2), mar=c(3,2,3,0))
  for (i in 1:n) {
    acf(x[,i], lag.max=100, main=colnames(x)[i])
    grid()
  }
}
plot.acfs(o1$Sol)

#### MODEL PREDICTIONS ####

#### Get Base States for Each Disease Class ####

head(o1$Sol)

stat0base<-o1$Sol[,1]

stat1base<-o1$Sol[,1]+o1$Sol[,2]

#### Extract Cutpoint Matrix ####

o1cp<-o1$CP

dim(o1cp)

o1cp

#Status 0 Age 
mat0<-matrix(NA,ncol=9,nrow=nrow(o1cp))
mat0[,1]<-pnorm(-stat0base)
mat0[,2]<-pnorm(o1cp[,1]-stat0base) - pnorm(-stat0base)
mat0[,3]<-pnorm(o1cp[,2]-stat0base) - pnorm(o1cp[,1]-stat0base)
mat0[,4]<-pnorm(o1cp[,3]-stat0base) - pnorm(o1cp[,2]-stat0base)
mat0[,5]<-pnorm(o1cp[,4]-stat0base) - pnorm(o1cp[,3]-stat0base)
mat0[,6]<-pnorm(o1cp[,5]-stat0base) - pnorm(o1cp[,4]-stat0base)
mat0[,7]<-pnorm(o1cp[,6]-stat0base) - pnorm(o1cp[,5]-stat0base)
mat0[,8]<-pnorm(o1cp[,7]-stat0base) - pnorm(o1cp[,6]-stat0base)
mat0[,9]<-1 - pnorm(o1cp[,7]-stat0base)

#Status 1 Age 
mat1<-matrix(NA,ncol=9,nrow=nrow(o1cp))
mat1[,1]<-pnorm(-stat1base)
mat1[,2]<-pnorm(o1cp[,1]-stat1base) - pnorm(-stat1base)
mat1[,3]<-pnorm(o1cp[,2]-stat1base) - pnorm(o1cp[,1]-stat1base)
mat1[,4]<-pnorm(o1cp[,3]-stat1base) - pnorm(o1cp[,2]-stat1base)
mat1[,5]<-pnorm(o1cp[,4]-stat1base) - pnorm(o1cp[,3]-stat1base)
mat1[,6]<-pnorm(o1cp[,5]-stat1base) - pnorm(o1cp[,4]-stat1base)
mat1[,7]<-pnorm(o1cp[,6]-stat1base) - pnorm(o1cp[,5]-stat1base)
mat1[,8]<-pnorm(o1cp[,7]-stat1base) - pnorm(o1cp[,6]-stat1base)
mat1[,9]<-1 - pnorm(o1cp[,7]-stat1base)


#### Test for Sign. Difference Between Disease Classes at various ages ####
  
  #Age 2 
  quantile(mat1[,1]-mat0[,1],c(0.025,0.975))* 100
  mean(mat1[,1]-mat0[,1]) *100
  hist(mat1[,1]-mat0[,1])
  
  #Age 3 
  quantile(mat1[,2]-mat0[,2],c(0.025,0.975)) * 100
  
  #Age 4 
  quantile(mat1[,5]-mat0[,5],c(0.025,0.975)) * 100
  
  #Age 10
  quantile(mat1[,9]-mat0[,9],c(0.025,0.975)) * 100
  
#### Assemble into differential Data Frame ####

mat1_d <- data.frame(mat1)
colnames(mat1_d) <- paste('age_', sep = '', 2:10)
mat1_d$status = 1
mat0_d <- data.frame(mat0)
colnames(mat0_d) <- paste('age_', sep = '', 2:10)
mat0_d$status = 0
  
ages <- colnames(mat1_d)[grepl('age', colnames(mat1_d))]

#### compiling function ####
quants <- function(x, data_0=NULL, data_1=NULL){
  temp0 <- data_0[,colnames(data_0) == x]
  temp1 <- data_1[,colnames(data_1) == x]
  
  temp <- data.frame(age = x, diff_mean = quantile(temp1 - temp0, c(0.5)) * 100,
                     CI_low = quantile(temp1 - temp0, c(0.025)) * 100,
                     CI_high = quantile(temp1 - temp0, c(0.975)) * 100)
  return(temp)
  }

diff_preds <- plyr::ldply(ages, quants, data_0 = mat0_d, data_1 = mat1_d)

diff_preds$age <- as.numeric(gsub('.*\\_', '', diff_preds$age))

diff_preds

#### Plot differences in posterior probabilities ####
  ## N.B - Fine tuning of this figure into publishable version performed in inkscape ##

ggplot(diff_preds) +
  geom_abline(aes(slope = 0, intercept = 0), linetype="dashed") +
  geom_errorbar(aes(x = age, ymin = CI_low, ymax = CI_high), size = 0.2) +
  geom_point(aes(x=age,y=diff_mean), size = 2, shape=16) +
  scale_x_continuous(breaks = round(seq(min(diff_preds$age), max(diff_preds$age), by = 1),1)) + 
  theme_bw() +
  xlab('Age') +
  ylab('Difference of posterior probabilities') +
  theme_bw(base_size = 16, 
           base_family = 'Helvetica') + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))