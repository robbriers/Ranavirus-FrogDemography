setwd("~/Desktop/")

##### Load packages #####

library(ggplot2)
library(dplyr)
library(MuMIn)
library(lme4)

#### Load all data ####

frog<-read.csv('demodat1.1.csv',header=T)

#### Complete case of age and modify variable types ####
agedata <- frog %>% filter(age != "NA")
agedata$status <- plyr::mapvalues(agedata$status, from = c(0,1), to = c('Negative', 'Positive'))
agedata <- mutate_at(agedata, c('status', 'site', 'paired'), as.factor)

### Complete_cases_of_SVL ####

svldata <- agedata %>% filter(svl != "NA")

#### subset for males ####

svlmales <- svldata %>% filter(sex == "M")

#### summary stats for males ####

svlmales %>% group_by(., status) %>%
  summarise(., meanage = mean(age), total=length(status), medianage=median(age), sd=sd(age), se=sd(age)/sqrt(length(age)))

#### subset for females ####

svlfem <- svldata %>% filter(sex == "F")

#### summary stats for females ####

svlfem %>% group_by(., status) %>%
  summarise(., meanage = mean(age), total=length(status), medianage=median(age), sd=sd(age), se=sd(age)/sqrt(length(age)))


#### Remove two and three year old animals from the positive female data set (not present in negative) ####

svlfem1 <- svlfem %>% filter(age > 3)

#### PLOTS ####

#### Plot SVL by age ####

#### all animals ####

svldata %>% 
  ggplot (aes(factor(age), svl, fill=status)) + 
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(~status) + 
  theme_bw(base_size = 16, 
           base_family = 'Helvetica') + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values=c("#2166AC", "#B2182B"), 
                    name="Population Disease\nStatus", 
                    breaks=c("0", "1"), 
                    labels=c("Negative", "Positive")) + 
  scale_color_manual(values=c("#2166AC", "#B2182B"), 
                     name="Population Disease\nStatus", 
                     breaks=c("0", "1"), 
                     labels=c("Negative", "Positive")) + 
  ylab("Snout to vent length (mm)") + 
  xlab("Age (years)") + 
  geom_jitter(aes(col=status), 
              shape = 21, fill = 'white', size = 1)

#### male animals ####

svlmales %>% 
  ggplot (aes(factor(age), svl, fill=status)) + 
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(~status) + 
  theme_bw(base_size = 16, 
           base_family = 'Helvetica') + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values=c("#2166AC", "#B2182B"), 
                    name="Population Disease\nStatus", 
                    breaks=c("0", "1"), 
                    labels=c("Negative", "Positive")) + 
  scale_color_manual(values=c("#2166AC", "#B2182B"), 
                     name="Population Disease\nStatus", 
                     breaks=c("0", "1"), 
                     labels=c("Negative", "Positive")) + 
  ylab("Snout to vent length (mm)") + 
  xlab("Age (years)") + 
  geom_jitter(aes(col=status), 
              shape = 21, fill = 'white', size = 1)


#### female animals ####

svlfem %>% 
  ggplot (aes(factor(age), svl, fill=status)) + 
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(~status) + 
  theme_bw(base_size = 16, 
           base_family = 'Helvetica') + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values=c("#2166AC", "#B2182B"), 
                    name="Population Disease\nStatus", 
                    breaks=c("0", "1"), 
                    labels=c("Negative", "Positive")) + 
  scale_color_manual(values=c("#2166AC", "#B2182B"), 
                     name="Population Disease\nStatus", 
                     breaks=c("0", "1"), 
                     labels=c("Negative", "Positive")) + 
  ylab("Snout to vent length (mm)") + 
  xlab("Age (years)") + 
  geom_jitter(aes(col=status), 
              shape = 21, fill = 'white', size = 1)

####STATISTICAL MODELS####

#### Question - Does svl vary significantly with age and is this influenced by disease status?

####linear mixed effects model (controlling for site)

#### all animals ###

svl1 <- lmer(svl ~ age*status + (1|site), data=svldata, na.action = na.fail)
svl2 <- lmer(svl ~ age + status + (1|site), data=svldata, na.action = na.fail)
svl3 <- lmer(svl ~ age + (1|site), data=svldata, na.action = na.fail)
svlnull <- lmer(svl ~ 1 + (1|site), data=svldata, na.action = na.fail)

summary(svl2)
summary(svl3)
summary(svl4)
summary(svlnull)

anova(svl1, svl2)
anova(svl2, svl3) # No effect of status on SVL p=0.294
anova(svl3, svlnull) # SVL increases significantly with age p=<0.001

#### male animals ###

msvl1 <- lmer(svl ~ age*status + (1|site), data=svlmales, na.action = na.fail)
msvl2 <- lmer(svl ~ age + status + (1|site), data=svlmales, na.action = na.fail)
msvl3 <- lmer(svl ~ age + (1|site), data=svlmales, na.action = na.fail)
msvlnull <- lmer(svl ~ 1 + (1|site), data=svlmales, na.action = na.fail)

summary(msvl1)
summary(msvl2)
summary(msvl3)
summary(msvlnull)

anova(msvl1, msvl2)
anova(msvl2, msvl3) # No effect of status on male SVL p=0.647
anova(msvl3, msvlnull) #Male SVL increases significantly with age p=<0.001

#### female animals #### 

#### with infected 2 and 3 year olds ####
fsvl1 <- lmer(svl ~ age*status + (1|site), data=svlfem, na.action = na.fail)
fsvl2 <- lmer(svl ~ age + status + (1|site), data=svlfem, na.action = na.fail)
fsvl3 <- lmer(svl ~ age + (1|site), data=svlfem, na.action = na.fail)
fsvlnull1 <- lmer(svl ~ 1 + (1|site), data=svlfem, na.action = na.fail)

summary(fsvl1)
summary(fsvl2)
summary(fsvl3)


anova(fsvl1, fsvl2) #Female SVL for given age different between disease statuses? p=0.02
#Suggests that infected females are smaller, but grow more rapidly than uninfected counter parts

anova(fsvl2, fsvl3) #No interaction of status alone p=0.119

anova(fsvl3, fsvlnull1) #Female SVL increases with age p=<0.001

#### Without infected 2and3 year olds ####
fsvl4 <- lmer(svl ~ age*status + (1|site), data=svlfem1, na.action = na.fail)
fsvl5 <- lmer(svl ~ age + status + (1|site), data=svlfem1, na.action = na.fail)
fsvl6 <- lmer(svl ~ age + (1|site), data=svlfem1, na.action = na.fail)
fsvlnull2 <- lmer(svl ~ 1 + (1|site), data=svlfem1, na.action = na.fail)

summary(fsvl4)
summary(fsvl5)
summary(fsvl6)

anova(fsvl4, fsvl5) #Effect now gone probably caused by the presence of infected young animals
anova(fsvl5, fsvl6) #No effect of status on SVL p=0.119
anova(fsvl6, fsvlnull2) #Female SVL increases with Age p=<0.001

# Answer - In both males and females SVL increased significantly with age, this is not altererd by status suggesting that growth rates are similar between population disease classes. In females, the inclusion of 2 and 3 year old animals absent from the Uninfected dataset creates a signifcant effect of the interaction between age*status on SVL, possibly suggesting that females are smaller but grow faster at infected populations. Possibly increased extrinsic mortality resulting in increased growth rate to maturity but lower maximal size?