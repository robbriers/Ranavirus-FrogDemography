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

  #### Complete cases of SVL ####

svldata <- agedata %>% filter(svl != "NA")

  #### Complete cases for sexual maturity data ####

sexmat <- svldata %>% filter(agemat != "NA")
sexmat$agemat <- factor(sexmat$agemat) 
sexmat$sex <- plyr::revalue(sexmat$sex, c("F" = "Female", "M" = "Male"))

#### PLOTS ####

  #### create custom colour palette ####

cols <- c("#B2182B", "#D6604D", "#F4A582")

  #### Stacked bar of age at sexual maturity by disease history status (faceted by sex) ####
    ## N.B - Fine tuning of this figure into publishable version performed in inkscape ##

sexmat %>%
  ggplot(aes(x=status, fill=agemat)) + 
  geom_bar(position="fill") + 
  facet_wrap(~sex) +
  scale_fill_manual(values=c(cols), name = "Age at \nMaturity") +
  theme_bw(base_size = 16, base_family = 'Helvetica') + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  ylab("Proportion of summed population") +
  xlab("Population Disease Status")

#### STATISTICAL MODELS ####

  #### Create dataset with age at maturity as numeric variable ####

sexmat1 <- svldata %>% filter(agemat != "NA")
sexmat1$agemat <- as.numeric(sexmat1$agemat)
sexmat1$sex <- plyr::revalue(sexmat1$sex, c("F" = "Female", "M" = "Male"))

  #### Filter for males ####

sexmatmale1 <- sexmat1 %>% filter(sex == "Male")
glimpse(sexmatmale1)

  #### Summary stats for males ####

sexmatmale1 %>%
  group_by(status) %>% 
  summarise(mean=mean(agemat), total=length(agemat), median = median(agemat), sd=sd(agemat), se=sd(agemat)/sqrt(length(age)), tot4 = sum(agemat==4), tot2 = sum(agemat==2))

  #### Filter for females ####

sexmatfem1<- sexmat1 %>% filter(sex == "Female")

  #### Summary stats for females ####

sexmatfem1 %>%
  group_by(status) %>% 
  summarise(mean=mean(agemat), total=length(agemat), median = median(agemat), sd=sd(agemat), se=sd(agemat)/sqrt(length(age)), tot4 = sum(agemat==4), tot2 = sum(agemat==2))

#### Question - Do animals from infected populations reach sexualy maturity earlier than those from uninfected populations? ####

  #### all animals
    #linear_mixed_effects_model_(controlling_for_site)

asm1 <- lmer(agemat ~ status + (1|site), data=sexmat, na.action = na.fail)

asmnull <- lmer(agemat ~ 1 + (1|site), data=sexmat, na.action = na.fail)

summary(asm1)

anova(asm1, asmnull) #No_effect_of_status_p=0.134

  #### male animals

masm1 <- lmer(agemat ~ status + (1|site), data=sexmatmale1, na.action = na.fail)

masmnull <- lmer(agemat ~ 1 + (1|site), data=sexmatmale1, na.action = na.fail)

summary(masm1)

anova(masm1,masmnull) #No_effect_of_status_p=0.321

  #### female animals

fasm1 <- lmer(agemat ~ status + (1|site), data=sexmatfem1, na.action = na.fail)

fasmnull <- lmer(agemat ~ 1 + (1|site), data=sexmatfem1, na.action = na.fail)

summary(fasm1)

anova(fasm1,fasmnull) #No_effect_of_status_p=0.586

# Answer - No.