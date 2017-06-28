setwd("~/Desktop/")

######Load_packages#######
library(ggplot2)
library(dplyr)
library(plyr)
library(MuMIn)
library(lme4)
library(RColorBrewer)

#### Load all data ####
frog<-read.csv('demodat1.1.csv',header=T)

#### Complete case of age and modify variable types ####
ages <- frog %>% filter(age != "NA")
ages$status <- factor(ages$status)
ages$status <- plyr::revalue(ages$status, c("0" = "Negative", "1" = "Positive"))
ages$site <- factor(ages$site)
ages$paired <-  factor(ages$paired)
ages$sex <- plyr::revalue(ages$sex, c("F" = "Female", "M" = "Male"))

#### Stacked bar plots ####

  ## Make custom brewer palette ##

my.cols <- brewer.pal(9,"RdBu")
my.cols[5] <- "grey80"
my.cols

  ## Plot stacked bars of age by disease history status (faceted by sex) ##
    ## N.B - Fine tuning of this figure into publishable version performed in inkscape ##

ages %>%
  ggplot(aes(x=status, fill=factor(age))) + 
  geom_bar(position="fill") + 
  facet_wrap(~sex) +
  scale_fill_manual(values = my.cols, name = "Age") +
  theme_bw(base_size = 16, 
           base_family = 'Helvetica') + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  ylab("Proportion of summed population") +
  xlab("Population Disease Status")