setwd("~/Desktop/")

#Load_packages

library(popbio)
library(popdemo)
library(plyr)
library(dplyr)

#### Load data ####
frog<-read.csv('demodat1.1.csv',header=T)
head(frog)

#### Remove lines with no age data ####
agedata<-subset(frog,!is.na(age))
head(agedata)

#### Subset data by infection status ####
inf <- agedata [which(agedata$status=='1'), ]

uninf<- agedata [which(agedata$status=='0'), ]

#### Count age frequencies ####
infreq <- count(inf, 'age')

uninfreq <- count(uninf, 'age')

prop.table(infreq$freq)

prop.table(uninfreq$freq)


#### Define counting function ####
  ## N.B Ensure both population vectors contain 11 stages ##

start_samp <- function(age, prop, n_out){
  
  temp <- data.frame(age = sample(unique(age), size = n_out, replace = TRUE, prob = prop)) %>%
    dplyr::group_by(., age) %>%
    dplyr::summarise(., freq = n()) %>%
    data.frame()
  return(temp$freq)
  
}

infvec <- c(0,0,start_samp(infreq$age, prop.table(infreq$freq), n_out = 150))

infvec

uninfvec <- c(0,0,0,0,start_samp(uninfreq$age, prop.table(uninfreq$freq), n_out = 150))

uninfvec

#### Construct initial matrix (vital rates from Biek 2002) ####

Basemat <- matrix(c(0,0,0,0,650,650,650,650,650,650,650,
              0.92,0,0,0,0,0,0,0,0,0,0,
              0,0.06,0,0,0,0,0,0,0,0,0,
              0,0,0.34,0.25,0,0,0,0,0,0,0,
              0,0,0,0.08,0,0,0,0,0,0,0,
              0,0,0,0,0.43,0,0,0,0,0,0,
              0,0,0,0,0,0.43,0,0,0,0,0,
              0,0,0,0,0,0,0.43,0,0,0,0,
              0,0,0,0,0,0,0,0.43,0,0,0,
              0,0,0,0,0,0,0,0,0.43,0,0,
              0,0,0,0,0,0,0,0,0,0.43,0), ncol=11, byrow=T)

Basemat

#### CREATE STOCHASTIC MATRICES ####

#### Varying fecundity and survial based on age/size (Infected Scenario) ####
infmat1 <- matrix(c(0,0,250,300,350,400,450,500,550,600,650,
                    0.019,0.25,0,0,0,0,0,0,0,0,0,
                    0,0.01,0,0,0,0,0,0,0,0,0,
                    0,0.05,0.45,0,0,0,0,0,0,0,0,
                    0,0.02,0,0.44,0,0,0,0,0,0,0,
                    0,0,0,0,0.43,0,0,0,0,0,0,
                    0,0,0,0,0,0.42,0,0,0,0,0,
                    0,0,0,0,0,0,0.41,0,0,0,0,
                    0,0,0,0,0,0,0,0.40,0,0,0,
                    0,0,0,0,0,0,0,0,0.39,0,0,
                    0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmat5 <- matrix(c(0,0,250,300,350,400,450,500,550,600,650,
              0.019,0.25,0,0,0,0,0,0,0,0,0,
              0,0.01,0,0,0,0,0,0,0,0,0,
              0,0.05,0.45,0,0,0,0,0,0,0,0,
              0,0.02,0,0.40,0,0,0,0,0,0,0,
              0,0,0,0,0.35,0,0,0,0,0,0,
              0,0,0,0,0,0.30,0,0,0,0,0,
              0,0,0,0,0,0,0.25,0,0,0,0,
              0,0,0,0,0,0,0,0.20,0,0,0,
              0,0,0,0,0,0,0,0,0.15,0,0,
              0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmat10 <- matrix(c(0,0,250,300,350,400,450,500,550,600,650,
                    0.019,0.25,0,0,0,0,0,0,0,0,0,
                    0,0.01,0,0,0,0,0,0,0,0,0,
                    0,0.05,0.45,0,0,0,0,0,0,0,0,
                    0,0.02,0,0.35,0,0,0,0,0,0,0,
                    0,0,0,0,0.25,0,0,0,0,0,0,
                    0,0,0,0,0,0.15,0,0,0,0,0,
                    0,0,0,0,0,0,0.10,0,0,0,0,
                    0,0,0,0,0,0,0,0.10,0,0,0,
                    0,0,0,0,0,0,0,0,0.10,0,0,
                    0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmat15 <- matrix(c(0,0,250,300,350,400,450,500,550,600,650,
                     0.019,0.25,0,0,0,0,0,0,0,0,0,
                     0,0.01,0,0,0,0,0,0,0,0,0,
                     0,0.05,0.45,0,0,0,0,0,0,0,0,
                     0,0.02,0,0.30,0,0,0,0,0,0,0,
                     0,0,0,0,0.15,0,0,0,0,0,0,
                     0,0,0,0,0,0.10,0,0,0,0,0,
                     0,0,0,0,0,0,0.10,0,0,0,0,
                     0,0,0,0,0,0,0,0.10,0,0,0,
                     0,0,0,0,0,0,0,0,0.10,0,0,
                     0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmat20 <- matrix(c(0,0,250,300,350,400,450,500,550,600,650,
                      0.019,0.25,0,0,0,0,0,0,0,0,0,
                      0,0.01,0,0,0,0,0,0,0,0,0,
                      0,0.05,0.45,0,0,0,0,0,0,0,0,
                      0,0.02,0,0.25,0,0,0,0,0,0,0,
                      0,0,0,0,0.10,0,0,0,0,0,0,
                      0,0,0,0,0,0.10,0,0,0,0,0,
                      0,0,0,0,0,0,0.10,0,0,0,0,
                      0,0,0,0,0,0,0,0.10,0,0,0,
                      0,0,0,0,0,0,0,0,0.10,0,0,
                      0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmat25 <- matrix(c(0,0,250,300,350,400,450,500,550,600,650,
                     0.019,0.25,0,0,0,0,0,0,0,0,0,
                     0,0.01,0,0,0,0,0,0,0,0,0,
                     0,0.05,0.45,0,0,0,0,0,0,0,0,
                     0,0.02,0,0.20,0,0,0,0,0,0,0,
                     0,0,0,0,0.10,0,0,0,0,0,0,
                     0,0,0,0,0,0.10,0,0,0,0,0,
                     0,0,0,0,0,0,0.10,0,0,0,0,
                     0,0,0,0,0,0,0,0.10,0,0,0,
                     0,0,0,0,0,0,0,0,0.10,0,0,
                     0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)


#### Vary only fecundity by age/size (Uninfected Scenario) ####
uninfmat <- matrix(c(0,0,250,300,350,400,450,500,550,600,650,
                 0.019,0.25,0,0,0,0,0,0,0,0,0,
                 0,0.01,0,0,0,0,0,0,0,0,0,
                 0,0.05,0.45,0,0,0,0,0,0,0,0,
                 0,0.02,0,0.45,0,0,0,0,0,0,0,
                 0,0,0,0,0.45,0,0,0,0,0,0,
                 0,0,0,0,0,0.45,0,0,0,0,0,
                 0,0,0,0,0,0,0.45,0,0,0,0,
                 0,0,0,0,0,0,0,0.45,0,0,0,
                 0,0,0,0,0,0,0,0,0.45,0,0,
                 0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)


#### Mimic spawning failure in infected pops ####
infmat1spawn <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,
                    0.019,0.25,0,0,0,0,0,0,0,0,0,
                    0,0.01,0,0,0,0,0,0,0,0,0,
                    0,0.05,0.45,0,0,0,0,0,0,0,0,
                    0,0.02,0,0.44,0,0,0,0,0,0,0,
                    0,0,0,0,0.43,0,0,0,0,0,0,
                    0,0,0,0,0,0.42,0,0,0,0,0,
                    0,0,0,0,0,0,0.41,0,0,0,0,
                    0,0,0,0,0,0,0,0.40,0,0,0,
                    0,0,0,0,0,0,0,0,0.39,0,0,
                    0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmat5spawn <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,
                    0.019,0.25,0,0,0,0,0,0,0,0,0,
                    0,0.01,0,0,0,0,0,0,0,0,0,
                    0,0.05,0.45,0,0,0,0,0,0,0,0,
                    0,0.02,0,0.40,0,0,0,0,0,0,0,
                    0,0,0,0,0.35,0,0,0,0,0,0,
                    0,0,0,0,0,0.30,0,0,0,0,0,
                    0,0,0,0,0,0,0.25,0,0,0,0,
                    0,0,0,0,0,0,0,0.20,0,0,0,
                    0,0,0,0,0,0,0,0,0.15,0,0,
                    0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmat10spawn <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,
                     0.019,0.25,0,0,0,0,0,0,0,0,0,
                     0,0.01,0,0,0,0,0,0,0,0,0,
                     0,0.05,0.45,0,0,0,0,0,0,0,0,
                     0,0.02,0,0.35,0,0,0,0,0,0,0,
                     0,0,0,0,0.25,0,0,0,0,0,0,
                     0,0,0,0,0,0.15,0,0,0,0,0,
                     0,0,0,0,0,0,0.10,0,0,0,0,
                     0,0,0,0,0,0,0,0.10,0,0,0,
                     0,0,0,0,0,0,0,0,0.10,0,0,
                     0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmat15spawn <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,
                     0.019,0.25,0,0,0,0,0,0,0,0,0,
                     0,0.01,0,0,0,0,0,0,0,0,0,
                     0,0.05,0.45,0,0,0,0,0,0,0,0,
                     0,0.02,0,0.30,0,0,0,0,0,0,0,
                     0,0,0,0,0.15,0,0,0,0,0,0,
                     0,0,0,0,0,0.10,0,0,0,0,0,
                     0,0,0,0,0,0,0.10,0,0,0,0,
                     0,0,0,0,0,0,0,0.10,0,0,0,
                     0,0,0,0,0,0,0,0,0.10,0,0,
                     0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmat20spawn <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,
                     0.019,0.25,0,0,0,0,0,0,0,0,0,
                     0,0.01,0,0,0,0,0,0,0,0,0,
                     0,0.05,0.45,0,0,0,0,0,0,0,0,
                     0,0.02,0,0.25,0,0,0,0,0,0,0,
                     0,0,0,0,0.10,0,0,0,0,0,0,
                     0,0,0,0,0,0.10,0,0,0,0,0,
                     0,0,0,0,0,0,0.10,0,0,0,0,
                     0,0,0,0,0,0,0,0.10,0,0,0,
                     0,0,0,0,0,0,0,0,0.10,0,0,
                     0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmat25spawn <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,
                     0.019,0.25,0,0,0,0,0,0,0,0,0,
                     0,0.01,0,0,0,0,0,0,0,0,0,
                     0,0.05,0.45,0,0,0,0,0,0,0,0,
                     0,0.02,0,0.20,0,0,0,0,0,0,0,
                     0,0,0,0,0.10,0,0,0,0,0,0,
                     0,0,0,0,0,0.10,0,0,0,0,0,
                     0,0,0,0,0,0,0.10,0,0,0,0,
                     0,0,0,0,0,0,0,0.10,0,0,0,
                     0,0,0,0,0,0,0,0,0.10,0,0,
                     0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

#### Mimic spawning failure in uninfected pops ####
uninfspawn <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,
                 0.019,0.25,0,0,0,0,0,0,0,0,0,
                 0,0.01,0,0,0,0,0,0,0,0,0,
                 0,0.05,0.45,0,0,0,0,0,0,0,0,
                 0,0.02,0,0.45,0,0,0,0,0,0,0,
                 0,0,0,0,0.45,0,0,0,0,0,0,
                 0,0,0,0,0,0.45,0,0,0,0,0,
                 0,0,0,0,0,0,0.45,0,0,0,0,
                 0,0,0,0,0,0,0,0.45,0,0,0,
                 0,0,0,0,0,0,0,0,0.45,0,0,
                 0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

#### Mimic mass mortality in infected pops(survival of all post metamorphic stages 20%) ####
infmatmass1 <- matrix(c(0,0,250,300,350,400,450,500,550,600,650,
                    0.019,0.25,0,0,0,0,0,0,0,0,0,
                    0,0.01,0,0,0,0,0,0,0,0,0,
                    0,0.05,0.2,0,0,0,0,0,0,0,0,
                    0,0.02,0,0.2,0,0,0,0,0,0,0,
                    0,0,0,0,0.2,0,0,0,0,0,0,
                    0,0,0,0,0,0.2,0,0,0,0,0,
                    0,0,0,0,0,0,0.2,0,0,0,0,
                    0,0,0,0,0,0,0,0.2,0,0,0,
                    0,0,0,0,0,0,0,0,0.2,0,0,
                    0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmatmass2 <- matrix(c(0,0,250,300,350,400,450,500,550,600,650,
                        0.019,0.25,0,0,0,0,0,0,0,0,0,
                        0,0.01,0,0,0,0,0,0,0,0,0,
                        0,0.05,0.2,0,0,0,0,0,0,0,0,
                        0,0.02,0,0.2,0,0,0,0,0,0,0,
                        0,0,0,0,0.10,0,0,0,0,0,0,
                        0,0,0,0,0,0.10,0,0,0,0,0,
                        0,0,0,0,0,0,0.10,0,0,0,0,
                        0,0,0,0,0,0,0,0.10,0,0,0,
                        0,0,0,0,0,0,0,0,0.10,0,0,
                        0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

#### Mimic mass mortality and reproductive failure in infected populations(fecundity 0, survival 20%) ####
infmatcat1 <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,
                 0.019,0.25,0,0,0,0,0,0,0,0,0,
                 0,0.01,0,0,0,0,0,0,0,0,0,
                 0,0.05,0.2,0,0,0,0,0,0,0,0,
                 0,0.02,0,0.2,0,0,0,0,0,0,0,
                 0,0,0,0,0.2,0,0,0,0,0,0,
                 0,0,0,0,0,0.2,0,0,0,0,0,
                 0,0,0,0,0,0,0.2,0,0,0,0,
                 0,0,0,0,0,0,0,0.2,0,0,0,
                 0,0,0,0,0,0,0,0,0.2,0,0,
                 0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

infmatcat2 <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,
                    0.019,0.25,0,0,0,0,0,0,0,0,0,
                    0,0.01,0,0,0,0,0,0,0,0,0,
                    0,0.05,0.2,0,0,0,0,0,0,0,0,
                    0,0.02,0,0.2,0,0,0,0,0,0,0,
                    0,0,0,0,0.10,0,0,0,0,0,0,
                    0,0,0,0,0,0.10,0,0,0,0,0,
                    0,0,0,0,0,0,0.10,0,0,0,0,
                    0,0,0,0,0,0,0,0.10,0,0,0,
                    0,0,0,0,0,0,0,0,0.10,0,0,
                    0,0,0,0,0,0,0,0,0,0.10,0), ncol=11, byrow=T)

#### Projections over 100 year given matA (No difference between population vectors) ####

p1 <- project(uninfmat, uninfvec, 20, standard.A=T, standard.vec=T)
p2 <- project(infmat5, infvec, 20, standard.A=T, standard.vec=T)
p3 <- project(infmat10, infvec, 20, standard.A=T, standard.vec=T)
p4 <- project(infmat1, infvec, 20, standard.A=T, standard.vec=T)
p5 <- project(infmat20, infvec, 20, standard.A=T, standard.vec=T)
p6 <- project(infmat15, infvec, 20, standard.A=T, standard.vec=T)
p7 <- project(infmat25, infvec, 20, standard.A=T, standard.vec=T)

p1<- as.data.frame(p1)
p2 <- as.data.frame(p2)
p3 <- as.data.frame(p3)
p4 <- as.data.frame(p4)
p5 <- as.data.frame(p5)
p6 <- as.data.frame(p6)
p7 <- as.data.frame(p7)

p1$year <- 1:nrow(p1)
p2$year <- 1:nrow(p2)
p3$year <- 1:nrow(p3)
p4$year <- 1:nrow(p4)
p5$year <- 1:nrow(p5)
p6$year <- 1:nrow(p6)
p7$year <- 1:nrow(p7)

names(p1) <- c("popsize", "year")
names(p2) <- c("popsize", "year")
names(p3) <- c("popsize", "year")
names(p4) <- c("popsize", "year")
names(p5) <- c("popsize", "year")
names(p6) <- c("popsize", "year")
names(p7) <- c("popsize", "year")

p1$status <- rep("Negative",nrow(p1))
p2$status <- rep("Positive 5% inc mortality",nrow(p2)) 
p3$status <- rep("Positive 10% inc mortality", nrow(p3))
p4$status <- rep("Positive 1% inc mortality", nrow(p4))
p5$status <- rep("Positive 20% inc mortality", nrow(p5))
p6$status <- rep("Positive 15% inc mortality", nrow(p6))
p7$status <- rep("Positive 25% inc mortality", nrow(p7))

pcomb <- rbind(p1, p2, p3, p4, p5, p6, p7)

pcomb$status <- as.factor(pcomb$status)

levels(pcomb$status) <- c("Negative","Positive 1%inc mortality","Positive 5% inc mortality",
                                               "Positive 10% inc mortality","Positive 15% inc mortality",
                                               "Positive 20% inc mortality","Positive 25% inc mortality")

pcomb %>% 
  ggplot(aes(x=year, y=popsize, col=status)) +
  geom_line(size=0.9) +
  theme_bw(base_size = 14, base_family = 'Helvetica') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  scale_color_brewer(type = "seq", palette = "RdBu", name = "Population Disease \n            Status" )+
  xlab("Projected year") +
  ylab("Projected population size")

#### Stats ####

m1 <- lm(pcomb$popsize ~ pcomb$status)

summary(m1)

summary(aov(popsize ~ status, pcomb))

#### Plot Negative vs 5% Decline ####

pnegfive <- rbind(p1, p2)

pnegfive %>% 
  ggplot(aes(x=year, y=popsize, col=status)) +
  geom_line(size=0.9) +
  theme_bw(base_size = 14, base_family = 'Helvetica') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  scale_color_brewer(type = "seq", palette = "RdBu", name = "Population Disease \n            Status" )+
  xlab("Projected year") +
  ylab("Projected population size")

#### STOCHASTIC PROJECTIONS ####

#### list of unifected matrices ####

uninfmats <- list(uninfmat,uninfspawn)

#### list of infected matrices ####

fivemats <- list(infmat5,infmat5spawn)

#### list of infected matrices + mass mortality ####

fivemassmats <- list(infmat5,infmat5spawn,infmatmass1)

#### list of infected matrices + mass mortality && spawning failure in a given year ####

fivecatmats <- list(infmat5,infmat5spawn, infmatmass1, infmatcat1)

#### list of matrix probabilities(90%chance of successful reproduction) ####

probs <- c(0.9,0.1)

#### list_of_matrix_probabilities_inc_mass_mortality(80%chance_of_successful_reproduction_10%_reproductive_failure_10%_mass_mortality) ####

probsmass <- c(0.8,0.1,0.1)

#### list of matrix probabilities inc mass mortality and concurrent threats (75% of successful reproduction, 10% of reproductive failure, 10% chance of mass mortality and 5% chance of concurrent threat) ####

probscat <- c(0.75, 0.1, 0.1, 0.05)

#### sum_weight(vector_to_eliminate_non_adult_classes_from_carrying_capacity_calcs) ####

sumweights <- c(1,1,0,0,0,0,0,0,0,0,0)

#### 5% annual increased mortality ####

sp1 <- stoch.projection(uninfmats, uninfvec, tmax = 100, nreps = 5000, prob = probs, nmax = 200, sumweight = sumweights)
sp2 <- stoch.projection(fivemats, uninfvec, tmax = 100, nreps = 5000, prob = probs, nmax = 200, sumweight = sumweights)
sp3 <- stoch.projection(fivemassmats, uninfvec, tmax = 100, nreps = 5000, prob = probsmass, nmax = 200, sumweight = sumweights)
sp4 <- stoch.projection(fivecatmats, uninfvec, tmax = 100, nreps = 5000, prob = probscat, nmax = 200, sumweight = sumweights)

dfsp1 <- as.data.frame(sp1)
dfsp2 <- as.data.frame(sp2)
dfsp3 <- as.data.frame(sp3)
dfsp4 <- as.data.frame(sp4)

dfsp1$Status <- ("Negative")
dfsp2$Status <- ("Positive")
dfsp3$Status <- ("Positive A")
dfsp4$Status <- ("Positive B")

df <- rbind(dfsp1, dfsp2, dfsp3, dfsp4)

ggplot(df) +
  geom_freqpoly(aes(rowSums(select(df, -Status)), 
                    colour = Status), 
                binwidth = 18, size = 0.9) + 
  xlab("Population Size") + 
  ylab("Frequency") + 
  scale_colour_manual(values=c("#2166AC", 
                               "#B2182B", 
                               "#D6604D", 
                               "#F4A582"), 
                      name="Population Disease\n            Status") +  
  theme_bw(base_size = 14, base_family = 'Helvetica') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#### Summary numbers ####
numsp1 <- as.data.frame(sp1)
numsp2 <- as.data.frame(sp2)
numsp3 <- as.data.frame(sp3)
numsp4 <- as.data.frame(sp4)

numsp1$popsize <- rowSums(numsp1)
numsp2$popsize <- rowSums(numsp2)
numsp3$popsize <- rowSums(numsp3)
numsp4$popsize <- rowSums(numsp4)

numsp1 %>% summarise(carrying_capacity = sum(popsize > 200), 
                    upper_mid = sum(popsize < 200) - sum(popsize < 150),
                    mid = sum(popsize < 150) - sum(popsize < 100),
                    lower_mid = sum(popsize <100) - sum(popsize <50), 
                    lower = sum(popsize <50) - sum(popsize < 1),
                    extinct = sum(popsize < 1))

numsp2 %>% summarise(carrying_capacity = sum(popsize > 200), 
                    upper_mid = sum(popsize < 200) - sum(popsize < 150),
                    mid = sum(popsize < 150) - sum(popsize < 100),
                    lower_mid = sum(popsize <100) - sum(popsize <50), 
                    lower = sum(popsize <50) - sum(popsize < 1),
                    extinct = sum(popsize < 1))

numsp3 %>% summarise(carrying_capacity = sum(popsize > 200), 
                    upper_mid = sum(popsize < 200) - sum(popsize < 150),
                    mid = sum(popsize < 150) - sum(popsize < 100),
                    lower_mid = sum(popsize <100) - sum(popsize <50), 
                    lower = sum(popsize <50) - sum(popsize < 1),
                    extinct = sum(popsize < 1))

numsp4 %>% summarise(carrying_capacity = sum(popsize > 200), 
                    upper_mid = sum(popsize < 200) - sum(popsize < 150),
                    mid = sum(popsize < 150) - sum(popsize < 100),
                    lower_mid = sum(popsize <100) - sum(popsize <50), 
                    lower = sum(popsize <50) - sum(popsize < 1),
                    extinct = sum(popsize < 1))

#####Stochastic_growth_rates

#Marginal_growth_in_uninfected_populations
stoch.growth.rate(uninfmats, prob = probs, maxt = 100, verbose=FALSE)

#Very_marginal_decline_in_infected_populations
stoch.growth.rate(infmats, prob = probs, maxt = 100, verbose=FALSE)

#Maringal_decline_in_infected_populatioins_+_mass_mortatlity
stoch.growth.rate(infmassmats, prob= probsmass, maxt = 100, verbose=FALSE)

#Marignal_decline_in_infected_populations_+_mass_mortatlity&reproductive_failure
stoch.growth.rate(infcatmats, prob= probscat, maxt = 100, verbose=FALSE)

