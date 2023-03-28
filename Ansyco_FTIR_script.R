########### PACKAGES ######################
getwd()
library(tidyverse)
library(psych)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(writexl)

########### FTIR DATA IMPORT ###############
FTIR_raw <- read.delim("FTIR_final_data.txt") 
FTIR_input <- FTIR_raw %>%                                   #Filtering Southwestern data
        filter((FTIR_raw$wind_direction >= 160) 
               & 
               (FTIR_raw$wind_direction <= 270))

FTIR_input$wd_speed <- ifelse(FTIR_input$wind_speed > 3, "high", "low") #Sorting wind speeds

#strings
FTIR_input$Samp_loc <- as.factor(FTIR_input$Samp_loc)
FTIR_input$Messstelle <- as.factor(FTIR_input$Messstelle)
FTIR_input$height <- as.factor(FTIR_input$height)
FTIR_input$wd_speed <- as.factor(FTIR_input$wd_speed)
FTIR_input$wd_cardinals <- as.factor(FTIR_input$wd_cardinals)


########### DATA Visualization 1 (ggline::ggpubr) ###############
#CO2 at different heights
ggline(FTIR_input, x="height", y="CO2",
       add = "mean_se",
       shape = 2,
       point.size = 1.5,
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+
        theme_bw() + theme(legend.position="False")+
        xlab("Height  (meters)")+
        ylab("CO2  (ppm)")

#NH3 at different heights
ggline(FTIR_input, x="height", y="NH3",
       add = "mean_se",
       shape = 21,
       point.size = 1.5,
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+ theme_bw()+
        theme(legend.position="False")+
        xlab("Height  (meters)")+
        ylab("NH3  (ppm)")

#CH4 at different heights
ggline(FTIR_input, x="height", y="CH4",
       add = "mean_se",
       shape = 7,
       point.size = 1.5,
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+ theme_bw()+
        theme(legend.position="False")+
        xlab("Height  (meters)")+
        ylab("CH4  (ppm)")

########### DATA Visualization 2 (ggline::ggpubr) ###############
#CO2 at different speeds
ggline(FTIR_input, x="height", y="CO2",
       add = "mean_se",
       shape = 2,
       point.size = 1.5,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+
        theme_bw()+theme(legend.position="False")+
        xlab("Height  (meters)")+
        ylab("CO2  (ppm)")

#NH3 at different speeds
ggline(FTIR_input, x="height", y="NH3",
       add = "mean_se",
       shape = 21,
       point.size = 1.5,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+ theme_bw()+
        theme(legend.position="False")+
        xlab("Height  (meters)")+
        ylab("NH3  (ppm)")

#CH4 at different speeds
ggline(FTIR_input, x="height", y="CH4",
       add = "mean_se",
       shape = 7,
       point.size = 1.5,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+ theme_bw()+
        theme(legend.position="False")+
        xlab("Height  (meters)")+
        ylab("CH4  (ppm)")


########### DATA Modeling ###############
#ANOVA Model SS1
FTIR_SS1 <- FTIR_input %>% filter(Samp_loc == "SS1")
anova(aov(CO2~height*wd_speed, data=FTIR_SS1)) # alternative: kruskal.test(CO2 ~ height, FTIR_SS1)
anova(aov(CH4~height*wd_speed, data=FTIR_SS1))
anova(aov(NH3~height*wd_speed, data=FTIR_SS1))

#ANOVA Model SS2
FTIR_SS2 <- FTIR_input %>% filter(Samp_loc == "SS2")
anova(aov(CO2~height*wd_speed, data=FTIR_SS2))
anova(aov(CH4~height*wd_speed, data=FTIR_SS2))
anova(aov(NH3~height*wd_speed, data=FTIR_SS2))

#Regression Model SS1 (to find percentage difference)
summary(lm(CO2~height, data=FTIR_SS1))
summary(lm(CH4~height, data=FTIR_SS1))
summary(lm(NH3~height, data=FTIR_SS1))

#Regression Model SS2 (to find percentage difference)
summary(lm(CO2~height, data=FTIR_SS2))
summary(lm(CH4~height, data=FTIR_SS2))
summary(lm(NH3~height, data=FTIR_SS2)) 

#Regression Model with wd_speed SS1 (to find percentage difference)
summary(lm(CO2~height*wd_speed, data=FTIR_SS1))
summary(lm(CH4~height*wd_speed, data=FTIR_SS1))
summary(lm(NH3~height*wd_speed, data=FTIR_SS1))

#Regression Model with wd_speed SS2 (to find percentage difference)
summary(lm(CO2~height*wd_speed, data=FTIR_SS2))
summary(lm(CH4~height*wd_speed, data=FTIR_SS2))
summary(lm(NH3~height*wd_speed, data=FTIR_SS2)) 

########### Relative errors###############
Tib_SS1 <- FTIR_SS1 %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

Tib_SS2 <- FTIR_SS2 %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

#Calculating relative error as a percentage of the correct value of H=0.6
Tib_SS1 <- Tib_SS1 %>% 
        mutate(error_CO2 = abs(CO2 - CO2[height == 0.6]) / mean(CO2) * 100 * ifelse(CO2-CO2[height == 0.6] < 0, -1, 1),
               error_CH4 = abs(CH4 - CH4[height == 0.6]) / mean(CH4) * 100 * ifelse(CH4-CH4[height == 0.6] < 0, -1, 1),
               error_NH3 = abs(NH3 - NH3[height == 0.6]) / mean(NH3) * 100 * ifelse(NH3-NH3[height == 0.6] < 0, -1, 1))

Tib_SS2 <- Tib_SS2 %>% 
        mutate(error_CO2 = abs(CO2 - CO2[height == 0.6]) / mean(CO2) * 100 * ifelse(CO2-CO2[height == 0.6] < 0, -1, 1),
               error_CH4 = abs(CH4 - CH4[height == 0.6]) / mean(CH4) * 100 * ifelse(CH4-CH4[height == 0.6] < 0, -1, 1),
               error_NH3 = abs(NH3 - NH3[height == 0.6]) / mean(NH3) * 100 * ifelse(NH3-NH3[height == 0.6] < 0, -1, 1))

########### Relative errors SS1###############
Tib_SS1_low <- FTIR_SS1 %>% filter(wd_speed == "low") %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

Tib_SS1_high <- FTIR_SS1 %>% filter(wd_speed == "high") %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

#Calculating relative error as a percentage of the correct value of H=0.6
Tib_SS1_low<- Tib_SS1_low %>% 
        mutate(error_CO2 = abs(CO2 - CO2[height == 0.6]) / mean(CO2) * 100 * ifelse(CO2-CO2[height == 0.6] < 0, -1, 1),
               error_CH4 = abs(CH4 - CH4[height == 0.6]) / mean(CH4) * 100 * ifelse(CH4-CH4[height == 0.6] < 0, -1, 1),
               error_NH3 = abs(NH3 - NH3[height == 0.6]) / mean(NH3) * 100 * ifelse(NH3-NH3[height == 0.6] < 0, -1, 1))

Tib_SS1_high <-Tib_SS1_high  %>% 
        mutate(error_CO2 = abs(CO2 - CO2[height == 0.6]) / mean(CO2) * 100 * ifelse(CO2-CO2[height == 0.6] < 0, -1, 1),
               error_CH4 = abs(CH4 - CH4[height == 0.6]) / mean(CH4) * 100 * ifelse(CH4-CH4[height == 0.6] < 0, -1, 1),
               error_NH3 = abs(NH3 - NH3[height == 0.6]) / mean(NH3) * 100 * ifelse(NH3-NH3[height == 0.6] < 0, -1, 1))

########### Relative errors SS2###############
Tib_SS2_low <- FTIR_SS2 %>% filter(wd_speed == "low") %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

Tib_SS2_high <- FTIR_SS2 %>% filter(wd_speed == "high") %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

#Calculating relative error as a percentage of the correct value of H=0.6
Tib_SS2_low<- Tib_SS2_low %>% 
        mutate(error_CO2 = abs(CO2 - CO2[height == 0.6]) / mean(CO2) * 100 * ifelse(CO2-CO2[height == 0.6] < 0, -1, 1),
               error_CH4 = abs(CH4 - CH4[height == 0.6]) / mean(CH4) * 100 * ifelse(CH4-CH4[height == 0.6] < 0, -1, 1),
               error_NH3 = abs(NH3 - NH3[height == 0.6]) / mean(NH3) * 100 * ifelse(NH3-NH3[height == 0.6] < 0, -1, 1))

Tib_SS2_high <-Tib_SS2_high  %>% 
        mutate(error_CO2 = abs(CO2 - CO2[height == 0.6]) / mean(CO2) * 100 * ifelse(CO2-CO2[height == 0.6] < 0, -1, 1),
               error_CH4 = abs(CH4 - CH4[height == 0.6]) / mean(CH4) * 100 * ifelse(CH4-CH4[height == 0.6] < 0, -1, 1),
               error_NH3 = abs(NH3 - NH3[height == 0.6]) / mean(NH3) * 100 * ifelse(NH3-NH3[height == 0.6] < 0, -1, 1))


########### Calculating Mixing ratios ###############
FTIR_input <- FTIR_input %>%
        group_by(height) %>%
        mutate(GC_ratio = round(CH4/NH3, 2))


#Plotting CH4/NH3 Mixing ratios at different heights
ggline(FTIR_input, x="height", y="GC_ratio",
       add = "mean_se",
       shape = 22,
       point.size = 2.5,
       width=1.5)+
        theme_bw() + theme(legend.position="top")+
        xlab("Height  (meters)")+
        ylab("Ratios")

#With Sampling location information 
ggline(FTIR_input, x="height", y="GC_ratio",
       add = "mean_se",
       shape = 22,
       point.size = 2.5,
       width=1.5,
       facet.by ="Samp_loc")+
        theme_bw() + theme(legend.position="False")+
        xlab("Height  (meters)")+
        ylab("Ratios")

#Without Wind speed information 
ggline(FTIR_input, x="height", y="GC_ratio",
       add = "mean_se",
       shape = 22,
       point.size = 2.5,
       facet.by ="Samp_loc",
       color ="wd_speed",
       width=1.5,
       position = position_dodge(w=0.15))+
        theme_bw() + theme(legend.position="False")+
        xlab("Height  (meters)")+
        ylab("Ratio")

######ANOVA Model for Mixing ratios#####
FTIR_SS1 <- FTIR_input %>% filter(Samp_loc == "SS1")
FTIR_SS2 <- FTIR_input %>% filter(Samp_loc == "SS2")

anova(aov(GC_ratio~height, data=FTIR_input))
anova(aov(GC_ratio~height, data=FTIR_SS1))
anova(aov(GC_ratio~height, data=FTIR_SS2))

######Regression Model for Mixing ratios####
summary(lm(GC_ratio~height, data=FTIR_input))
summary(lm(GC_ratio~height, data=FTIR_SS1))
summary(lm(GC_ratio~height, data=FTIR_SS2))

####Coef. of Var of each gas at each height####
Group_stats <- FTIR_input %>% group_by(height) %>% 
        summarize(CO2_mean = mean(CO2), CO2_sd = sd(CO2),
                  CH4_mean = mean(CH4), CH4_sd = sd(CH4),
                  NH3_mean = mean(NH3), NH3_sd = sd(NH3),
                  GC_mean = mean(GC_ratio), GC_sd = sd(GC_ratio))

#Coeffecient of Variation of CO2, CH4, NH3 at each height
CV <- Group_stats %>%
        group_by(height) %>%
        mutate(cv_CO2 = ((CO2_sd/CO2_mean)*100),
               cv_CH4 = ((CH4_sd/CH4_mean)*100),
               cv_NH3 = ((NH3_sd/NH3_mean)*100))
print(CV)


######## Distribution of Data###########
#qqnorm(FTIR_input$NH3)



######EXTRAS: Create the speed column using cut()####
#speed_breaks <- c(0, 1, 2, 3, 4, 5, 6, 7, 8,9, 10,11, 12, Inf)
#speed_labels <- c(0:12)

#FTIR_input$wd_speed <- cut(FTIR_input$wind_speed, breaks = speed_breaks, labels = speed_labels)

# check number of observations for each wind speed level
#table(FTIR_input$wd_speed)


