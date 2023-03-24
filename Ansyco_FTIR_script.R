########### PACKAGES ######################
getwd()
library(tidyverse)
library(psych)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggpubr)
library(writexl)


########### FTIR DATA IMPORT ###############
FTIR_raw <- read_excel("FTIR_final_data.xlsx") 
FTIR_input <- FTIR_raw %>%                                   #Filtering Southwestern data
        filter((FTIR_raw$wind_direction >= 160) 
               & 
               (FTIR_raw$wind_direction <= 270))


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
        theme(legend.position="top")+
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
anova(aov(CO2~height*wd_speed, data=FTIR_SS1))
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

########### DATA mean tibble height###############
Tib_SS1 <- FTIR_SS1 %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

Tib_SS2 <- FTIR_SS2 %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

#write_xlsx(Tib_SS1, "Tib_SS1_data.xlsx")
#write_xlsx(Tib_SS2, "Tib_SS2_data.xlsx")

########### DATA mean tibble height & wd_speed (SS1) ###############
Tib_SS1_low <- FTIR_SS1 %>% filter(wd_speed == "low") %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

Tib_SS1_high <- FTIR_SS1 %>% filter(wd_speed == "high") %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

write_xlsx(Tib_SS1_low, "Tib_SS1_low_data.xlsx")
write_xlsx(Tib_SS1_high, "Tib_SS1_high_data.xlsx")


########### DATA mean tibble height & wd_speed (SS2) ###############
Tib_SS2_low <- FTIR_SS2 %>% filter(wd_speed == "low") %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

Tib_SS2_high <- FTIR_SS2 %>% filter(wd_speed == "high") %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

#write_xlsx(Tib_SS2_low, "Tib_SS2_low_data.xlsx")
#write_xlsx(Tib_SS2_high, "Tib_SS2_high_data.xlsx")



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
        theme_bw() + theme(legend.position="top")+
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
        theme_bw() + theme(legend.position="top")+
        xlab("Height  (meters)")+
        ylab("Ratios")

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

####mean and SD of CO2, CH4, NH3 at each height####
Group_stats <- FTIR_input %>% group_by(height) %>% 
        summarize(CO2_mean = mean(CO2), CO2_sd = sd(CO2),
                  CH4_mean = mean(CH4), CH4_sd = sd(CH4),
                  NH3_mean = mean(NH3), NH3_sd = sd(NH3),
                  GC_mean = mean(GC_ratio), GC_sd = sd(GC_ratio))

#write_xlsx(Group_stats, "stat_SS1_.xlsx")

#Coeffecient of Variation of CO2, CH4, NH3 at each height
CV <- Group_stats %>%
        group_by(height) %>%
        mutate(cv_CO2 = ((CO2_sd/CO2_mean)*100),
               cv_CH4 = ((CH4_sd/CH4_mean)*100),
               cv_NH3 = ((NH3_sd/NH3_mean)*100))
print(CV)




#####Calculating relative error as a percentage of the correct value of H=0.6#####
Tib_SS2_low <- FTIR_SS2 %>% filter(wd_speed == "low") %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

Tib_error<- Tib_SS2_low %>% mutate(error_CO2 = abs(CO2 - 648) / 648 * 100,
                                   error_CH4 = abs(CH4 - 16.8) / 16.8 * 100,
                                   error_NH3 = abs(NH3 - 2.7) / 2.7 * 100)



