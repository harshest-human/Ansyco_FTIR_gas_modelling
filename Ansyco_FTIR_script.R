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
FTIR_input <- read_excel("FTIR_final_data.xlsx") %>% filter(wd_cardinals == "Southern")

#strings
FTIR_input$Samp_loc <- as.factor(FTIR_input$Samp_loc)
FTIR_input$Messstelle <- as.factor(FTIR_input$Messstelle)
FTIR_input$height <- as.factor(FTIR_input$height)
FTIR_input$wd_speed <- as.factor(FTIR_input$wd_speed)
FTIR_input$wd_cardinals <- as.factor(FTIR_input$wd_cardinals)


########### DATA Visualization (ggline::ggpubr) ###############
#CO2 at different speeds
ggline(FTIR_input, x="height", y="CO2",
       add = "mean_se",
       shape = 2,
       point.size = 1,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+ theme_bw() + theme(legend.position="top")

#NH3 at different speeds
ggline(FTIR_input, x="height", y="NH3",
       add = "mean_se",
       shape = 21,
       point.size = 1,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+ theme_bw() + theme(legend.position="top")

#CH4 at different speeds
ggline(FTIR_input, x="height", y="CH4",
       add = "mean_se",
       shape = 7,
       point.size = 1,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+ theme_bw() + theme(legend.position="top")


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

#Regression Model SS1 (to find percentage difference)
summary(lm(CO2~wd_speed, data=FTIR_SS1))
summary(lm(CH4~wd_speed, data=FTIR_SS1))
summary(lm(NH3~wd_speed, data=FTIR_SS1))

#Regression Model SS2 (to find percentage difference)
summary(lm(CO2~wd_speed, data=FTIR_SS2))
summary(lm(CH4~wd_speed, data=FTIR_SS2))
summary(lm(NH3~wd_speed, data=FTIR_SS2)) 


########### DATA mean tibble ###############
Tib_SS1 <- FTIR_SS1 %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

write_xlsx(Tib_SS1, "Tib_SS1_data.xlsx")

Tib_SS2 <- FTIR_SS2 %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

write_xlsx(Tib_SS2, "Tib_SS2_data.xlsx")


Tib_SS2_low <- FTIR_SS2 %>% filter(wd_speed == "low") %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

write_xlsx(Tib_SS2_low, "Tib_SS2_low_data.xlsx")

Tib_SS2_high <- FTIR_SS2 %>% filter(wd_speed == "high") %>% group_by(height) %>%
        summarise(CO2=mean(CO2),
                  CH4=mean(CH4),
                  NH3=mean(NH3))

write_xlsx(Tib_SS2_high, "Tib_SS2_high_data.xlsx")


