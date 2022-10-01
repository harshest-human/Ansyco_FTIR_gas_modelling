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



#CO2 at different speeds
ggline(FTIR_input, x="height", y="CO2",
       add = "mean_se",
       size = 0.5,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+ ggtitle("CO2") +theme_bw()
        

#NH3 at different speeds
ggline(FTIR_input, x="height", y="NH3",
       add = "mean_se",
       size = 0.5,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+ ggtitle("NH3") +theme_bw()

#CH4 at different speeds
ggline(FTIR_input, x="height", y="CH4",
       add = "mean_se",
       size = 0.5,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+ ggtitle("CH4") +theme_bw()
