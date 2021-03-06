########### PACKAGES ######################
getwd()
library(tidyverse)
library(reshape2)
library(hablar)
library(lubridate)
library(psych)
library(rmarkdown)
library(ggplot2)
library(readxl)
library(dplyr)
library(openair)
library(car)
library(xlsx)
library(ggpubr)
library(outliers)
library(gtsummary)
library(Rmisc) #to summarize and find confidence intervals

########### FTIR DATA IMPORT ###############
FTIR_input <- read.table(paste0("20210902_Vertical_Pipes_Harsh_06nov.txt"), header = T, fill = TRUE) %>%
        mutate(DateTime = paste(Datum, " ", Zeit)) %>%
        relocate(DateTime)

 #Categorize Messstelle into actual heights in meters
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==1]  = "0.60"
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==2]  = "0.90"
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==3]  = "1.50"
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==4]  = "1.80"
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==5]  = "2.40"
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==6]  = "2.70"
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==7]  = "0.60"
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==8]  = "0.90"
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==9]  = "1.50"
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==10] = "1.80"
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==11] = "2.40"
FTIR_input$height[as.numeric(FTIR_input$Messstelle)==12] = "2.70"

 #Categorize Messstelle into Samp_loc_location
FTIR_input$Samp_loc[as.numeric(FTIR_input$Messstelle)>=7]  = "SS1"
FTIR_input$Samp_loc[as.numeric(FTIR_input$Messstelle)<=6]  = "SS2" 

 #Manipulation of strings
FTIR_input$height <- as.numeric(FTIR_input$height)
FTIR_input$CO2 <- as.numeric(FTIR_input$CO2)
FTIR_input$NH3 <- as.numeric(FTIR_input$NH3) 
FTIR_input$CH4 <- as.numeric(FTIR_input$CH4) 
FTIR_input$Messstelle <- as.numeric(FTIR_input$Messstelle)
FTIR_input$DateTime <- ymd_hms(FTIR_input$DateTime)
FTIR_input$DateTime_FI3min = round_date(FTIR_input$DateTime, "3 minutes")

 # FTIR Data Frame
FTIR_input <- select(FTIR_input, 
                     DateTime_FI3min,Samp_loc,Messstelle,height,CO2,CH4,NH3) %>% 
        convert(fct(Samp_loc)) %>% na.omit()


########### WIND & DWD DATA IMPORT ########
wind_input <- read.table("D:/HARSHY DATA 2020/Master Thesis/USA Windmast data/USA_Anemometer_wind_modelling/wind_WD_WS_data.txt")
wind_input$DateTime_WI3min <- ymd_hms(wind_input$DateTime_WI3min)
DWD_input <- read.table("D:/HARSHY DATA 2020/Master Thesis/USA Windmast data/USA_Anemometer_wind_modelling/DWD_interpolated.txt")
DWD_input$MESS_DATUM <- ymd_hms(DWD_input$MESS_DATUM)
 
 #FTIR+WIND 
FTIR_02SEP_06OCT <- select(FTIR_input, 
                           DateTime_FI3min,Samp_loc,Messstelle,height,CO2,CH4,NH3)%>% 
        filter(DateTime_FI3min >= ymd_hms("2021-09-02 11:42:00"),
               DateTime_FI3min <= ymd_hms("2021-10-06 11:21:00"))
FTIRxwind <- left_join(FTIR_02SEP_06OCT, wind_input, 
                       by = c("DateTime_FI3min" = "DateTime_WI3min"))

 #FTIR+DWD
FTIR_06OCT_06NOV <- select(FTIR_input, 
                           DateTime_FI3min,Samp_loc,Messstelle,height,CO2,CH4,NH3)%>% 
        filter(DateTime_FI3min >= ymd_hms("2021-10-06 11:24:00"),
               DateTime_FI3min <= ymd_hms("2021-11-06 11:21:00"))
FTIRxDWD <- left_join(FTIR_06OCT_06NOV,DWD_input, 
                      by = c("DateTime_FI3min" = "MESS_DATUM"))


########### FINAL DATA FRAME ###################
FTIRxwindxDWD <- rbind(FTIRxwind,FTIRxDWD)

 #Integration of 16 wind_cardinals
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "N"]  = "Northern"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NE"]  = "Northern"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NNE"]  = "Northern"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "ENE"]  = "Northern"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NNW"]  = "Northern"

FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "S"]  = "Southern"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SSE"]  = "Southern"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SW"]  = "Southern"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SSW"]  = "Southern"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "WSW"]  = "Southern"

FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "E"]  = "East"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "ESE"]  = "East"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SE"]  = "East"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "W"]  = "West"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NW"]  = "West"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "WNW"]  = "West"


 #Integration of 4 wind_speeds
FTIRxwindxDWD$wd_speed[as.character(FTIRxwindxDWD$wind_speed)> "0-5"]  = "low"
FTIRxwindxDWD$wd_speed[as.character(FTIRxwindxDWD$wind_speed)> "5-14"]  = "high"

FTIRxwindxDWD$wind_speed <- as.numeric(FTIRxwindxDWD$wind_speed)
FTIRxwindxDWD$wd_speed <- as.factor(FTIRxwindxDWD$wd_speed)

 #Final Dataframe
FTIRxwindxDWD <- select(FTIRxwindxDWD,
                        DateTime_FI3min,Samp_loc,Messstelle,
                        height,CO2,CH4,NH3,
                        wind_direction,wind_speed,wd_speed,
                        wd_cardinals,-wd_cardinal) %>% 
        filter(DateTime_FI3min >= ymd_hms("2021-09-02 11:57:00"),
               DateTime_FI3min <= ymd_hms("2021-11-06 11:18:00")) %>% na.omit()

 #Remove Outliers
Remove_outliers_function <- source("remove_outliers_function.R")
FTIRxwindxDWD$CO2 <- remove_outliers(FTIRxwindxDWD$CO2)
FTIRxwindxDWD$CH4 <- remove_outliers(FTIRxwindxDWD$CH4)
FTIRxwindxDWD$NH3 <- remove_outliers(FTIRxwindxDWD$NH3)


########### WIND_GRAPH ######################
windRose(FTIRxwindxDWD  , ws = "wind_speed", wd = "wind_direction",
         breaks = c(0,2,4,6,8,12),
         auto.text = FALSE,
         paddle = FALSE,
         grid.line = 5,
         key = list(lables = c(">0 - 2",
                               ">2 - 4",
                               ">4 - 6",
                               ">6 - 8",
                               ">8 - 12")),
         key.header = "02.09.2021 - 06.11.2021",
         key.footer = "Wind_speed (m/s)",
         key.position = "bottom",
         par.settings=list(axis.line=list(col="lightgray")),
         col = c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f", "#d7153a"))


##################  MODELING STRATEGY 1 ############################
summary(lm(CO2~height, data=FTIRxwindxDWD))
anova(aov(CO2~as.factor(height), data=FTIRxwindxDWD))
TukeyHSD(aov(CO2~as.factor(height), data=FTIRxwindxDWD))


summary(lm(CH4~height, data=FTIR_SW_SS1))
anova(aov(CH4~as.factor(height), data=FTIRxwindxDWD))
TukeyHSD(aov(CH4~as.factor(height), data=FTIRxwindxDWD))


summary(lm(NH3~height, data=FTIR_SW_SS1))
anova(aov(NH3~as.factor(height), data=FTIRxwindxDWD))
TukeyHSD(aov(NH3~as.factor(height), data=FTIRxwindxDWD))


##################  MODELING STRATEGY 2 ############################
summary(lm(CO2~height+Samp_loc, data=FTIRxwindxDWD))
anova(aov(CO2~as.factor(height)*Samp_loc, data=FTIRxwindxDWD))
compare_means(CO2~Samp_loc, data=FTIRxwindxDWD, group.by = "height", method = "t.test")

summary(lm(CH4~height+Samp_loc, data=FTIRxwindxDWD))
anova(aov(CH4~as.factor(height)*Samp_loc, data=FTIRxwindxDWD))
compare_means(CH4~Samp_loc, data=FTIRxwindxDWD, group.by = "height", method = "t.test")

summary(lm(CH4~height+Samp_loc, data=FTIRxwindxDWD))
anova(aov(NH3~as.factor(height)*Samp_loc, data=FTIRxwindxDWD))
compare_means(NH3~Samp_loc, data=FTIRxwindxDWD, group.by = "height", method = "t.test")

##################  MODELING STRATEGY 3 SS1 #########################
FTIR_SW_NE <- FTIRxwindxDWD %>% filter(wd_cardinals== c("Northern","Southern"))
FTIR_SW_NE_SS1 <- FTIR_SW_NE %>% filter(Samp_loc == "SS1")


summary(lm(CO2~as.factor(height)+as.factor(wd_cardinals), data=FTIR_SW_NE_SS1))
anova(aov(CO2~as.factor(height)*as.factor(wd_cardinals), data=FTIR_SW_NE_SS1))
compare_means(CO2~wd_cardinals, data=FTIR_SW_NE_SS1, group.by = "height", method = "t.test")

summary(lm(CH4~as.factor(height)+as.factor(wd_cardinals), data=FTIR_SW_NE_SS1))
anova(aov(CH4~as.factor(height)*as.factor(wd_cardinals), data=FTIR_SW_NE_SS1))
compare_means(CH4~wd_cardinals, data=FTIR_SW_NE_SS1, group.by = "height", method = "t.test")

summary(lm(NH3~as.factor(height)+as.factor(wd_cardinals), data=FTIR_SW_NE_SS1))
anova(aov(NH3~as.factor(height)*as.factor(wd_cardinals), data=FTIR_SW_NE_SS1))
compare_means(NH3~wd_cardinals, data=FTIR_SW_NE_SS1, group.by = "height", method = "t.test")


##################  MODELING STRATEGY 3 SS2 #########################
FTIR_SW_NE <- FTIRxwindxDWD %>% filter(wd_cardinals== c("Northern","Southern"))
FTIR_SW_NE_SS2 <- FTIR_SW_NE %>% filter(Samp_loc == "SS2")

summary(lm(CO2~as.factor(height)+as.factor(wd_cardinals), data=FTIR_SW_NE_SS2))
anova(aov(CO2~as.factor(height)*as.factor(wd_cardinals), data=FTIR_SW_NE_SS2))
compare_means(CO2~wd_cardinals, data=FTIR_SW_NE_SS2, group.by = "height", method = "t.test")

summary(lm(CH4~as.factor(height)+as.factor(wd_cardinals), data=FTIR_SW_NE_SS2))
anova(aov(CH4~as.factor(height)*as.factor(wd_cardinals), data=FTIR_SW_NE_SS2))
compare_means(CH4~wd_cardinals, data=FTIR_SW_NE_SS2, group.by = "height", method = "t.test")

summary(lm(NH3~as.factor(height)+as.factor(wd_cardinals), data=FTIR_SW_NE_SS2))
anova(aov(NH3~as.factor(height)*as.factor(wd_cardinals), data=FTIR_SW_NE_SS2))
compare_means(NH3~wd_cardinals, data=FTIR_SW_NE_SS2, group.by = "height", method = "t.test")


##################  MODELING STRATEGY 4 SS1 #########################
FTIR_SW_SS1<- FTIR_SW_NE_SS1 %>% filter(wd_cardinals == "Southern")

summary(lm(CO2~height*wd_speed, data=FTIR_SW_SS1))
anova(aov(CO2~height*wd_speed, data=FTIR_SW_SS1))
compare_means(CO2~wd_speed, data=FTIR_SW_SS1, group.by = "height", method = "t.test")

summary(lm(CH4~height*wd_speed, data=FTIR_SW_SS1))
anova(aov(CH4~height*wd_speed, data=FTIR_SW_SS1))
compare_means(CH4~wd_speed, data=FTIR_SW_SS1, group.by = "height", method = "t.test")

summary(lm(NH3~height*wd_speed, data=FTIR_SW_SS1))
anova(aov(NH3~height*wd_speed, data=FTIR_SW_SS1))
compare_means(NH3~wd_speed, data=FTIR_SW_SS1, group.by = "height", method = "t.test")

##################  MODELING STRATEGY 4 SS2 #########################
FTIR_SW_SS2<- FTIR_SW_NE_SS2 %>% filter(wd_cardinals == "Southern")

summary(lm(CO2~height*wd_speed, data=FTIR_SW_SS2))
anova(aov(CO2~height*wd_speed, data=FTIR_SW_SS2))
compare_means(CO2~wd_speed, data=FTIR_SW_SS2, group.by = "height", method = "t.test")

summary(lm(CH4~height*wd_speed, data=FTIR_SW_SS2))
anova(aov(CH4~height*wd_speed, data=FTIR_SW_SS2))
compare_means(CH4~wd_speed, data=FTIR_SW_SS2, group.by = "height", method = "t.test")

summary(lm(NH3~height*wd_speed, data=FTIR_SW_SS2))
anova(aov(NH3~height*wd_speed, data=FTIR_SW_SS2))
compare_means(NH3~wd_speed, data=FTIR_SW_SS2, group.by = "height", method = "t.test")


##################  GRAPH STRATEGY 1 ############################
FTIRxwindxDWD$height <-as.factor(FTIRxwindxDWD$height)

strategy1_CO2 <- summarySE(FTIRxwindxDWD, measurevar="CO2", groupvars=c("height"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy1_CO2, aes(x=height, y=CO2))+ 
        geom_errorbar(aes(ymin=CO2-ci, ymax=CO2+ci), width=.2) +
        geom_point(size=3)+ 
        xlab("Height (m)") + ylab("CO2 (ppm)")+
        geom_point(size=3)+ theme_classic(base_size = 16)+
        geom_line(aes(group=T))+ geom_point()

strategy1_CH4 <- summarySE(FTIRxwindxDWD, measurevar="CH4", groupvars=c("height"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy1_CH4, aes(x=height, y=CH4))+ 
        geom_errorbar(aes(ymin=CH4-ci, ymax=CH4+ci), width=.2) +
        geom_point(size=3)+ 
        xlab("Height (m)") + ylab("CH4 (ppm)")+
        geom_point(size=3)+ theme_classic(base_size = 16)+
        geom_line(aes(group=T))+ geom_point()

strategy1_NH3 <- summarySE(FTIRxwindxDWD, measurevar="NH3", groupvars=c("height"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy1_NH3, aes(x=height, y=NH3))+ 
        geom_errorbar(aes(ymin=NH3-ci, ymax=NH3+ci), width=.2) +
        geom_point(size=3)+ 
        xlab("Height (m)") + ylab("NH3 (ppm)")+
        geom_point(size=3)+ theme_classic(base_size = 16)+
        geom_line(aes(group=T))+ geom_point()


##################  GRAPH STRATEGY 2 ############################
strategy2_CO2 <- summarySE(FTIRxwindxDWD, measurevar="CO2", groupvars=c("height","Samp_loc"),
                           na.rm = TRUE, conf.interval = 0.50, .drop = TRUE)

ggplot(strategy2_CO2, aes(x=height, y=CO2, colour=Samp_loc))+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(colour = "Setup")+ 
        geom_errorbar(aes(ymin=CO2-ci, ymax=CO2+ci), width=.2) +
        geom_point(size=3)+ theme_classic(base_size = 16)+
        geom_line(aes(group=Samp_loc))+ geom_point()

strategy2_CH4 <- summarySE(FTIRxwindxDWD, measurevar="CH4", groupvars=c("height","Samp_loc"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy2_CH4, aes(x=height, y=CH4, colour=Samp_loc))+ 
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(colour = "Setup")+ 
        geom_errorbar(aes(ymin=CH4-ci, ymax=CH4+ci), width=.2) +
        geom_point(size=3)+ theme_classic(base_size = 16)+
        geom_line(aes(group=Samp_loc))+ geom_point()

strategy2_NH3 <- summarySE(FTIRxwindxDWD, measurevar="NH3", groupvars=c("height","Samp_loc"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy2_NH3, aes(x=height, y=NH3, colour=Samp_loc))+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(colour = "Setup")+ 
        geom_errorbar(aes(ymin=NH3-ci, ymax=NH3+ci), width=.2) +
        geom_point(size=3)+ theme_classic(base_size = 16)+
        geom_line(aes(group=Samp_loc))+ geom_point()


##################  GRAPH STRATEGY 3 SS1############################
FTIR_SW_NE <- FTIRxwindxDWD %>% filter(wd_cardinals== c("Northern","Southern"))
FTIR_SW_NE_SS1 <- FTIR_SW_NE %>% filter(Samp_loc == "SS1")

strategy3_SS1_CO2 <- summarySE(FTIR_SW_NE_SS1, measurevar="CO2", groupvars=c("height","wd_cardinals"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy3_SS1_CO2, aes(x=height, y=CO2, colour=wd_cardinals))+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(colour = "Windward")+ 
        geom_errorbar(aes(ymin=CO2-ci, ymax=CO2+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#F5AAB0","#D36069"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_cardinals))+ geom_point()

strategy3_SS1_CH4 <- summarySE(FTIR_SW_NE_SS1, measurevar="CH4", groupvars=c("height","wd_cardinals"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy3_SS1_CH4, aes(x=height, y=CH4, colour=wd_cardinals))+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(colour = "Windward")+ 
        geom_errorbar(aes(ymin=CH4-ci, ymax=CH4+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#F5AAB0","#D36069"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_cardinals))+ geom_point()

strategy3_SS1_NH3 <- summarySE(FTIR_SW_NE_SS1, measurevar="NH3", groupvars=c("height","wd_cardinals"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy3_SS1_NH3, aes(x=height, y=NH3, colour=wd_cardinals))+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(colour = "Windward")+ 
        geom_errorbar(aes(ymin=NH3-ci, ymax=NH3+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#F5AAB0","#D36069"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_cardinals))+ geom_point()


##################  GRAPH STRATEGY 3 SS2############################
FTIR_SW_NE <- FTIRxwindxDWD %>% filter(wd_cardinals== c("Northern","Southern"))
FTIR_SW_NE_SS2 <- FTIR_SW_NE %>% filter(Samp_loc == "SS2")

strategy3_SS2_CO2 <- summarySE(FTIR_SW_NE_SS2, measurevar="CO2", groupvars=c("height","wd_cardinals"),
                               na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy3_SS2_CO2, aes(x=height, y=CO2, colour=wd_cardinals))+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(colour = "Windward")+ 
        geom_errorbar(aes(ymin=CO2-ci, ymax=CO2+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#A4EBEA","#2FA2A0"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_cardinals))+ geom_point()



strategy3_SS2_CH4 <- summarySE(FTIR_SW_NE_SS2, measurevar="CH4", groupvars=c("height","wd_cardinals"),
                               na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy3_SS2_CH4, aes(x=height, y=CH4, colour=wd_cardinals))+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(colour = "Windward")+ 
        geom_errorbar(aes(ymin=CH4-ci, ymax=CH4+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#A4EBEA","#2FA2A0"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_cardinals))+ geom_point()



strategy3_SS2_NH3 <- summarySE(FTIR_SW_NE_SS2, measurevar="NH3", groupvars=c("height","wd_cardinals"),
                               na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy3_SS2_NH3, aes(x=height, y=NH3, colour=wd_cardinals))+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(colour = "Windward")+ 
        geom_errorbar(aes(ymin=NH3-ci, ymax=NH3+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#A4EBEA","#2FA2A0"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_cardinals))+ geom_point()


##################  GRAPH STRATEGY 4 SS1 ############################
FTIR_SW_SS1<- FTIR_SW_NE_SS1 %>% filter(wd_cardinals == "Southern")

strategy4_SS1_CO2 <- summarySE(FTIR_SW_SS1, measurevar="CO2", groupvars=c("height","wd_speed"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy4_SS1_CO2, aes(x=height, y=CO2, colour=wd_speed))+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(colour = "Wind Speed")+ 
        geom_errorbar(aes(ymin=CO2-ci, ymax=CO2+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#F5AAB0","#D36069"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_speed))+ geom_point()

strategy4_SS1_CH4 <- summarySE(FTIR_SW_SS1, measurevar="CH4", groupvars=c("height","wd_speed"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy4_SS1_CH4, aes(x=height, y=CH4, colour=wd_speed))+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(colour = "Wind Speed")+ 
        geom_errorbar(aes(ymin=CH4-ci, ymax=CH4+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#F5AAB0","#D36069"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_speed))+ geom_point()

strategy4_SS1_NH3 <- summarySE(FTIR_SW_SS1, measurevar="NH3", groupvars=c("height","wd_speed"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy4_SS1_NH3, aes(x=height, y=NH3, colour=wd_speed))+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(colour = "Wind Speed")+ 
        geom_errorbar(aes(ymin=NH3-ci, ymax=NH3+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#F5AAB0","#D36069"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_speed))+ geom_point()


##################  GRAPH STRATEGY 4 SS2############################
FTIR_SW_SS2<- FTIR_SW_NE_SS2 %>% filter(wd_cardinals == "Southern")

strategy4_SS2_CO2 <- summarySE(FTIR_SW_NE_SS2, measurevar="CO2", groupvars=c("height","wd_speed"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy4_SS2_CO2, aes(x=height, y=CO2, colour=wd_speed))+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(colour = "Wind Speed")+ 
        geom_errorbar(aes(ymin=CO2-ci, ymax=CO2+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#A4EBEA","#2FA2A0"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_speed))+ geom_point()

strategy4_SS2_CH4 <- summarySE(FTIR_SW_NE_SS2, measurevar="CH4", groupvars=c("height","wd_speed"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy4_SS2_CH4, aes(x=height, y=CH4, colour=wd_speed))+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(colour = "Wind Speed")+ 
        geom_errorbar(aes(ymin=CH4-ci, ymax=CH4+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#A4EBEA","#2FA2A0"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_speed))+ geom_point()

strategy4_SS2_NH3 <- summarySE(FTIR_SW_NE_SS2, measurevar="NH3", groupvars=c("height","wd_speed"),
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ggplot(strategy4_SS2_NH3, aes(x=height, y=NH3, colour=wd_speed))+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(colour = "Wind Speed")+ 
        geom_errorbar(aes(ymin=NH3-ci, ymax=NH3+ci), width=.2) +
        geom_point(size=3)+ 
        scale_colour_manual(values=c("#A4EBEA","#2FA2A0"))+
        theme_classic(base_size = 16)+
        geom_line(aes(group=wd_speed))+ geom_point()

########### Write table (dataframe.xlsx) ##################
#write.xlsx(FTIRxwindxDWD, file="FTIR_final_data.xlsx",sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)

#FTIR_SW_NE_SS1 %>% group_by(height) %>% summarise(CH4 = mean(CH4, na.rm = TRUE))
#FTIR_SW_NE_SS2 %>% group_by(height) %>% summarise(CO2 = mean(CO2, na.rm = TRUE))
#FTIR_SW_NE_SS2 %>% group_by(height) %>% summarise(NH3 = mean(NH3, na.rm = TRUE))

########## (alternative method) gg_line ###########
#ggline(FTIRxwindxDWD, x="height", y="CO2", na.rm=TRUE, add="mean_se")+ 
        #xlab("Height (m)")+
        #ylab("CO2 (ppm)")+ 
        #theme_classic(base_size = 15)

#ggline(FTIRxwindxDWD, x="height", y="CO2", na.rm=TRUE,
       #add = c("mean_se"),
       #size = 0.5,
       #color = "Samp_loc")+
        #theme_classic(base_size = 15)+
        #stat_compare_means(aes(group = Samp_loc), hide.ns=T, label = "p.signif") 


#ggline(FTIR_SW_NE_SS1, x="height", y="CO2", na.rm=TRUE,
       #add = c("mean_se"),
       #color = "wd_cardinals",
       #palette = c("#F5AAB0","#D36069"))+
        #theme_classic(base_size = 15)



##############GRAPH 4 SS1 with ggline#############
ggline(FTIR_SW_SS1, x="height", y="CO2", na.rm=TRUE,
       add = c("mean_se"),
       size = 0.5,
       color = "wd_speed")+
        theme_classic(base_size = 15)+
        stat_compare_means(aes(group = wd_speed), hide.ns=T, label = "p.format") 

ggline(FTIR_SW_SS1, x="height", y="CH4", na.rm=TRUE,
       add = c("mean_se"),
       size = 0.5,
       color = "wd_speed")+
        theme_classic(base_size = 15)+
        stat_compare_means(aes(group = wd_speed), hide.ns=T, label = "p.format") 


ggline(FTIR_SW_SS1, x="height", y="NH3", na.rm=TRUE,
       add = c("mean_se"),
       size = 0.5,
       color = "wd_speed")+
        theme_classic(base_size = 15)+
        stat_compare_means(aes(group = wd_speed), hide.ns=T, label = "p.format") 


##############GRAPH 4 SS2 with ggline#############
ggline(FTIR_SW_SS2, x="height", y="CO2", na.rm=TRUE,
       add = c("mean_se"),
       size = 0.5,
       color = "wd_speed")+
        theme_classic(base_size = 15)+
        stat_compare_means(aes(group = wd_speed), hide.ns=T, label = "p.format") 

ggline(FTIR_SW_SS2, x="height", y="CH4", na.rm=TRUE,
       add = c("mean_se"),
       size = 0.5,
       color = "wd_speed")+
        theme_classic(base_size = 15)+
        stat_compare_means(aes(group = wd_speed), hide.ns=T, label = "p.format") 


ggline(FTIR_SW_SS2, x="height", y="NH3", na.rm=TRUE,
       add = c("mean_se"),
       size = 0.5,
       color = "wd_speed")+
        theme_classic(base_size = 15)+
        stat_compare_means(aes(group = wd_speed), hide.ns=T, label = "p.format") 

##################### pairwise.test ########################

#write.xlsx(compare_means(NH3~wd_speed, data=FTIR_SW_SS2,
                         #group.by = "height",
                         #method = "t.test",
                         #p.adjust.method = NULL), 
           #file="t.results.xlsx",sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)

