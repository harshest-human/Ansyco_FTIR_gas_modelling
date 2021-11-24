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

 #Categorize Messstelle into Pipe_location
FTIR_input$Pipe[as.numeric(FTIR_input$Messstelle)>=7]  = "SW Pipe"
FTIR_input$Pipe[as.numeric(FTIR_input$Messstelle)<=6]  = "SE Pipe" 

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
                     DateTime_FI3min,Pipe,Messstelle,height,CO2,CH4,NH3) %>% 
        convert(fct(Pipe)) %>% na.omit()



########### WIND & DWD DATA IMPORT ########
wind_input <- read.table("D:/HARSHY DATA 2020/Master Thesis/USA Windmast data/USA_Anemometer_wind_modelling/wind_WD_WS_data.txt")
wind_input$DateTime_WI3min <- ymd_hms(wind_input$DateTime_WI3min)
DWD_input <- read.table("D:/HARSHY DATA 2020/Master Thesis/USA Windmast data/USA_Anemometer_wind_modelling/DWD_interpolated.txt")
DWD_input$MESS_DATUM <- ymd_hms(DWD_input$MESS_DATUM)
 
 #FTIR+WIND 
FTIR_02SEP_06OCT <- select(FTIR_input, 
                           DateTime_FI3min,Pipe,Messstelle,height,CO2,CH4,NH3)%>% 
        filter(DateTime_FI3min >= ymd_hms("2021-09-02 11:42:00"),
               DateTime_FI3min <= ymd_hms("2021-10-06 11:21:00"))
FTIRxwind <- left_join(FTIR_02SEP_06OCT, wind_input, 
                       by = c("DateTime_FI3min" = "DateTime_WI3min"))

 #FTIR+DWD
FTIR_06OCT_06NOV <- select(FTIR_input, 
                           DateTime_FI3min,Pipe,Messstelle,height,CO2,CH4,NH3)%>% 
        filter(DateTime_FI3min >= ymd_hms("2021-10-06 11:24:00"),
               DateTime_FI3min <= ymd_hms("2021-11-06 11:21:00"))
FTIRxDWD <- left_join(FTIR_06OCT_06NOV,DWD_input, 
                      by = c("DateTime_FI3min" = "MESS_DATUM"))


########### FINAL DATA FRAME ###################
FTIRxwindxDWD <- rbind(FTIRxwind,FTIRxDWD)

 #Integration of 16 wind_cardinals
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "N"]  = "North"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NE"]  = "North"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NNE"]  = "North"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NW"]  = "North"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NNW"]  = "North"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "E"]  = "East"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "ENE"]  = "East"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "ESE"]  = "East"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "S"]  = "South"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SE"]  = "South"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SSE"]  = "South"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SW"]  = "South"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SSW"]  = "South"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "W"]  = "West"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "WNW"]  = "West"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "WSW"]  = "West"

 #Final Dataframe
FTIRxwindxDWD <- select(FTIRxwindxDWD,
                        DateTime_FI3min,Pipe,Messstelle,
                        height,CO2,CH4,NH3,
                        wind_direction,wind_speed,
                        wd_cardinals,-wd_cardinal) %>% 
        filter(DateTime_FI3min >= ymd_hms("2021-09-02 11:57:00"),
               DateTime_FI3min <= ymd_hms("2021-11-06 11:18:00")) %>% na.omit()

Final_summary <- select(FTIRxwindxDWD,-Messstelle,-Pipe,-DateTime_FI3min,-wind_direction,-wind_speed) 
Final_summary %>% tbl_summary(by = wd_cardinals)


 #Remove Outliers
Remove_outliers_function <- source("remove_outliers_function.R")
FTIRxwindxDWD$CO2 <- remove_outliers(FTIRxwindxDWD$CO2)
FTIRxwindxDWD$CH4 <- remove_outliers(FTIRxwindxDWD$CH4)
FTIRxwindxDWD$NH3 <- remove_outliers(FTIRxwindxDWD$NH3)

########### WIND_GRAPH ######################
windRose(FTIRxwindxDWD  , ws = "wind_speed", wd = "wind_direction",
         breaks = c(0,1,2,4,6,12),
         auto.text = FALSE,
         paddle = FALSE,
         grid.line = 5,
         key = list(lables = c(">0 - 1",
                               ">1 - 2",
                               ">2 - 4",
                               ">4 - 6",
                               ">6 - 12")),
         key.header = "02.09.2021 - 06.11.2021",
         key.footer = "Wind_speed (m/s)",
         key.position = "bottom",
         par.settings=list(axis.line=list(col="lightgray")),
         col = c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f", "#d7153a"))


########### GAS_CONCENTRATIONS_VS_HEIGHTS ##############
CO2xheight <- ggplot(FTIRxwindxDWD, aes(x=as.numeric(height), y=CO2))+ 
        ggtitle("CO2 at varying heights")+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(fill = "Sampling_Line")+
        geom_point()+ geom_smooth(method = "lm",color="red",fill="grey",se=TRUE)

CH4xheight <- ggplot(FTIRxwindxDWD, aes(x=as.numeric(height), y=CH4))+ 
        ggtitle("CH4 at varying heights")+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(fill = "Sampling_Line")+
        geom_point()+ geom_smooth(method = "lm",color="red",fill="grey",se=TRUE)

NH3xheight <- ggplot(FTIRxwindxDWD, aes(x=as.numeric(height), y=NH3))+ 
        ggtitle("NH3 at varying heights")+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(fill= "Sampling_Line")+
        geom_point()+ geom_smooth(method = "lm",color="red",fill="grey",se=TRUE)

CO2xheight
CH4xheight
NH3xheight


CO2xheight <- ggplot(FTIRxwindxDWD, aes(x=as.factor(height), y=CO2, fill=(as.factor(Pipe))))+ 
        ggtitle("CO2 at varying heights")+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(fill = "Sampling_Line")+
        geom_boxplot()+ stat_compare_means(method = "anova")

CH4xheight <- ggplot(FTIRxwindxDWD, aes(x=as.factor(height), y=CH4, fill=(as.factor(Pipe))))+ 
        ggtitle("CH4 at varying heights")+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(fill = "Sampling_Line")+
        geom_boxplot()+ stat_compare_means(method = "anova")

NH3xheight <- ggplot(FTIRxwindxDWD, aes(x=as.factor(height), y=NH3, fill=(as.factor(Pipe))))+ 
        ggtitle("NH3 at varying heights")+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(fill= "Sampling_Line")+
        geom_boxplot()+ stat_compare_means(method = "anova")


CO2xheight
CH4xheight
NH3xheight


########### GAS_CONCENTRATIONS_VS_WIND ############
CO2xwind <- ggplot(FTIRxwindxDWD,aes(x=as.factor(height),y=CO2,col=wd_cardinals))+ 
        geom_boxplot() + 
        ggtitle("CO2 at varying heights and wind directions")+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(colour = "Wind_cardinal")+
        scale_y_continuous(breaks = seq(0,1400, by = 100))

CH4xwind <- ggplot(FTIRxwindxDWD,aes(x=as.factor(height),y= CH4,col=wd_cardinals))+ 
        geom_boxplot() + 
        ggtitle("CH4 at varying heights and wind directions")+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(colour = "Wind_cardinal")+
        scale_y_continuous(breaks = seq(10,100, by = 10))


NH3xwind <- ggplot(FTIRxwindxDWD,aes(x=as.factor(height),y= NH3,col= wd_cardinals))+
        geom_boxplot()+
        ggtitle("NH3 at varying heights and wind directions")+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(colour = "Wind_cardinal")+
        scale_y_continuous(breaks = seq(0, 10, by = 0.5))

CO2xwind
CH4xwind
NH3xwind


########### Linear Modelling ########################
CO2_lm <- lm(CO2~Messstelle*wd_cardinals, data=FTIRxwindxDWD)
CH4_lm <- lm(CH4~Messstelle*wd_cardinals, data=FTIRxwindxDWD)
NH3_lm <- lm(NH3~Messstelle*wd_cardinals, data=FTIRxwindxDWD)

summary(CO2_lm)
summary(CH4_lm)
summary(NH3_lm)

############## ANOVA  ###############################
FTIRxwindxDWD$Messstelle <-as.factor(FTIRxwindxDWD$Messstelle)
FTIRxwindxDWD$height <-as.factor(FTIRxwindxDWD$height)
CO2_aov <- aov(CO2~height*wd_cardinals, data=FTIRxwindxDWD)
CH4_aov <- aov(CH4~height*wd_cardinals, data=FTIRxwindxDWD)
NH3_aov <- aov(NH3~height*wd_cardinals, data=FTIRxwindxDWD)

anova(CO2_aov)
anova(CH4_aov)
anova(NH3_aov)

########### Multiple_comparison_test ################
CO2_MCT <- TukeyHSD(CO2_aov)
CH4_MCT <- TukeyHSD(CH4_aov)
NH3_MCT <- TukeyHSD(NH3_aov)

 # height wise
CO2xheight_MCT <- as.data.frame(CO2_MCT["height"])
CH4xheight_MCT <- as.data.frame(CH4_MCT["height"])
NH3xheight_MCT <- as.data.frame(NH3_MCT["height"])
write.xlsx(CO2_MCT["height"], 'CO2xheight_MCT.xlsx')
write.xlsx(CH4_MCT["height"], 'CH4xheight_MCT.xlsx')
write.xlsx(NH3_MCT["height"], 'NH34xheight_MCT.xlsx')

# wd_cardinal wise
CO2xwind_MCT <- as.data.frame(CO2_MCT["wd_cardinals"])
CH4xwind_MCT <- as.data.frame(CH4_MCT["wd_cardinals"])
NH3xwind_MCT <- as.data.frame(NH3_MCT["wd_cardinals"])
write.xlsx(CO2_MCT["wd_cardinals"], 'CO2xwd_cardinals_MCT.xlsx')
write.xlsx(CH4_MCT["wd_cardinals"], 'CH4xwd_cardinals_MCT.xlsx')
write.xlsx(NH3_MCT["wd_cardinals"], 'NH3xwd_cardinals_MCT.xlsx')

CO2xheight_MCT
CH4xheight_MCT
NH3xheight_MCT
CO2xwind_MCT
CH4xwind_MCT
NH3xwind_MCT
########### Write table (dataframe.xlsx) ##################
#write.xlsx(FTIRxwindxDWD, file="FTIR_final_data.xlsx",sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)

