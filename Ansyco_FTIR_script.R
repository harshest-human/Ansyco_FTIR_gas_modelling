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

 #Categorize Messstelle into Samp_loc_location
FTIR_input$Samp_loc[as.numeric(FTIR_input$Messstelle)>=7]  = "Set-up 1"
FTIR_input$Samp_loc[as.numeric(FTIR_input$Messstelle)<=6]  = "Set-up 2" 

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
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "N"]  = "North"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NE"]  = "Northeast"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NNE"]  = "Northeast"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NW"]  = "Northwest"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NNW"]  = "Northwest"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "E"]  = "East"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "ENE"]  = "Northeast"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "ESE"]  = "Southeast"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "S"]  = "South"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SE"]  = "Southeast"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SSE"]  = "Southeast"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SW"]  = "Southwest"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SSW"]  = "Southwest"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "W"]  = "West"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "WNW"]  = "Northwest"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "WSW"]  = "Northwest"

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


########### filter wind direction S-SW & N-NE | SS1_NW & SS2_NE ############
FTIR_north_east <- FTIRxwindxDWD %>% filter(wd_cardinals== c("North","Northeast"))
FTIR_south_west <- FTIRxwindxDWD %>% filter(wd_cardinals == c("South","Southwest"))
FTIR_south_west_SS1 <- FTIR_south_west %>% filter(Samp_loc == "Set-up 1")
FTIR_south_west_SS2 <- FTIR_south_west %>% filter(Samp_loc == "Set-up 2")


########### GAS_CONCENTRATIONS_VS_HEIGHTS_SW ##############
CO2xheightxSSW <- ggplot(FTIR_south_west, aes(x=as.factor(height),y=CO2,fill=(as.factor(Samp_loc))))+ 
        ggtitle("CO2 at varying heights (South-Southwest)")+ 
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(fill = "Sampling_Location")+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))
        
CH4xheightxSSW <- ggplot(FTIR_south_west, aes(x=as.factor(height), y=CH4, fill=(as.factor(Samp_loc))))+ 
        ggtitle("CH4 at varying heights (South-Southwest)")+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(fill = "Sampling_Location")+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))

NH3xheightxSSW <- ggplot(FTIR_south_west, aes(x=as.factor(height), y=NH3, fill=(as.factor(Samp_loc))))+ 
        ggtitle("NH3 at varying heights (South-Southwest)")+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(fill= "Sampling_Location")+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))


CO2xheightxSSW
CH4xheightxSSW
NH3xheightxSSW


########### GAS_CONCENTRATIONS_VS_HEIGHTS_NE ##############
CO2xheightxNNE <- ggplot(FTIR_north_east, aes(x=as.factor(height), y=CO2, fill=(as.factor(Samp_loc))))+ 
        ggtitle("CO2 at varying heights (North-Northeast)")+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(fill = "Sampling_Location")+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))

CH4xheightxNNE <- ggplot(FTIR_north_east, aes(x=as.factor(height), y=CH4, fill=(as.factor(Samp_loc))))+ 
        ggtitle("CH4 at varying heights (North-Northeast)")+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(fill = "Sampling_Location")+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))

NH3xheightxNNE <- ggplot(FTIR_north_east, aes(x=as.factor(height), y=NH3, fill=(as.factor(Samp_loc))))+ 
        ggtitle("NH3 at varying heights (North-Northeast)")+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(fill= "Sampling_Location")+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))


CO2xheightxNNE
CH4xheightxNNE
NH3xheightxNNE


########### GAS_CONCENTRATIONS_VS_WIND_SPEED_SW_SS1 ############
CO2xwind_SS1 <- ggplot(FTIR_south_west,aes(x=as.factor(height),y=CO2,fill=as.factor(wd_speed)))+ 
        geom_boxplot(outlier.shape = NA) + 
        ggtitle("CO2 at varying heights and wind speeds (South-Southwest) SS1")+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(fill = "wind speed")+ 
        scale_fill_manual(values=c("#D36069","#F5AAB0"))+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))
        
CH4xwind_SS1 <- ggplot(FTIR_south_west_SS1,aes(x=as.factor(height),y=CH4,fill= as.factor(wd_speed)))+ 
        geom_boxplot(outlier.shape = NA) + 
        ggtitle("CH4 at varying heights and wind speeds (South-Southwest) SS1")+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(fill = "wind speed")+ 
        scale_fill_manual(values=c("#D36069","#F5AAB0"))+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))

NH3xwind_SS1 <- ggplot(FTIR_south_west_SS1,aes(x=as.factor(height),y=NH3,fill= as.factor(wd_speed)))+ 
        geom_boxplot(outlier.shape = NA) + 
        ggtitle("NH3 at varying heights and wind speeds (South-Southwest) SS1")+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(fill = "wind speed")+ 
        scale_fill_manual(values=c("#D36069","#F5AAB0"))+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))

CO2xwind_SS1
CH4xwind_SS1
NH3xwind_SS1


########### GAS_CONCENTRATIONS_VS_WIND_SPEED_SW_SS2 ############
CO2xwind_SS2 <- ggplot(FTIR_south_west_SS2,aes(x=as.factor(height),y=CO2,fill= as.factor(wd_speed)))+ 
        geom_boxplot(outlier.shape = NA) + 
        ggtitle("CO2 at varying heights and wind speeds (South-Southwest) SS2")+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(fill = "wind speed")+ 
        scale_fill_manual(values=c("#2FA2A0","#A4EBEA"))+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))

CH4xwind_SS2 <- ggplot(FTIR_south_west_SS2,aes(x=as.factor(height),y=CH4,fill= as.factor(wd_speed)))+ 
        geom_boxplot(outlier.shape = NA) + 
        ggtitle("CH4 at varying heights and wind speeds (South-Southwest) SS2")+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(fill = "wind speed")+ 
        scale_fill_manual(values=c("#2FA2A0","#A4EBEA"))+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))

NH3xwind_SS2 <- ggplot(FTIR_south_west_SS2,aes(x=as.factor(height),y=NH3,fill= as.factor(wd_speed)))+ 
        geom_boxplot(outlier.shape = NA) + 
        ggtitle("NH3 at varying heights and wind speeds (South-Southwest) SS2")+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(fill = "wind speed")+ 
        scale_fill_manual(values=c("#2FA2A0","#A4EBEA"))+
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.shape = NA)+
        stat_compare_means(method = "anova",size = 5, color = "darkred",label="p.format")+
        theme(text = element_text(size=16))

CO2xwind_SS2
CH4xwind_SS2
NH3xwind_SS2


########### Linear_Modelling_SS1########################
summary(lm(CO2~height, data=FTIR_south_west_SS1))
ggplot(FTIR_south_west_SS1, aes(x=as.numeric(height), y=CO2))+ 
        ggtitle("CO2 at varying heights")+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(fill = "Sampling_Line")+
        geom_point()+ geom_smooth(method = "lm",color="red",fill="grey",se=TRUE)

summary(lm(CH4~height, data=FTIR_south_west_SS1))
ggplot(FTIR_south_west_SS1, aes(x=as.numeric(height), y=CH4))+ 
        ggtitle("CH4 at varying heights")+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(fill = "Sampling_Line")+
        geom_point()+ geom_smooth(method = "lm",color="red",fill="grey",se=TRUE)

summary(lm(NH3~height, data=FTIR_south_west_SS1))
ggplot(FTIR_south_west_SS1, aes(x=as.numeric(height), y=NH3))+ 
        ggtitle("NH3 at varying heights")+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(fill = "Sampling_Line")+
        geom_point()+ geom_smooth(method = "lm",color="red",fill="grey",se=TRUE)


########### Linear_Modelling_SS2########################
summary(lm(CO2~height, data=FTIR_south_west_SS2))
ggplot(FTIR_south_west_SS2, aes(x=as.numeric(height), y=CO2))+ 
        ggtitle("CO2 at varying heights")+
        xlab("Height (m)") + ylab("CO2 (ppm)")+ labs(fill = "Sampling_Line")+
        geom_point()+ geom_smooth(method = "lm",color="blue",fill="grey",se=TRUE)

summary(lm(CH4~height, data=FTIR_south_west_SS2))
ggplot(FTIR_south_west_SS2, aes(x=as.numeric(height), y=CH4))+ 
        ggtitle("CH4 at varying heights")+
        xlab("Height (m)") + ylab("CH4 (ppm)")+ labs(fill = "Sampling_Line")+
        geom_point()+ geom_smooth(method = "lm",color="blue",fill="grey",se=TRUE)

summary(lm(NH3~height, data=FTIR_south_west_SS2))
ggplot(FTIR_south_west_SS2, aes(x=as.numeric(height), y=NH3))+ 
        ggtitle("NH3 at varying heights")+
        xlab("Height (m)") + ylab("NH3 (ppm)")+ labs(fill = "Sampling_Line")+
        geom_point()+ geom_smooth(method = "lm",color="blue",fill="grey",se=TRUE)


############## ANOVA  ###############################
FTIR_south_west_SS1$height <-as.factor(FTIR_south_west_SS1$height)
FTIR_south_west_SS2$height <-as.factor(FTIR_south_west_SS2$height)


CO2_aov <- aov(CO2~height, data=FTIR_south_west_SS1)
CH4_aov <- aov(CH4~height, data=FTIR_south_west_SS1)
NH3_aov <- aov(NH3~height, data=FTIR_south_west_SS1)

anova(CO2_aov)
anova(CH4_aov)
anova(NH3_aov)

CO2_aov_2 <- aov(CO2~height, data=FTIR_south_west_SS2)
CH4_aov_2 <- aov(CH4~height, data=FTIR_south_west_SS2)
NH3_aov_2 <- aov(NH3~height, data=FTIR_south_west_SS2)

anova(CO2_aov_2)
anova(CH4_aov_2)
anova(NH3_aov_2)
########### Multiple_comparison_test_SS1 ################
CO2_MCT_SS1 <- TukeyHSD(CO2_aov)
CH4_MCT_SS1 <- TukeyHSD(CH4_aov)
NH3_MCT_SS1 <- TukeyHSD(NH3_aov)

CO2xheight_MCT_SS1 <- as.data.frame(CO2_MCT_SS1["height"])
CH4xheight_MCT_SS1 <- as.data.frame(CH4_MCT_SS1["height"])
NH3xheight_MCT_SS1 <- as.data.frame(NH3_MCT_SS1["height"])
write.xlsx(CO2_MCT_SS1["height"], 'CO2xheight_MCT_SS1.xlsx')
write.xlsx(CH4_MCT_SS1["height"], 'CH4xheight_MCT_SS1.xlsx')
write.xlsx(NH3_MCT_SS1["height"], 'NH3xheight_MCT_SS1.xlsx')


CO2xheight_MCT_SS1
CH4xheight_MCT_SS1
NH3xheight_MCT_SS1


########### Multiple_comparison_test_SS2 ################
CO2_MCT_SS2 <- TukeyHSD(CO2_aov_2)
CH4_MCT_SS2 <- TukeyHSD(CH4_aov_2)
NH3_MCT_SS2 <- TukeyHSD(NH3_aov_2)

CO2xheight_MCT_SS2 <- as.data.frame(CO2_MCT_SS2["height"])
CH4xheight_MCT_SS2 <- as.data.frame(CH4_MCT_SS2["height"])
NH3xheight_MCT_SS2 <- as.data.frame(NH3_MCT_SS2["height"])
write.xlsx(CO2_MCT_SS2["height"], 'CO2xheight_MCT_SS2.xlsx')
write.xlsx(CH4_MCT_SS2["height"], 'CH4xheight_MCT_SS2.xlsx')
write.xlsx(NH3_MCT_SS2["height"], 'NH3xheight_MCT_SS2.xlsx')


CO2xheight_MCT_SS2
CH4xheight_MCT_SS2
NH3xheight_MCT_SS2

########### Write table (dataframe.xlsx) ##################
#write.xlsx(FTIRxwindxDWD, file="FTIR_final_data.xlsx",sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)
#Final_summary <- select(FTIRxwindxDWD,-Messstelle,-Samp_loc,-DateTime_FI3min,-wind_direction,-wind_speed) 
#Final_summary %>% tbl_summary(by = wd_cardinals)


