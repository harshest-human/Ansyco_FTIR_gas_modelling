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
FTIR_input$Pipe[as.numeric(FTIR_input$Messstelle)>=7]  = "SW"
FTIR_input$Pipe[as.numeric(FTIR_input$Messstelle)<=6]  = "SE" 

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
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NE"]  = "Northeast"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NNE"]  = "Northeast"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NW"]  = "Northwest"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "NNW"]  = "Northwest"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "E"]  = "East"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "ENE"]  = "East"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "ESE"]  = "East"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "S"]  = "South"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SE"]  = "Southeast"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SSE"]  = "Southeast"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SW"]  = "Southwest"
FTIRxwindxDWD$wd_cardinals[as.character(FTIRxwindxDWD$wd_cardinal)== "SSW"]  = "Southwest"
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
               DateTime_FI3min <= ymd_hms("2021-11-06 11:18:00"))


########### GAS_CONCENTRATIONS_VS_HEIGHTS ##############

CO2xheight <- ggplot(FTIRxwindxDWD, aes(x=as.factor(height), y=CO2, fill=(as.factor(Pipe))))+ 
        ggtitle("CO2 at varying heights ")+
        xlab("Height (m)") + ylab("CO2 (ppm)")+
        geom_boxplot()

CH4xheight <- ggplot(FTIRxwindxDWD, aes(x=as.factor(height), y=CH4, fill=(as.factor(Pipe))))+ 
        ggtitle("CH4 at varying heights ")+
        xlab("Height (m)") + ylab("CH4 (ppm)")+
        geom_boxplot()

NH3xheight <- ggplot(FTIRxwindxDWD, aes(x=as.factor(height), y=NH3, fill=(as.factor(Pipe))))+ 
        ggtitle("NH3 at varying heights ")+
        xlab("Height (m)") + ylab("NH3 (ppm)")+
        geom_boxplot()

CO2xheight
CH4xheight
NH3xheight


########### WIND_GRAPH ######################
Wind_roses <- windRose(FTIRxwindxDWD  , ws = "wind_speed", wd = "wind_direction",
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

Wind_roses


########### GAS_AT_DIFF_WIND ################
heightxCO2xwind <-  select(FTIRxwindxDWD, Messstelle,wd_cardinals, height, CO2)%>% 
        filter(Messstelle <=6, Messstelle >=1) %>% na.omit(FTIRxwindxDWD)
ggplot(heightxCO2xwind, aes(x=height, y=CO2, col=wd_cardinals))+ geom_point() + 
        ggtitle("CO2 at varying heights and wind directions") +
        xlab("Height (m)") + ylab("CO2 (ppm)") + 
        scale_y_continuous(breaks = seq(0, 1400, by = 100)) +
        scale_x_continuous(breaks = c(0.6, 0.9, 1.5, 1.8, 2.4, 2.7)) +
        geom_smooth(method = "lm")

CO2linmod <- lm(CO2~height, data=heightxCO2xwind)
anova(CO2linmod)
summary(CO2linmod)


heightxCH4xwind <-  select(FTIRxwindxDWD, height, wd_cardinals, CH4)%>% na.omit(FTIRxwindxDWD)
qplot(data=heightxCH4xwind ,x= height, y= CH4,col=wd_cardinals, geom= "point") + 
        ggtitle("CH4 at varying heights and wind directions") +
        xlab("Height (m)") + ylab("CH4 (ppm)") +
        scale_y_continuous(breaks = seq(10, 100, by = 10))+
        scale_x_continuous(breaks = c(0.6, 0.9, 1.5, 1.8, 2.4, 2.7)) +
        geom_smooth(method = "lm")


heightxNH3xwind <-  select(FTIRxwindxDWD, height, wd_cardinals, NH3)%>% na.omit(FTIRxwindxDWD)
qplot(data=heightxNH3xwind, x= height, col= wd_cardinals, y= NH3, geom= "point") + 
        ggtitle("NH3 at varying heights and wind directions")+
        scale_y_continuous(breaks = seq(0, 10, by = 0.5))+
        scale_x_continuous(breaks = c(0.6, 0.9, 1.5, 1.8, 2.4, 2.7)) +
        geom_smooth(method = "lm")


########### RESIDUAL_ANALYSIS #################
expected <- fitted(CH4linmodii)
residuals <- resid(CH4linmodii)

boxplot(residuals, main="residual boxplot")
plot(x=expected, y=residuals, xlab="expected values", ylab="residuals", main="residual plot")
abline(h=0)
plot(CH4linmodii, which=1:2)

############### T-test ########################
MessstellexCO2i %>% group_by(Messstelle) %>% summarise(check = mean(CO2)) #mean tibble
t.test(MessstellexCO2i[MessstellexCO2i$Messstelle==1, 2], MessstellexCO2i[MessstellexCO2i$Messstelle==2,2 ])

MessstellexCO2i %>% group_by(Messstelle) %>% summarise(check = mean(CO2)) #mean tibble
t.test(MessstellexCO2i[MessstellexCO2i$Messstelle==1, 2], MessstellexCO2i[MessstellexCO2i$Messstelle==3,2 ])

MessstellexCO2i %>% group_by(Messstelle) %>% summarise(check = mean(CO2)) #mean tibble
t.test(MessstellexCO2i[MessstellexCO2i$Messstelle==1, 2], MessstellexCO2i[MessstellexCO2i$Messstelle==4,2 ])

MessstellexCO2i %>% group_by(Messstelle) %>% summarise(check = mean(CO2)) #mean tibble
t.test(MessstellexCO2i[MessstellexCO2i$Messstelle==1, 2], MessstellexCO2i[MessstellexCO2i$Messstelle==5,2 ])

MessstellexCO2i %>% group_by(Messstelle) %>% summarise(check = mean(CO2)) #mean tibble
t.test(MessstellexCO2i[MessstellexCO2i$Messstelle==1, 2], MessstellexCO2i[MessstellexCO2i$Messstelle==6,2 ])

p_table <- select(FTIRxwindxDWD,CO2,CH4,NH3,Messstelle)
tbl_summary(p_table, by = Messstelle, missing = "no") %>%
        add_p()


#+ stat_compare_means(method = "t.test")







