############### PACKAGES ######################
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

############## FTIR DATA IMPORT ####################
FTIR_input <- read.table(paste0("20210902_Vertical_Pipes_Harsh_06nov.txt"), header = T, fill = TRUE) %>%
        mutate(DateTime = paste(Datum, " ", Zeit)) %>%
        relocate(DateTime)

FTIR_input$CO2 <- as.numeric(FTIR_input$CO2)
FTIR_input$NH3 <- as.numeric(FTIR_input$NH3) 
FTIR_input$CH4 <- as.numeric(FTIR_input$CH4) 
FTIR_input$Messstelle <- as.numeric(FTIR_input$Messstelle)
FTIR_input$DateTime <- ymd_hms(FTIR_input$DateTime)
FTIR_input$DateTime_FI3min = round_date(FTIR_input$DateTime, "3 minutes")
FTIR_input <- select(FTIR_input, DateTime_FI3min, Messstelle, CO2, CH4, NH3, H2O, N2O)

########### WIND & DWD DATA IMPORT #################
wind_input <- read.table("D:/HARSHY DATA 2020/Master Thesis/USA Windmast data/wind_WD_WS_data.txt")
wind_input$DateTime_WI3min <- ymd_hms(wind_input$DateTime_WI3min)

DWD_input <- read.table("D:/HARSHY DATA 2020/Master Thesis/USA Windmast data/DWD_interpolated.txt")
DWD_input$MESS_DATUM <- ymd_hms(DWD_input$MESS_DATUM)


############## FTIR+WIND ######################
FTIR_02SEP_06OCT <- select(FTIR_input, DateTime_FI3min, Messstelle, CO2, CH4, NH3, H2O, N2O) %>% 
        filter(DateTime_FI3min >= ymd_hms("2021-09-02 11:42:00"),
               DateTime_FI3min <= ymd_hms("2021-10-06 11:21:00"))

FTIRxwind <- left_join(FTIR_02SEP_06OCT, wind_input, 
                       by = c("DateTime_FI3min" = "DateTime_WI3min"))

############## FTIR+DWD ######################
FTIR_06OCT_06NOV <- select(FTIR_input, DateTime_FI3min, Messstelle, CO2, CH4, NH3, H2O, N2O) %>% 
        filter(DateTime_FI3min >= ymd_hms("2021-10-06 11:24:00"),
               DateTime_FI3min <= ymd_hms("2021-11-06 11:21:00"))
FTIRxDWD <- left_join(FTIR_06OCT_06NOV,DWD_input, 
                      by = c("DateTime_FI3min" = "MESS_DATUM"))


############## FTIR+WIND+DWD ###################
FTIRxwindxDWD <- rbind(FTIRxwind,FTIRxDWD)


############# FTIR SOUTH ONLY ##################
FTIR_south  <- FTIRxwindxDWD  %>% filter(wind_direction >= 150, wind_direction <= 230)


############## WIND_GRAPH ######################
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
         key.header = "02.09.2021 - 06.10.2021",
         key.footer = "Wind_speed (m/s)",
         key.position = "bottom",
         par.settings=list(axis.line=list(col="lightgray")),
         col = c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f", "#d7153a"))


########## GAS_CONCENTRATION_AT_DIFF_HEIGHT ##############

#1 MessstellexCO2 South_East (Before  milking parlor)
MessstellexCO2i <-  FTIR_south %>% select(Messstelle, CO2)%>%
        filter(Messstelle <=6, Messstelle >=1) %>% convert(fct(Messstelle))
CO2linmodi <- lm(CO2~Messstelle, data=MessstellexCO2i)
summary(CO2linmodi)    
anova(CO2linmodi)

plot(CO2~Messstelle, data=MessstellexCO2i, main = "CO2_South_East")
abline(reg=CO2linmodi, col="red")



#1 MessstellexCO2 South_West (After  milking parlor)
MessstellexCO2ii <-  FTIR_south %>% select(Messstelle, CO2) %>%
        filter(Messstelle <=12, Messstelle >=7) %>% convert(fct(Messstelle)) 
CO2linmodii <- lm(CO2~Messstelle, data=MessstellexCO2ii)
summary(CO2linmodii)    
boxplot(CO2~Messstelle, data=MessstellexCO2ii, main = "CO2_South_West")
abline(reg=CO2linmodii, col="red")


#2 MessstellexNH3 South_East (Before  milking parlor)
MessstellexNH3i <- FTIR_south %>% select(Messstelle, NH3) %>%
        filter(Messstelle <=6, Messstelle >=1) %>% convert(fct(Messstelle))
NH3linmodi <- lm(NH3~Messstelle, data=MessstellexNH3i)
summary(NH3linmodi)    
boxplot(NH3~Messstelle, data=MessstellexNH3i, main = "NH3_South_East")
abline(reg=NH3linmodi, col="red")

#2 MessstellexNH3 South_West (After  milking parlor)
MessstellexNH3ii <- FTIR_south %>% select(Messstelle, NH3) %>%
        filter(Messstelle <=12, Messstelle >=7) %>% convert(fct(Messstelle))
NH3linmodii <- lm(NH3~Messstelle, data=MessstellexNH3ii)
summary(NH3linmodii)    
boxplot(NH3~Messstelle, data=MessstellexNH3ii, main = "NH3_South_West")
abline(reg=NH3linmodii, col="red")


#3 MessstellexCH4 South_East (Before  milking parlor)
MessstellexCH4i <- FTIR_south %>% select(Messstelle, CH4) %>%
        filter(Messstelle <=6, Messstelle >=1) %>% convert(fct(Messstelle))
CH4linmodi <- lm(CH4~Messstelle, data=MessstellexCH4i)
summary(CH4linmodi)    
boxplot(CH4~Messstelle, data=MessstellexCH4i, main = "CH4_South_East")
abline(reg=CH4linmodi, col="red")

#3 MessstellexCH4 South_West (After  milking parlor)
MessstellexCH4ii <- FTIR_south %>% select(Messstelle, CH4) %>%
        filter(Messstelle <=12, Messstelle >=7) %>% convert(fct(Messstelle))
CH4linmodii <- lm(CH4~Messstelle, data=MessstellexCH4ii)
anova(CH4linmodii)
summary(CH4linmodii)    
boxplot(CH4~Messstelle, data=MessstellexCH4ii, main = "CH4_South_West")
abline(reg=CH4linmodii, col="red")

anova(CH4linmodii)

############### RESIDUAL_ANALYSIS ########################
expected <- fitted(CH4linmodii)
residuals <- resid(CH4linmodii)

boxplot(residuals, main="residual boxplot")
plot(x=expected, y=residuals, xlab="expected values", ylab="residuals", main="residual plot")
abline(h=0)
plot(CH4linmodii, which=1:2)

# T-test
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




################ non-linear_modelling ##############
#CO2quegri <- lm(CO2~Messstelle + I(Messstelle^2), data=MessstellexCO2i)
