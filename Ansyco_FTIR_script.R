########### PACKAGES ######################
getwd()
library(tidyverse)
library(psych)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(dplyr)
library(ggpubr)
library(writexl)
library(readr)

########### FTIR DATA IMPORT ###############
FTIR_raw <- read.delim("FTIR_final_data.txt") 
FTIR_input <- FTIR_raw %>%                                              #Filtering Southwestern data
        filter((FTIR_raw$wind_direction >= 160) 
               & 
               (FTIR_raw$wind_direction <= 270))

FTIR_input$wd_speed <- ifelse(FTIR_input$wind_speed > 3, "high", "low") #Sorting wind speeds 
                                                                        #Because average wind speed was 3m/s)                                                
#strings
FTIR_input$Samp_loc <- as.factor(FTIR_input$Samp_loc)
FTIR_input$Messstelle <- as.factor(FTIR_input$Messstelle)
FTIR_input$height <- as.factor(FTIR_input$height)
FTIR_input$wd_speed <- as.factor(FTIR_input$wd_speed)
FTIR_input$wd_cardinals <- as.factor(FTIR_input$wd_cardinals)


########### DATA Visualization 1 (ggline::ggpubr) ###############
#CO2 at different heights
plot1a <- ggline(FTIR_input, x="height", y="CO2",
       add = "mean_se",
       shape = 2,
       point.size = 1.5,
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+
        theme_bw()+theme(legend.position="False",
                         axis.text = element_text(size=12),
                         axis.title = element_text(size=12))+
        xlab("Height  (meters)")+
        ylab("CO2  (ppm)")

ggsave("plot1a.pdf", width = 8, height = 4)

#CH4 at different heights
plot1b <-ggline(FTIR_input, x="height", y="CH4",
                add = "mean_se",
                shape = 7,
                point.size = 1.5,
                facet.by ="Samp_loc",
                width=0.5,
                position = position_dodge(w=0.15))+
        theme_bw()+theme(legend.position="False",
                         axis.text = element_text(size=12),
                         axis.title = element_text(size=12))+
        xlab("Height  (meters)")+
        ylab("CH4  (ppm)")

ggsave("plot1b.pdf", width = 8, height = 4)

#NH3 at different heights
plot1c <-ggline(FTIR_input, x="height", y="NH3",
                add = "mean_se",
                shape = 21,
                point.size = 1.5,
                facet.by ="Samp_loc",
                width=0.5,
                position = position_dodge(w=0.15))+ 
        theme_bw()+theme(legend.position="False",
                         axis.text = element_text(size=12),
                         axis.title = element_text(size=12))+
        xlab("Height  (meters)")+
        ylab("NH3  (ppm)")

ggsave("plot1c.pdf", width = 8, height = 4)

# Saving graphs 
# Combine the three plots horizontally
#cowplot::plot_grid(plot1a, plot1b, plot1c,  ncol = 3, align = "h", axis = "tb", rel_widths = c(1, 1, 1))

#Alternative
# combine the three plots horizontally with shared legend at top
#grid.arrange(arrangeGrob(plot1a + theme(legend.position = "none"),plot1b + theme(legend.position = "none"),nrow = 1),plot1c,nrow = 2,heights = c(3, 1))

# Save the combined plot as a PDF file
#ggsave("myplot1.pdf", width = 12, height = 3)


########### DATA Visualization 2 (ggline::ggpubr) ###############
#CO2 at different speeds
plot2a <- ggline(FTIR_input, x="height", y="CO2",
       add = "mean_se",
       shape = 2,
       point.size = 1.5,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+
        scale_color_manual(values = c("chartreuse4","deepskyblue4"))+
        theme_bw()+theme(legend.position="False",
                         axis.text = element_text(size=12),
                         axis.title = element_text(size=12))+
        xlab("Height  (meters)")+
        ylab("CO2  (ppm)")

ggsave("plot2a.pdf", width = 8, height = 4)

#CH4 at different speeds
plot2b <- ggline(FTIR_input, x="height", y="CH4",
       add = "mean_se",
       shape = 7,
       point.size = 1.5,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+
        scale_color_manual(values = c("chartreuse4","deepskyblue4"))+
        theme_bw()+ theme(legend.position="False",
                          axis.text = element_text(size=12),
                          axis.title = element_text(size=12))+
        xlab("Height  (meters)")+
        ylab("CH4  (ppm)")

ggsave("plot2b.pdf", width = 8, height = 4)

#NH3 at different speeds
plot2c <- ggline(FTIR_input, x="height", y="NH3",
       add = "mean_se",
       shape = 21,
       point.size = 1.5,
       color ="wd_speed",
       facet.by ="Samp_loc",
       width=0.5,
       position = position_dodge(w=0.15))+
        scale_color_manual(values = c("chartreuse4","deepskyblue4"))+
        theme_bw()+ theme(legend.position="False",
                          axis.text = element_text(size=12),
                          axis.title = element_text(size=12))+
        xlab("Height  (meters)")+
        ylab("NH3  (ppm)")

ggsave("plot2c.pdf", width = 8, height = 4)

#Saving graphs
# Combine the three plots horizontally
#cowplot::plot_grid(plot2a, plot2b, plot2c,  ncol = 3, align = "h", axis = "tb", rel_widths = c(1, 1, 1))

#Alternative
# combine the three plots horizontally with shared legend at top
#gridExtra::grid.arrange(arrangeGrob(plot2a, plot2b, plot2c, ncol=3, widths=c(1,1,1)))

# Save the combined plot as a PDF file
#ggsave("myplot2.pdf", width = 12, height = 3)


########### ANOVA & Kruskal Modeling ###############
###### W/O SS 
#ANOVA 
anova(aov(CO2~height*Samp_loc, data=FTIR_input))
anova(aov(CH4~height*Samp_loc, data=FTIR_input))
anova(aov(NH3~height*Samp_loc, data=FTIR_input))

#Kruskal 
kruskal.test(CO2 ~ Samp_loc, FTIR_input)
kruskal.test(CH4 ~ Samp_loc, FTIR_input)
kruskal.test(NH3 ~ Samp_loc, FTIR_input)

###### SS1 
#ANOVA SS1
FTIR_SS1 <- FTIR_input %>% filter(Samp_loc == "SS1")
anova(aov(CO2~height*wd_speed, data=FTIR_SS1))
anova(aov(CH4~height*wd_speed, data=FTIR_SS1))
anova(aov(NH3~height*wd_speed, data=FTIR_SS1))

#Kruskal SS1
kruskal.test(CO2 ~ wd_speed, FTIR_SS1)
kruskal.test(CH4 ~ wd_speed, FTIR_SS1)
kruskal.test(NH3 ~ wd_speed, FTIR_SS1)

###### SS2
#ANOVA SS2
FTIR_SS2 <- FTIR_input %>% filter(Samp_loc == "SS2")
anova(aov(CO2~height*wd_speed, data=FTIR_SS2))
anova(aov(CH4~height*wd_speed, data=FTIR_SS2))
anova(aov(NH3~height*wd_speed, data=FTIR_SS2))

#Kruskal SS2
kruskal.test(CO2 ~ wd_speed, FTIR_SS2)
kruskal.test(CH4 ~ wd_speed, FTIR_SS2)
kruskal.test(NH3 ~ wd_speed, FTIR_SS2)


########### Generalized Linear Regression Modeling ###############
#Regression  effect: height & Samp_loc 
summary(glm(CO2~height*Samp_loc, data=FTIR_input, family = Gamma(link = "log")))
summary(glm(CH4~height*Samp_loc, data=FTIR_input, family = Gamma(link = "log")))
summary(glm(NH3~height*Samp_loc, data=FTIR_input, family = Gamma(link = "log")))

#Regression  effect: height & wd_speed (SS1) 
summary(glm(CO2~height*wd_speed, data=FTIR_SS1, family = Gamma(link = "log")))
summary(glm(CH4~height*wd_speed, data=FTIR_SS1, family = Gamma(link = "log")))
summary(glm(NH3~height*wd_speed, data=FTIR_SS1, family = Gamma(link = "log")))

#Regression  effect: height & wd_speed (SS2) 
summary(glm(CO2~height*wd_speed, data=FTIR_SS2, family = Gamma(link = "log")))
summary(glm(CH4~height*wd_speed, data=FTIR_SS2, family = Gamma(link = "log")))
summary(glm(NH3~height*wd_speed, data=FTIR_SS2, family = Gamma(link = "log"))) 

########### Relative errors###############
Tib_SS1 <- FTIR_input %>% 
        filter(Samp_loc == "SS1") %>%
        group_by(height) %>%
        summarise(meanCO_2 = mean(CO2),
                  meanCH_4 = mean(CH4),
                  meanNH_3 = mean(NH3)) %>% 
        mutate(errorCO_2 = abs(meanCO_2 - mean(meanCO_2[height == 0.6])) / mean(meanCO_2[height == 0.6]) * 100 * sign(meanCO_2 - mean(meanCO_2[height == 0.6])),
               errorCH_4 = abs(meanCH_4 - mean(meanCH_4[height == 0.6])) / mean(meanCH_4[height == 0.6]) * 100 * sign(meanCH_4 - mean(meanCH_4[height == 0.6])),
               errorNH_3 = abs(meanNH_3 - mean(meanNH_3[height == 0.6])) / mean(meanNH_3[height == 0.6]) * 100 * sign(meanNH_3 - mean(meanNH_3[height == 0.6])))%>%
        mutate(across(c(meanCO_2, meanCH_4, meanNH_3, errorCO_2, errorCH_4, errorNH_3), round, 2))

readr::write_csv(Tib_SS1, "TibSS1.csv")


Tib_SS2 <- FTIR_input %>% 
        filter(Samp_loc == "SS2") %>%
        group_by(height) %>%
        summarise(meanCO_2 = mean(CO2),
                  meanCH_4 = mean(CH4),
                  meanNH_3 = mean(NH3)) %>% 
        mutate(errorCO_2 = abs(meanCO_2 - mean(meanCO_2[height == 0.6])) / mean(meanCO_2[height == 0.6]) * 100 * sign(meanCO_2 - mean(meanCO_2[height == 0.6])),
               errorCH_4 = abs(meanCH_4 - mean(meanCH_4[height == 0.6])) / mean(meanCH_4[height == 0.6]) * 100 * sign(meanCH_4 - mean(meanCH_4[height == 0.6])),
               errorNH_3 = abs(meanNH_3 - mean(meanNH_3[height == 0.6])) / mean(meanNH_3[height == 0.6]) * 100 * sign(meanNH_3 - mean(meanNH_3[height == 0.6])))%>%
        mutate(across(c(meanCO_2, meanCH_4, meanNH_3, errorCO_2, errorCH_4, errorNH_3), round, 2)) 

readr::write_csv(Tib_SS2, "TibSS2.csv")


########### Relative errors high###############
Tib_SS1_high <- FTIR_input %>% 
        filter(Samp_loc == "SS1", wd_speed == "high") %>%
        group_by(height) %>%
        summarise(meanCO_2 = mean(CO2),
                  meanCH_4 = mean(CH4),
                  meanNH_3 = mean(NH3)) %>% 
        mutate(errorCO_2 = abs(meanCO_2 - mean(meanCO_2[height == 0.6])) / mean(meanCO_2[height == 0.6]) * 100 * sign(meanCO_2 - mean(meanCO_2[height == 0.6])),
               errorCH_4 = abs(meanCH_4 - mean(meanCH_4[height == 0.6])) / mean(meanCH_4[height == 0.6]) * 100 * sign(meanCH_4 - mean(meanCH_4[height == 0.6])),
               errorNH_3 = abs(meanNH_3 - mean(meanNH_3[height == 0.6])) / mean(meanNH_3[height == 0.6]) * 100 * sign(meanNH_3 - mean(meanNH_3[height == 0.6])))%>%
        mutate(across(c(meanCO_2, meanCH_4, meanNH_3, errorCO_2, errorCH_4, errorNH_3), round, 2)) 

readr::write_csv(Tib_SS1_high, "TibSS1high.csv")

Tib_SS2_high <- FTIR_input %>% 
        filter(Samp_loc == "SS2", wd_speed == "high") %>%
        group_by(height) %>%
        summarise(meanCO_2 = mean(CO2),
                  meanCH_4 = mean(CH4),
                  meanNH_3 = mean(NH3)) %>% 
        mutate(errorCO_2 = abs(meanCO_2 - mean(meanCO_2[height == 0.6])) / mean(meanCO_2[height == 0.6]) * 100 * sign(meanCO_2 - mean(meanCO_2[height == 0.6])),
               errorCH_4 = abs(meanCH_4 - mean(meanCH_4[height == 0.6])) / mean(meanCH_4[height == 0.6]) * 100 * sign(meanCH_4 - mean(meanCH_4[height == 0.6])),
               errorNH_3 = abs(meanNH_3 - mean(meanNH_3[height == 0.6])) / mean(meanNH_3[height == 0.6]) * 100 * sign(meanNH_3 - mean(meanNH_3[height == 0.6])))%>%
        mutate(across(c(meanCO_2, meanCH_4, meanNH_3, errorCO_2, errorCH_4, errorNH_3), round, 2)) 

readr::write_csv(Tib_SS2_high, "TibSS2high.csv")

########### Relative errors low###############
Tib_SS1_low <- FTIR_input %>% 
        filter(Samp_loc == "SS1", wd_speed == "low") %>%
        group_by(height) %>%
        summarise(meanCO_2 = mean(CO2),
                  meanCH_4 = mean(CH4),
                  meanNH_3 = mean(NH3)) %>% 
        mutate(errorCO_2 = abs(meanCO_2 - mean(meanCO_2[height == 0.6])) / mean(meanCO_2[height == 0.6]) * 100 * sign(meanCO_2 - mean(meanCO_2[height == 0.6])),
               errorCH_4 = abs(meanCH_4 - mean(meanCH_4[height == 0.6])) / mean(meanCH_4[height == 0.6]) * 100 * sign(meanCH_4 - mean(meanCH_4[height == 0.6])),
               errorNH_3 = abs(meanNH_3 - mean(meanNH_3[height == 0.6])) / mean(meanNH_3[height == 0.6]) * 100 * sign(meanNH_3 - mean(meanNH_3[height == 0.6])))%>%
        mutate(across(c(meanCO_2, meanCH_4, meanNH_3, errorCO_2, errorCH_4, errorNH_3), round, 2)) 

readr::write_csv(Tib_SS1_low, "TibSS1low.csv")

Tib_SS2_low <- FTIR_input %>% 
        filter(Samp_loc == "SS2", wd_speed == "low") %>%
        group_by(height) %>%
        summarise(meanCO_2 = mean(CO2),
                  meanCH_4 = mean(CH4),
                  meanNH_3 = mean(NH3)) %>% 
        mutate(errorCO_2 = abs(meanCO_2 - mean(meanCO_2[height == 0.6])) / mean(meanCO_2[height == 0.6]) * 100 * sign(meanCO_2 - mean(meanCO_2[height == 0.6])),
               errorCH_4 = abs(meanCH_4 - mean(meanCH_4[height == 0.6])) / mean(meanCH_4[height == 0.6]) * 100 * sign(meanCH_4 - mean(meanCH_4[height == 0.6])),
               errorNH_3 = abs(meanNH_3 - mean(meanNH_3[height == 0.6])) / mean(meanNH_3[height == 0.6]) * 100 * sign(meanNH_3 - mean(meanNH_3[height == 0.6])))%>%
        mutate(across(c(meanCO_2, meanCH_4, meanNH_3, errorCO_2, errorCH_4, errorNH_3), round, 2)) 

readr::write_csv(Tib_SS2_low, "TibSS2low.csv")

########### Calculating Mixing ratios ###############
##CH4/NH3 ratio
CH_NH_data <- FTIR_input %>%
        group_by(height) %>%
        mutate(CH_NH_ratio = round(CH4/NH3, 2))


#Plotting CH4/NH3 Mixing ratios at different heights With Sampling location information 
plot3a <- ggline(CH_NH_data, x="height", y="CH_NH_ratio",
       add = "mean_se",
       shape = 11,
       point.size = 1.5,
       width=1.5,
       facet.by ="Samp_loc")+
        theme_bw() + theme(legend.position="False",
                           axis.text = element_text(size=12),
                           axis.title = element_text(size=12))+
        xlab("Height  (meters)")+
        ylab(expression(Ratio ~ (bar(CH[4]/NH[3]))))  

ggsave("plot3a.pdf", width = 8, height = 4)

#With Wind speed information 
plot3b <- ggline(CH_NH_data, x="height", y="CH_NH_ratio",
       add = "mean_se",
       shape = 11,
       point.size = 1.5,
       facet.by ="Samp_loc",
       color ="wd_speed",
       width=1.5,
       position = position_dodge(w=0.15))+
        scale_color_manual(values = c("darkorchid4","burlywood4"))+
        theme_bw() + theme(legend.position="False",
                           axis.text = element_text(size=12),
                           axis.title = element_text(size=12))+
        xlab("Height  (meters)")+
        ylab(expression(Ratio ~ (bar(CH[4]/NH[3])))) 

ggsave("plot3b.pdf", width = 8, height = 4)


#Saving graphs
# Combine the three plots horizontally
#cowplot::plot_grid(plot3a, plot3b,  ncol = 2, align = "h", axis = "tb", rel_widths = c(1, 1))
# Save the combined plot as a PDF file
#ggsave("myplot3.pdf", width = 8, height = 3)

########### Calculating Mixing ratios ###############
##CO2/CH4 ratio
CO_CH_data <- FTIR_input %>%
        group_by(height) %>%
        mutate(CO_CH_ratio = round(CO2/NH3, 2))


#Plotting CH4/NH3 Mixing ratios at different heights With Sampling location information 
plot4a <- ggline(CO_CH_data, x="height", y="CO_CH_ratio",
                 add = "mean_se",
                 shape = 11,
                 point.size = 1.5,
                 width=1.5,
                 facet.by ="Samp_loc")+
        theme_bw() + theme(legend.position="False",
                           axis.text = element_text(size=12),
                           axis.title = element_text(size=12))+
        xlab("Height  (meters)")+
        ylab(expression(Ratio ~ (bar(CO[2]/CH[4])))) 

ggsave("plot4a.pdf", width = 8, height = 4)

#With Wind speed information 
plot4b <- ggline(CO_CH_data, x="height", y="CO_CH_ratio",
                 add = "mean_se",
                 shape = 11,
                 point.size = 1.5,
                 facet.by ="Samp_loc",
                 color ="wd_speed",
                 width=1.5,
                 position = position_dodge(w=0.15))+
        scale_color_manual(values = c("darkorchid4","burlywood4"))+
        theme_bw() + theme(legend.position="False",
                           axis.text = element_text(size=12),
                           axis.title = element_text(size=12))+
        xlab("Height  (meters)")+
        ylab(expression(Ratio ~ (bar(CO[2]/CH[4])))) 

ggsave("plot4b.pdf", width = 8, height = 4)

#Saving graphs
# Combine the three plots horizontally
#cowplot::plot_grid(plot4a, plot4b,  ncol = 2, align = "h", axis = "tb", rel_widths = c(1, 1))
# Save the combined plot as a PDF file
#ggsave("myplot4.pdf", width = 8, height = 3)


######ANOVA for Mixing ratios#####
FTIR_SS1 <- CO_CH_data %>% filter(Samp_loc == "SS1")
FTIR_SS2 <- CO_CH_data %>% filter(Samp_loc == "SS2")

anova(aov(CO_CH_ratio~height, data=CO_CH_data))
anova(aov(CO_CH_ratio~height, data=FTIR_SS1))
anova(aov(CO_CH_ratio~height, data=FTIR_SS2))

kruskal.test(CO_CH_ratio~height, data=CO_CH_data)
kruskal.test(CO_CH_ratio~height, data=FTIR_SS1)
kruskal.test(CO_CH_ratio~height, data=FTIR_SS2)

######Regression  for Mixing ratios####
summary(lm(CO_CH_ratio~height*Samp_loc*wd_speed, data=CO_CH_data))
summary(lm(CO_CH_ratio~height*wd_speed, data=FTIR_SS1))
summary(lm(CO_CH_ratio~height*wd_speed, data=FTIR_SS2))

####Coef. of Var of each gas at each height####
#Group_stats <- FTIR_input %>% group_by(height) %>% 
        #summarize(CO2_mean = mean(CO2), CO2_sd = sd(CO2),
                  #CH4_mean = mean(CH4), CH4_sd = sd(CH4),
                  #NH3_mean = mean(NH3), NH3_sd = sd(NH3),
                  #GC_mean = mean(GC_ratio), GC_sd = sd(GC_ratio))

#Coeffecient of Variation of CO2, CH4, NH3 at each height
#CV <- Group_stats %>%
        #group_by(height) %>%
        #mutate(cv_CO2 = ((CO2_sd/CO2_mean)*100),
               #cv_CH4 = ((CH4_sd/CH4_mean)*100),
               #cv_NH3 = ((NH3_sd/NH3_mean)*100))
#print(CV)


######## Distribution of Data###########
par(mfrow=c(1,3)) # set plot layout to one row and three columns
qqnorm(FTIR_input$NH3, col = "blue4", main = expression(paste(NH[3])))
qqline(FTIR_input$NH3, col = "blue4")
qqnorm(FTIR_input$CH4, col = "red3", main = expression(paste(CH[4])))
qqline(FTIR_input$CH4, col = "red3")
qqnorm(FTIR_input$CO2, col = "green4", main = expression(paste(CO[2])))
qqline(FTIR_input$CO2, col = "green4")

#ggsave will not work... SO SAVE THIS GRAPH BY EXPORTING AS PDF


######EXTRAS: Create the speed column using cut()####
#speed_breaks <- c(0, 1, 2, 3, 4, 5, 6, 7, 8,9, 10,11, 12, Inf)
#speed_labels <- c(0:12)

#FTIR_input$wd_speed <- cut(FTIR_input$wind_speed, breaks = speed_breaks, labels = speed_labels)

# check number of observations for each wind speed level
#table(FTIR_input$wd_speed)




