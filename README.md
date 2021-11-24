---
title: "Ansyco FTIR Gas Modelling"
author: "Harsh Sahu"
date: "16/11/2021"
output: html_document
---

## Introduction

Statistical analysis and modelling of the influence of the positioning height of the sensors and the influence of wind on gas concentration measurements by the device **FTIR**.

This project is an integral part of a master thesis _"Investigation of the spatial distribution of ammonia, methane, and carbon dioxide in a naturally ventilated dairy barn"_.

***

> Main tasks

* Cleaning the FTIR data, Wind data and DWD data
* Working with Time-series; Rounding date & time
* Manipulating the strings
* Combining the FTIR, Wind and DWD dataframes
* Visualization of the data-
  + Wind direction and speed
  + Gas concentrations
* Statistical analysis & Modelling-
  + Variance analysis of sampling heights
  + Regression analysis of gases vs. heights
  + Residual analysis of models



```{install packages}
library(tidyverse, reshape2, hablar, lubridate, psych, rmarkdown, ggplot2, readxl, dplyr, openair, car)
```


