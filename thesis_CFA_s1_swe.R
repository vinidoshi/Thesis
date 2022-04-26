install.packages("tidyverse")
install.packages("psych")
install.packages("lavaan")
install.packages("lm.beta")
install.packages("semPlot")
install.packages("dplyr")
install.packages("car")

library(tidyverse)
library(psych)
library(lm.beta)
library (lavaan)
library(semPlot)
library(gridExtra)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(car)
library(dplyr)
library(lmtest)
library(lme4)
library(lmerTest) 
library(cAIC4) 	
library(r2glmm) 
library(MuMIn)
library(gsheet)
library(boot)
library(lmboot)
library(boot)

### upload dataset ###
study1_swe_final <- read_excel("C:/Users/Vini/Downloads/study1_swe_final.xlsx")
View(study1_swe_final)

cfadata <- study1_swe_final[1:512,]
View(cfadata)

### hierachical cfa ###
## define model ##
hierarchy_mod3 <- "
IPP_CD =~ IPP01 + IPP04 + IPP05 + IPP10 + IPP13 + IPP14_R + IPP18 + IPP21 + IPP23 + IPP26 + IPP30 
IPP_WS =~ IPP02 + IPP06 + IPP11_R + IPP17 + IPP24 + IPP29_R
IPP_A =~ IPP22 + IPP25 + IPP28 
IPP_OSD =~ IPP03 + IPP07 + IPP20 + IPP27 
IPP_Amb =~ IPP08 + IPP15 + IPP19
IPP_NS =~ IPP09 + IPP12 + IPP16
IPP_sof =~ IPP_CD + IPP_WS + IPP_A + IPP_OSD + IPP_Amb + IPP_NS"
## model estimation ##
result_hierarchy_mod3 <- cfa(hierarchy_mod3, data = cfadata, std.lv = TRUE)
summary(result_hierarchy_mod3, fit.measures = TRUE, standardized = TRUE)
## model visualisation ##
semPaths( result_hierarchy_mod3, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
          sizeMan = 8, sizeMan2 = 5)

### bifactor cfa ###
## define model ##
bifactor_mod4 <- "
IPP_CD =~ IPP01 + IPP04 + IPP05 + IPP10 + IPP13 + IPP14_R + IPP18 + IPP21 + IPP23 + IPP26 + IPP30 
IPP_WS =~ IPP02 + IPP06 + IPP11_R + IPP17 + IPP24 + IPP29_R
IPP_A =~ IPP22 + IPP25 + IPP28 
IPP_OSD =~ IPP03 + IPP07 + IPP20 + IPP27 
IPP_Amb =~ IPP08 + IPP15 + IPP19
IPP_NS =~ IPP09 + IPP12 + IPP16
IPP_total =~ IPP01+ IPP04 + IPP05 + IPP10 + IPP13 + IPP14_R + IPP18 + IPP21 + IPP23 + IPP26 + IPP30 + IPP02 + IPP06 + IPP11_R + IPP17 + IPP24 + IPP29_R + IPP22 + IPP25 + IPP28 + IPP03 + IPP07 + IPP20 + IPP27 + IPP08 + IPP15 + IPP19 + IPP09 + IPP12 + IPP16"
## model estimation##
result_bifactor_mod4 <- cfa(bifactor_mod4, data = cfadata, std.lv = TRUE, orthogonal = TRUE, auto.cov.lv.x = TRUE)
summary(result_bifactor_mod4, fit.measures = TRUE, standardized = TRUE)
