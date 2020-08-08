# The script build a SEM with mixed effects and varying in a 7 days time window
library(lavaan)
library(ggplot2)
library(semPlot)
library(lavaanPlot)
library(dplyr)
library(psych)
library(wesanderson)
library(piecewiseSEM)
library(nlme)
library(lme4)
library(mgcv)
library(semTools)
library(performance)

# Read DATA
Agg_Trips_1 <- read.csv('D:/COVID-19/PNAS_SECOND/All_XY_Features_To_R_County_Level_0731_toR.csv')
Agg_Trips_1 <- Agg_Trips_1[with(Agg_Trips_1, order(CTFIPS, Date)),]
Agg_Trips_1$Is_ReopenState <- TRUE
Agg_Trips_1$Date <- as.Date(Agg_Trips_1$Date)
Start_date <- as.Date('2020-03-10')
Max_Day <- as.numeric(difftime(max(Agg_Trips_1$Date), Start_date, units = 'days'))
time_window <- 7
Agg_Trips_1$Pct_Age_25_65 <- Agg_Trips_1$Pct_Age_25_40 + Agg_Trips_1$Pct_Age_40_65

# LAG 8
Agg_Trips_1 <- Agg_Trips_1 %>%
  group_by(CTFIPS) %>%
  mutate(Lag8_Log_Risked_WInput = dplyr::lag(Log_Risked_WInput, n = 8, default = NA))
Agg_Trips_1 <- Agg_Trips_1 %>%
  group_by(CTFIPS) %>%
  mutate(Lag8_Log_InFlow_Weight = dplyr::lag(Log_InFlow_Weight, n = 8, default = NA))
Agg_Trips_1 <- Agg_Trips_1 %>%
  group_by(CTFIPS) %>%
  mutate(Lag7_Log_National_Cases = dplyr::lag(Log_National_Cases, n = 7, default = NA))
Agg_Trips_1 <- Agg_Trips_1 %>%
  group_by(CTFIPS) %>%
  mutate(Lag7_PRCP_NEW = dplyr::lag(PRCP_NEW, n = 7, default = NA))
Agg_Trips_1 <- Agg_Trips_1 %>%
  group_by(CTFIPS) %>%
  mutate(Lag7_TMAX = dplyr::lag(TMAX, n = 7, default = NA))
Agg_Trips_1 <- subset(Agg_Trips_1, select = -c(New_cases_rate))
colSums(is.na(Agg_Trips_1))

# Model compare
# SEM FOR ALL STATES
Agg_Trips_tem <- Agg_Trips_1[(Agg_Trips_1$Date > Start_date) &
                               (Agg_Trips_1$New_cases > 0) &
                               (Agg_Trips_1$InFlow_Weight > 0),]
Agg_Trips_tem <- na.omit(Agg_Trips_tem)
fit <- sem('Log_New_cases ~ Lag7_Log_InFlow_Weight + Lag1_Log_New_cases + Is_Weekend  + Population_density  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 + Med_House_Income
              Lag7_Log_InFlow_Weight ~  Lag7_Log_National_Cases + Lag8_Log_InFlow_Weight + Is_Weekend + Population_density + Employment_density + Lag7_PRCP_NEW + Lag7_TMAX  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 +  Med_House_Income  + Pct_Black + Pct_White', data = Agg_Trips_tem)
fit1 <- sem('Log_New_cases ~  Lag1_Log_New_cases + Is_Weekend  + Population_density  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 + Med_House_Income
              Lag7_Log_InFlow_Weight ~  Lag7_Log_National_Cases + Lag8_Log_InFlow_Weight + Is_Weekend + Population_density + Employment_density + Lag7_PRCP_NEW + Lag7_TMAX  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 +  Med_House_Income  + Pct_Black + Pct_White', data = Agg_Trips_tem)
compareFit(fit, fit1, nested = FALSE)
model_performance(fit, metrics = "all")
summary(fit)
# Abs calculate
# How many cases are casued by inflow?
# Extract the new cases model
Agg_Trips_1_tem <- na.omit(Agg_Trips_1)
model_lme <-
  lm(Log_New_cases ~ 1 +
    Lag7_Log_InFlow_Weight +
    Lag1_Log_New_cases +
    Population_density +
    Pct_Age_0_24 +
    Pct_Age_25_40 +
    Pct_Age_40_65 +
    Med_House_Income +
    Is_Weekend, na.action = na.omit, data = Agg_Trips_1_tem)
summary(model_lme)

Agg_Trips_1_tem$sem_fitted <- predict(model_lme)
Agg_Trips_1_tem$Pred_Cases <- exp(Agg_Trips_1_tem$sem_fitted)
sum(Agg_Trips_1_tem$New_cases)
sum(exp(Agg_Trips_1_tem$sem_fitted))
Agg_Trips_1_pred <- Agg_Trips_1_tem
Agg_Trips_1_pred$Lag7_Log_InFlow_Weight <- log(exp(Agg_Trips_1_pred$Lag7_Log_InFlow_Weight) * 1.1)
Agg_Trips_1_pred$Pred_Log_Inflow_Case_new <- predict(model_lme, Agg_Trips_1_pred)
Agg_Trips_1_pred$Pred_Cases_new <- exp(Agg_Trips_1_pred$Pred_Log_Inflow_Case_new)
sum(exp(Agg_Trips_1_pred$Pred_Log_Inflow_Case_new)) - sum(exp(Agg_Trips_1_tem$sem_fitted))

sum(Agg_Trips_1_pred$New_cases)
sum(Agg_Trips_1_pred$Pred_Cases)
sum(Agg_Trips_1_pred$Pred_Cases_new)
sum(Agg_Trips_1_pred$Pred_Cases_new) - sum(Agg_Trips_1_pred$Pred_Cases)
(sum(Agg_Trips_1_pred$Pred_Cases_new) - sum(Agg_Trips_1_pred$Pred_Cases)) / sum(Agg_Trips_1_pred$New_cases)

Pred_Summa <- select(Agg_Trips_1_pred, New_cases, Pred_Cases, Pred_Cases_new, Date) %>%
  group_by(Date) %>%
  summarise_each(funs(sum))

ggplot() +
  #geom_ribbon(data = All_corr_[[3]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), alpha = 0.2, colour = 'red') +
  geom_line(data = Pred_Summa, aes(x = Date, y = New_cases, colour = "New_cases"), size = 1) +
  geom_line(data = Pred_Summa, aes(x = Date, y = Pred_Cases, colour = "Pred_Cases"), size = 1) +
  geom_line(data = Pred_Summa, aes(x = Date, y = Pred_Cases_new, colour = "Pred_Cases_1.1INFLOW"), size = 1)

max(Agg_Trips_1_pred$Date)

# Abs calculate / Prediction
Agg_Trips_1_tem <- na.omit(Agg_Trips_1)
Agg_Trips_1_tem$CTFIPS <- as.factor(Agg_Trips_1_tem$CTFIPS)
model_gam <-
  bam(Log_New_cases ~ 1 +
    s(Lag7_Log_InFlow_Weight) +
    s(Lag1_Log_New_cases) +
    Population_density +
    Pct_Age_0_24 +
    Pct_Age_25_40 +
    Pct_Age_40_65 +
    Med_House_Income +
    Week +
    s(LAT, LNG, bs = 'gp') +
    s(Time_Index) +
    s(CTFIPS, bs = 're'),
      control = gam.control(trace = TRUE),
      method = "fREML", discrete = TRUE, nthreads = 100, data = Agg_Trips_1_tem)

#summary(model_gam)
Agg_Trips_1_tem$gam_fitted <- predict(model_gam)
sum(Agg_Trips_1_tem$New_cases)
sum(exp(Agg_Trips_1_tem$gam_fitted))

# Predict
Agg_Trips_1_pred <- Agg_Trips_1_tem
Agg_Trips_1_pred$Pred_Cases <- exp(Agg_Trips_1_pred$gam_fitted)
Agg_Trips_1_pred$Lag7_Log_InFlow_Weight <- log(exp(Agg_Trips_1_tem$Lag7_Log_InFlow_Weight) * 1.1)
Agg_Trips_1_pred$Pred_Log_Inflow_Case_new <- predict(model_gam, Agg_Trips_1_pred)
Agg_Trips_1_pred$Pred_Cases_new1.1 <- exp(Agg_Trips_1_pred$Pred_Log_Inflow_Case_new)
Agg_Trips_1_pred$Lag7_Log_InFlow_Weight <- log(exp(Agg_Trips_1_tem$Lag7_Log_InFlow_Weight) * 1.2)
Agg_Trips_1_pred$Pred_Log_Inflow_Case_new <- predict(model_gam, Agg_Trips_1_pred)
Agg_Trips_1_pred$Pred_Cases_new1.2 <- exp(Agg_Trips_1_pred$Pred_Log_Inflow_Case_new)

sum(Agg_Trips_1_pred$New_cases)
sum(Agg_Trips_1_pred$Pred_Cases)
sum(Agg_Trips_1_pred$Pred_Cases_new1.1)
sum(Agg_Trips_1_pred$Pred_Cases_new1.2)

Agg_Trips_1_pred$CTFIPS <- as.numeric(Agg_Trips_1_pred$CTFIPS)
Pred_Summa <- select(Agg_Trips_1_pred, New_cases, Pred_Cases, Pred_Cases_new1.1, Pred_Cases_new1.2, Date) %>%
  group_by(Date) %>%
  summarise_each(funs(sum))

ggplot() +
  #geom_ribbon(data = All_corr_[[3]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), alpha = 0.2, colour = 'red') +
  geom_line(data = Pred_Summa, aes(x = Date, y = New_cases, colour = "New_cases"), size = 1) +
  geom_line(data = Pred_Summa, aes(x = Date, y = Pred_Cases, colour = "Pred_Cases"), size = 1) +
  geom_line(data = Pred_Summa, aes(x = Date, y = Pred_Cases_new1.1, colour = "Pred_Cases_1.1INFLOW"), size = 1) +
  geom_line(data = Pred_Summa, aes(x = Date, y = Pred_Cases_new1.2, colour = "Pred_Cases_1.2INFLOW"), size = 1)