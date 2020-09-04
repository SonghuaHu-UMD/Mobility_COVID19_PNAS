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
library(zoo)

# Read DATA
Agg_Trips_1 <- read.csv('D:/COVID-19/PNAS_SECOND/All_XY_Features_To_R_County_Level_0731_toR.csv')
Agg_Trips_1 <- Agg_Trips_1[with(Agg_Trips_1, order(CTFIPS, Date)),]
Agg_Trips_1$Is_ReopenState <- TRUE
Agg_Trips_1$Date <- as.Date(Agg_Trips_1$Date)
Start_date <- as.Date('2020-03-10')
Max_Day <- as.numeric(difftime(max(Agg_Trips_1$Date), Start_date, units = 'days'))
Agg_Trips_1$TSFIPS <- 1
time_window <- 7
Agg_Trips_1$Pct_Age_25_65 <- Agg_Trips_1$Pct_Age_25_40 + Agg_Trips_1$Pct_Age_40_65

# LAG 8
Agg_Trips_1 <- Agg_Trips_1 %>%
  group_by(CTFIPS) %>%
  mutate(Lag8_Log_InFlow_Weight = dplyr::lag(Log_InFlow_Weight, n = 8, default = NA))
Agg_Trips_1 <- Agg_Trips_1 %>%
  group_by(CTFIPS) %>%
  mutate(Lag7_Log_National_Cases = dplyr::lag(Log_National_Cases, n = 7, default = NA))
Agg_Trips_1 <- Agg_Trips_1 %>%
  group_by(CTFIPS) %>%
  mutate(Lag7_Log_National_Cases_Reopen = dplyr::lag(Log_National_Cases_Reopen, n = 7, default = NA))
Agg_Trips_1 <- Agg_Trips_1 %>%
  group_by(CTFIPS) %>%
  mutate(Lag7_Log_National_Cases_Close = dplyr::lag(Log_National_Cases_Close, n = 7, default = NA))
#Agg_Trips_1 <- Agg_Trips_1 %>%
#  group_by(CTFIPS) %>%
#  mutate(Lag7_Log_New_cases = dplyr::lag(Log_New_cases, n = 7, default = NA))
Agg_Trips_1 <- Agg_Trips_1 %>%
  group_by(CTFIPS) %>%
  mutate(Lag7_PRCP_NEW = dplyr::lag(PRCP_NEW, n = 7, default = NA))
Agg_Trips_1 <- Agg_Trips_1 %>%
  group_by(CTFIPS) %>%
  mutate(Lag7_TMAX = dplyr::lag(TMAX, n = 7, default = NA))
Agg_Trips_1 <- subset(Agg_Trips_1, select = -c(New_cases_rate, Lag7_Log_Risked_WInput))
colSums(is.na(Agg_Trips_1))


# SEM PANEL MODEL
# A function for all state
All_State_SEM_Panel <- function(Max_Day, Agg_Trips_1, time_window, xvar) {
  # RUN LOOP
  All_corr_Reopen <- c()
  All_perform <- c()
  Start_date <- as.Date('2020-03-10')
  for (jj in (1:(Max_Day - 7))) {
    print(jj)
    skip_to_next <- FALSE
    Agg_Trips_tem <- Agg_Trips_1[(Agg_Trips_1$Date <= Start_date + time_window) &
                                   (Agg_Trips_1$Date > Start_date) &
                                   (Agg_Trips_1$New_cases > 0) &
                                   (Agg_Trips_1$InFlow_Weight > 0),]
    Agg_Trips_tem <-
      subset(Agg_Trips_tem, select
        = c(Log_New_cases, Lag7_Log_InFlow_Weight, Lag1_Log_New_cases, Is_Weekend, Population_density, Pct_Age_0_24, Pct_Age_25_40,
            Pct_Age_25_40, Pct_Age_40_65, Med_House_Income, Lag7_Log_InFlow_Weight, Lag7_Log_National_Cases, Lag8_Log_InFlow_Weight,
            Lag7_PRCP_NEW, Lag7_TMAX, Pct_Black, Pct_White, Employment_density, CTFIPS, Week))
    Agg_Trips_tem <- na.omit(Agg_Trips_tem)
    rownames(Agg_Trips_tem) <- NULL
    tryCatch({
      model.list <- list(
        lme(Log_New_cases ~ Lag7_Log_InFlow_Weight +
          Lag1_Log_New_cases +
          Is_Weekend +
          Population_density +
          Pct_Age_0_24 +
          Pct_Age_25_40 +
          Pct_Age_40_65 +
          Med_House_Income, random = ~1 | CTFIPS, na.action = na.omit, data = Agg_Trips_tem),
        lme(Lag7_Log_InFlow_Weight ~ Lag7_Log_National_Cases +
          Lag8_Log_InFlow_Weight +
          Is_Weekend +
          Population_density +
          Employment_density +
          Lag7_PRCP_NEW +
          Lag7_TMAX +
          Pct_Age_0_24 +
          Pct_Age_25_40 +
          Pct_Age_40_65 +
          Med_House_Income +
          Pct_Black +
          Pct_White, random = ~1 | CTFIPS, na.action = na.omit, data = Agg_Trips_tem))
      fit <- as.psem(model.list) # ,orthogonal = TRUE,std.lv = TRUE
      new.summary <- summary(fit, .progressBar = F)
      #residuals(fit)
      para <- coefs(fit, standardize = "scale", intercepts = TRUE)
      #anova(fit, fit.partial)
      #para <- (new.summary$coefficients)
      para$Date <- Start_date
      All_corr_Reopen[[jj]] <- para
      All_perform[[jj]] <- new.summary$R2
    },
      error = function(e) { skip_to_next <<- TRUE })
    Start_date <- Start_date + 1
    if (skip_to_next) { next }
  }
  All_corr_Reopen <- do.call(rbind.data.frame, All_corr_Reopen)
  # PLOT COEFFI
  All_corr_1_Reopen <- All_corr_Reopen[(All_corr_Reopen$Response == 'Log_New_cases') &
                                         (All_corr_Reopen$Predictor == xvar) &
                                         (All_corr_Reopen$Date > as.Date('2020-03-10')) &
                                         (All_corr_Reopen$P.Value < 0.1),]
  All_corr_1_Reopen <- na.omit(All_corr_1_Reopen)
  rownames(All_corr_1_Reopen) <- NULL

  All_corr_1_Reopen_1 <- All_corr_Reopen[(All_corr_Reopen$Response == xvar) &
                                           (All_corr_Reopen$Predictor == 'Lag7_Log_National_Cases') &
                                           (All_corr_Reopen$Date > as.Date('2020-03-10')) &
                                           (All_corr_Reopen$P.Value < 0.1),]
  All_corr_1_Reopen_1 <- na.omit(All_corr_1_Reopen_1)
  rownames(All_corr_1_Reopen_1) <- NULL

  list_result <- list(All_corr_Reopen, All_corr_1_Reopen)
  return(list_result)
}

All_corr_ <- All_State_SEM_Panel(Max_Day, Agg_Trips_1, time_window, xvar = 'Lag7_Log_InFlow_Weight')

ggplot(All_corr_[[2]], aes(x = Date, y = Estimate)) +
  geom_ribbon(aes(ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), alpha = 0.2, colour = NA) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Coeff") +
  theme_bw()

# How other coefficient looks like
All_corr_Reopen <- All_corr_[[1]]
All_corr_1_Reopen <- All_corr_Reopen[(All_corr_Reopen$Date > as.Date('2020-03-10')) &
                                       (All_corr_Reopen$P.Value < 0.1),]
Coeff_Other <- select(All_corr_1_Reopen, Response, Predictor, Std.Estimate) %>%
  group_by(Response, Predictor) %>%
  summarise_each(funs(mean, median, sd, sum(!is.na(.))))

# Split the state
Split_State_SEM_Panel <- function(Max_Day, Agg_Trips_1, time_window, xvar, Idea_Reopen_State) {
  # RUN LOOP
  #Idea_Reopen_State <- c(51, 37, 27, 49, 4, 48, 12, 28, 01, 06, 19)
  Agg_Trips_1$Is_ReopenState <- Agg_Trips_1$STFIPS %in% Idea_Reopen_State
  All_corr_Reopen <- c()
  All_corr_Close <- c()
  Start_date <- as.Date('2020-03-10')
  for (jj in (1:(Max_Day - 7))) {
    print(jj)
    skip_to_next <- FALSE
    Agg_Trips_tem <- Agg_Trips_1[(Agg_Trips_1$Date <= Start_date + time_window) &
                                   (Agg_Trips_1$Date > Start_date) &
                                   (Agg_Trips_1$New_cases > 0) &
                                   (Agg_Trips_1$InFlow_Weight > 0),]
    Agg_Trips_tem <- select(Agg_Trips_tem, Log_New_cases, Lag7_Log_InFlow_Weight, Lag1_Log_New_cases, Is_Weekend,
                            Population_density, Pct_Age_0_24, Pct_Age_25_40, Pct_Age_40_65, Med_House_Income,
                            Lag7_Log_InFlow_Weight, Lag7_Log_National_Cases_Reopen, Lag7_Log_National_Cases_Close,
                            Lag8_Log_InFlow_Weight, Employment_density, Lag7_PRCP_NEW, Lag7_TMAX,
                            Pct_Black, Pct_White, Is_ReopenState, Week, TSFIPS)

    Agg_Trips_tem <- na.omit(Agg_Trips_tem)
    rownames(Agg_Trips_tem) <- NULL
    Reopen_tem <- Agg_Trips_tem[Agg_Trips_tem$Is_ReopenState,]
    Close_tem <- Agg_Trips_tem[!Agg_Trips_tem$Is_ReopenState,]
    tryCatch({
      model.list <- list(
        lme(Log_New_cases ~ 1 +
          Lag7_Log_InFlow_Weight +
          Lag1_Log_New_cases +
          Is_Weekend +
          Population_density +
          Pct_Age_0_24 +
          Pct_Age_25_40 +
          Pct_Age_40_65 +
          Med_House_Income, random = ~1 | CTFIPS, na.action = na.omit, data = Reopen_tem),
        lme(Lag7_Log_InFlow_Weight ~ 1 +
          Lag7_Log_National_Cases_Reopen +
          Lag8_Log_InFlow_Weight +
          Is_Weekend +
          Population_density +
          Employment_density +
          Lag7_PRCP_NEW +
          Lag7_TMAX +
          Pct_Age_0_24 +
          Pct_Age_25_40 +
          Pct_Age_40_65 +
          Med_House_Income +
          Pct_Black +
          Pct_White, random = ~1 | CTFIPS, na.action = na.omit, data = Reopen_tem))
      model.list1 <- list(
        lme(Log_New_cases ~ 1 +
          Lag7_Log_InFlow_Weight +
          Lag1_Log_New_cases +
          Is_Weekend +
          Population_density +
          Pct_Age_0_24 +
          Pct_Age_25_40 +
          Pct_Age_40_65 +
          Med_House_Income, random = ~1 | CTFIPS, na.action = na.omit, data = Close_tem),
        lme(Lag7_Log_InFlow_Weight ~ 1 +
          Lag7_Log_National_Cases_Close +
          Lag8_Log_InFlow_Weight +
          Is_Weekend +
          Population_density +
          Employment_density +
          Lag7_PRCP_NEW +
          Lag7_TMAX +
          Pct_Age_0_24 +
          Pct_Age_25_40 +
          Pct_Age_40_65 +
          Med_House_Income +
          Pct_Black +
          Pct_White, random = ~1 | CTFIPS, na.action = na.omit, data = Close_tem))

      fit <- as.psem(model.list)  # ,orthogonal = TRUE,std.lv = TRUE
      fit1 <- as.psem(model.list1)  # ,orthogonal = TRUE,std.lv = TRUE
      #anova(fit, fit.partial)
      para <- coefs(fit, standardize = "scale", intercepts = TRUE)
      para1 <- coefs(fit1, standardize = "scale", intercepts = TRUE)
      para$Date <- Start_date
      para1$Date <- Start_date
      All_corr_Reopen[[jj]] <- para
      All_corr_Close[[jj]] <- para1
    },
      error = function(e) { skip_to_next <<- TRUE })
    Start_date <- Start_date + 1
    if (skip_to_next) { next }
  }
  All_corr_Reopen <- do.call(rbind.data.frame, All_corr_Reopen)
  All_corr_Close <- do.call(rbind.data.frame, All_corr_Close)

  # PLOT COEFFI
  All_corr_1_Reopen <- All_corr_Reopen[(All_corr_Reopen$Response == 'Log_New_cases') &
                                         (All_corr_Reopen$Predictor == xvar) &
                                         (All_corr_Reopen$Date > as.Date('2020-03-10')) &
                                         (All_corr_Reopen$P.Value < 0.1),]
  All_corr_1_Reopen <- na.omit(All_corr_1_Reopen)
  rownames(All_corr_1_Reopen) <- NULL
  All_corr_1_Close <- All_corr_Close[(All_corr_Close$Response == 'Log_New_cases') &
                                       (All_corr_Close$Predictor == xvar) &
                                       (All_corr_Close$Date > as.Date('2020-03-10')) &
                                       (All_corr_Close$P.Value < 0.1),]
  All_corr_1_Close <- na.omit(All_corr_1_Close)
  rownames(All_corr_1_Close) <- NULL
  list_result <- list(All_corr_Reopen, All_corr_Close, All_corr_1_Reopen, All_corr_1_Close)
  return(list_result)
}

# c(12, 6, 22, 13, 1, 17, 4, 47, 37, 45, 32, 51)
#c(1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40, 45, 46, 47, 48, 49)
All_corr_ <- Split_State_SEM_Panel(Max_Day, Agg_Trips_1, 7,
                                   xvar = 'Lag7_Log_InFlow_Weight',
                                   Idea_Reopen_State = c(1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40, 45, 46, 47, 48, 49))
ggplot() +
  #geom_ribbon(data = All_corr_[[3]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), alpha = 0.2, colour = 'red') +
  geom_errorbar(data = All_corr_[[3]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.5) +
  geom_line(data = All_corr_[[3]], aes(x = Date, y = Estimate, colour = "Reopen"), size = 1) +
  geom_point(data = All_corr_[[3]], aes(x = Date, y = Estimate)) +
  #geom_ribbon(data = All_corr_[[4]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), alpha = 0.2, colour = 'green') +
  geom_errorbar(data = All_corr_[[4]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.5) +
  geom_line(data = All_corr_[[4]], aes(x = Date, y = Estimate, colour = "Lock-Down"), size = 1) +
  geom_point(data = All_corr_[[4]], aes(x = Date, y = Estimate)) +
  labs(x = "Date", y = "Coeff") +
  theme_bw()
