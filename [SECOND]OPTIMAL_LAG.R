library(lavaan)
library(ggplot2)
library(semPlot)
library(lavaanPlot)
library(dplyr)
library(piecewiseSEM)
library(nlme)
library(lme4)

# Read DATA
Agg_Trips_1 <- read.csv('D:/COVID-19/PNAS_SECOND/All_XY_Features_To_R_County_Level_0731_toR.csv')
Agg_Trips_1 <- Agg_Trips_1[with(Agg_Trips_1, order(CTFIPS, Date)),]
Agg_Trips_1$Is_ReopenState <- TRUE
Agg_Trips_1$Date <- as.Date(Agg_Trips_1$Date)
Start_date <- as.Date('2020-03-10')
Max_Day <- as.numeric(difftime(max(Agg_Trips_1$Date), Start_date, units = 'days'))
time_window <- 7

# A function for all state
All_State_SEM <- function(Max_Day, Agg_Trips_1, time_window, model, xvar) {
  # RUN LOOP
  Start_date <- as.Date('2020-03-10')
  All_corr_Reopen <- c()
  All_Predict <- c()
  for (jj in (1:(Max_Day - 7))) {
    print(jj)
    skip_to_next <- FALSE
    Agg_Trips_tem <- Agg_Trips_1[(Agg_Trips_1$Date <= Start_date + time_window) &
                                   (Agg_Trips_1$Date > Start_date) &
                                   (Agg_Trips_1$New_cases > 0) &
                                   (Agg_Trips_1$InFlow_Weight > 0),]
    Agg_Trips_tem <- na.omit(Agg_Trips_tem)
    rownames(Agg_Trips_tem) <- NULL
    tryCatch({
               fit <- sem(model, data = Agg_Trips_tem) # ,orthogonal = TRUE,std.lv = TRUE
               #anova(fit, fit.partial)
               para <- parameterEstimates(fit)
               # Predict the cases
               Beta <- para[(para$lhs == 'Log_New_cases') & (para$op == "~"), 'est']
               xNames <- para[(para$lhs == 'Log_New_cases') & (para$op == "~"), 'rhs']
               Agg_Trips_tem$pred.cases <- c(as.matrix(Agg_Trips_tem[xNames]) %*% Beta)
               para$Date <- Start_date
               All_corr_Reopen[[jj]] <- para
               All_Predict[[jj]] <- Agg_Trips_tem
             },
             error = function(e) { skip_to_next <<- TRUE })
    Start_date <- Start_date + 1
    if (skip_to_next) { next }
  }
  All_corr_Reopen <- do.call(rbind.data.frame, All_corr_Reopen)
  All_predict <- do.call(rbind.data.frame, All_Predict)
  # PLOT COEFFI
  All_corr_1_Reopen <- All_corr_Reopen[(All_corr_Reopen$lhs == 'Log_New_cases') &
                                         (All_corr_Reopen$rhs == xvar) &
                                         (All_corr_Reopen$Date > as.Date('2020-03-10')) &
                                         (All_corr_Reopen$pvalue < 0.1),]
  All_corr_1_Reopen <- na.omit(All_corr_1_Reopen)
  rownames(All_corr_1_Reopen) <- NULL
  # Print RESIDUAL
  # Let <0 to 0
  All_predict$pred.cases[All_predict$pred.cases < 0] <- 0
  A_resi <- mean(abs(All_predict$pred.cases - All_predict$Log_New_cases))
  list_result <- list(All_corr_Reopen, All_predict, All_corr_1_Reopen, A_resi)
  return(list_result)
}

# LOOP IN LAG
# FINISH A LAG FIRST
All_resid <- c()
All_resid_result <- c()
for (jj in (0:30)) {
  lag_data <- Agg_Trips_1 %>%
    group_by(CTFIPS) %>%
    mutate(lag.value = dplyr::lag(Log_Risked_WInput, n = jj, default = NA))
  #lag_data <-lag_data[!rowSums(is.na(lag_data['lag.value15'])), ]
  lag_data <- lag_data %>%
    group_by(CTFIPS) %>%
    filter((row_number() > 30))
  All_corr_1 <- All_State_SEM(Max_Day, lag_data, time_window,
                              model = 'Log_New_cases ~ lag.value  + Is_Weekend  + Population_density  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65',
                              xvar = 'lag.value')
  All_resid[[jj + 1]] <- All_corr_1[[4]]
  All_corr_1[[2]]$LAG_NUM <- jj + 1
  All_resid_result[[jj + 1]] <- select(All_corr_1[[2]], pred.cases, Log_New_cases, LAG_NUM)
}

All_resid1 <- do.call(rbind.data.frame, All_resid)
colnames(All_resid1) <- 'resi'
plot(All_resid1$resi)

All_resid2 <- do.call(rbind.data.frame, All_resid_result)
All_resid2$resid <- abs(All_resid2$pred.cases - All_resid2$Log_New_cases)
# Plot bar
boxplot(resid ~ LAG_NUM, data = All_resid2, outline = FALSE, range = 1)

Resid_summ <- All_resid2 %>%
  group_by(LAG_NUM) %>%
  summarise_each(funs(mean, median, sd))

ggplot(Resid_summ, aes(x = LAG_NUM, y = resid_mean)) +
  geom_errorbar(aes(ymin = resid_mean - resid_sd * 0.05, ymax = resid_mean + resid_sd * 0.05), width = 0.5) +
  geom_line() +
  geom_point() +
  labs(x = "Lag", y = "MAE") +
  theme_bw()

ggplot(Resid_summ, aes(x = LAG_NUM, y = resid_mean)) +
  geom_line() +
  geom_point() +
  theme_bw()

# TRY PANLE MODEL
# A function for all state
All_State_SEM_Panel_Lag <- function(Max_Day, Agg_Trips_1, time_window) {
  # RUN LOOP
  All_resid <- c()
  Start_date <- as.Date('2020-03-10')
  for (jj in (1:(Max_Day - 7))) {
    print(jj)
    skip_to_next <- FALSE
    Agg_Trips_tem <- Agg_Trips_1[(Agg_Trips_1$Date <= Start_date + time_window) &
                                   (Agg_Trips_1$Date > Start_date) &
                                   (Agg_Trips_1$New_cases > 0) &
                                   (Agg_Trips_1$InFlow_Weight > 0),]
    Agg_Trips_tem <- na.omit(Agg_Trips_tem)
    rownames(Agg_Trips_tem) <- NULL
    tryCatch({
               model.list <- list(
                 lme(Log_New_cases ~ lag.value +
                   Lag1_Log_New_cases +
                   Is_Weekend +
                   Population_density +
                   Pct_Age_0_24 +
                   Pct_Age_25_65 +
                   Med_House_Income, random = ~1 | CTFIPS, na.action = na.omit, data = Agg_Trips_tem))
               fit <- as.psem(model.list) # ,orthogonal = TRUE,std.lv = TRUE
               #new.summary <- summary(fit, .progressBar = F)
               resi <- data.frame(residuals(fit))
               #anova(fit, fit.partial)
               All_resid[[jj]] <- resi
             },
             error = function(e) { skip_to_next <<- TRUE })
    Start_date <- Start_date + 1
    if (skip_to_next) { next }
  }
  All_resid <- do.call(rbind.data.frame, All_resid)
  return(All_resid)
}

# LOOP IN LAG
# FINISH A LAG FIRST
All_resid <- c()
for (jj in (0:30)) {
  lag_data <- Agg_Trips_1 %>%
    group_by(CTFIPS) %>%
    mutate(lag.value = dplyr::lag(Log_Risked_WInput, n = jj, default = NA))
  #lag_data <-lag_data[!rowSums(is.na(lag_data['lag.value15'])), ]
  lag_data <- lag_data %>%
    group_by(CTFIPS) %>%
    filter((row_number() > 30))
  All_corr_1 <- All_State_SEM_Panel_Lag(Max_Day, lag_data, time_window)
  All_corr_1$LAG_NUM <- jj + 1
  All_resid[[jj + 1]] <- All_corr_1
}

All_resid1 <- do.call(rbind.data.frame, All_resid)
All_resid1$Log_New_cases_residuals <- abs(All_resid1$Log_New_cases_residuals)
# Plot bar
boxplot(Log_New_cases_residuals ~ LAG_NUM, data = All_resid1, outline = FALSE, range = 0.5)

Resid_summ <- All_resid1 %>%
  group_by(LAG_NUM) %>%
  summarise_each(funs(mean, median, sd))

ggplot(Resid_summ, aes(x = LAG_NUM, y = median)) +
  geom_line() +
  geom_point() +
  theme_bw()

ggplot(Resid_summ, aes(x = LAG_NUM, y = median)) +
  geom_errorbar(aes(ymin = median - sd, ymax = median + sd), width = 0) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Coeff") +
  theme_bw()