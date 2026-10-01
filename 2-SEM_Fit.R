# The script build a SEM varying in a 7 days time window
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
Agg_Trips_1 <- read.csv(Sys.getenv('PNAS_DATA_PATH', 'Data/All_XY_Features_To_R_County_Level_0731_toR.csv.gz'))
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
Agg_Trips_1 <- Agg_Trips_1[, setdiff(names(Agg_Trips_1), c('New_cases_rate', 'Lag7_Log_Risked_WInput'))]
colSums(is.na(Agg_Trips_1))

# SEM PANEL MODEL
# A function for all state
# Complete coefficient series; failures remain NA and are logged.
validate_window <- function(time_window) {
  if (length(time_window) != 1 || !is.finite(time_window) ||
      time_window < 1 || time_window != as.integer(time_window)) stop("Invalid time_window")
}
curve <- function(tab, dates, response, predictor) {
  selected <- tab[tab$Response == response & tab$Predictor == predictor, , drop=FALSE]
  out <- merge(data.frame(Date=dates), selected, by="Date", all.x=TRUE, sort=TRUE)
  out$Significant_0_1 <- !is.na(out$P.Value) & out$P.Value < 0.1
  out
}
fit_panel_windows <- function(Max_Day, data, time_window, xvar, states=NULL) {
  validate_window(time_window)
  dates <- as.Date("2020-03-10") + seq_len(max(0, Max_Day - time_window)) - 1
  groups <- if (is.null(states)) "National" else c("Reopen", "Close")
  coeff <- setNames(lapply(groups, function(g) list()), groups)
  performance <- setNames(lapply(groups, function(g) list()), groups)
  logs <- list()
  for (jj in seq_along(dates)) {
    date <- dates[jj]
    window <- data[data$Date >= date & data$Date < date + time_window &
                   data$New_cases > 0 & data$InFlow_Weight > 0, , drop=FALSE]
    for (group in groups) {
      part <- window
      if (!is.null(states)) {
        reopened <- part$STFIPS %in% states
        part <- part[if (group == "Reopen") reopened else !reopened, , drop=FALSE]
      }
      national <- switch(group, National="Lag7_Log_National_Cases",
                         Reopen="Lag7_Log_National_Cases_Reopen", Close="Lag7_Log_National_Cases_Close")
      formula1 <- as.formula(paste("Log_New_cases ~", xvar,
        "+ Lag1_Log_New_cases + Is_Weekend + Population_density + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 + Med_House_Income"))
      formula2 <- as.formula(paste(xvar, "~", national,
        "+ Lag8_Log_InFlow_Weight + Is_Weekend + Population_density + Employment_density + Lag7_PRCP_NEW + Lag7_TMAX + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 + Med_House_Income + Pct_Black + Pct_White"))
      needed <- unique(c(all.vars(formula1), all.vars(formula2)))
      part <- part[complete.cases(part[, needed, drop=FALSE]), , drop=FALSE]
      error_text <- ""
      ok <- tryCatch({
        if (!nrow(part)) stop("No complete observations")
        fit <- as.psem(list(lm(formula1, data=part, na.action=na.fail),
                            lm(formula2, data=part, na.action=na.fail)))
        para <- coefs(fit, standardize="scale", intercepts=TRUE)
        para$Date <- date
        para$Significant_0_1 <- !is.na(para$P.Value) & para$P.Value < 0.1
        coeff[[group]][[jj]] <- para
        perf <- summary(fit, .progressBar=FALSE, rsq=TRUE)$R2
        perf$Date <- date
        perf$Evaluation <- "in_sample"
        performance[[group]][[jj]] <- perf
        TRUE
      }, error=function(e) { error_text <<- conditionMessage(e); FALSE })
      logs[[length(logs)+1]] <- data.frame(Date=date, Group=group,
        Eligible=nrow(window), Complete=nrow(part), Success=ok, Error=error_text)
    }
  }
  write.csv(do.call(rbind, logs), if (is.null(states)) "National_fit_status.csv" else "Split_fit_status.csv", row.names=FALSE)
  empty <- data.frame(Date=as.Date(character()), Response=character(), Predictor=character(),
                      Estimate=numeric(), Std.Error=numeric(), P.Value=numeric())
  for (group in groups) {
    coeff[[group]] <- if (length(coeff[[group]])) do.call(rbind, coeff[[group]]) else empty
  }
  if (is.null(states)) return(list(coeff$National,
    curve(coeff$National, dates, "Log_New_cases", xvar),
    curve(coeff$National, dates, xvar, "Lag7_Log_National_Cases"), performance$National))
  list(coeff$Reopen, coeff$Close,
    curve(coeff$Reopen, dates, "Log_New_cases", xvar),
    curve(coeff$Close, dates, "Log_New_cases", xvar),
    curve(coeff$Reopen, dates, xvar, "Lag7_Log_National_Cases_Reopen"),
    curve(coeff$Close, dates, xvar, "Lag7_Log_National_Cases_Close"),
    performance$Reopen, performance$Close)
}
All_State_SEM_Panel <- function(Max_Day, Agg_Trips_1, time_window, xvar) {
  fit_panel_windows(Max_Day, Agg_Trips_1, time_window, xvar)
}

All_corr_ <- All_State_SEM_Panel(Max_Day, Agg_Trips_1, 7, xvar = 'Lag7_Log_InFlow_Weight')

All_corr_perform <- do.call(rbind.data.frame, All_corr_[[4]])
mean(All_corr_perform[All_corr_perform$Response == 'Log_New_cases', 'R.squared'])
min(All_corr_perform[All_corr_perform$Response == 'Log_New_cases', 'R.squared'])
max(All_corr_perform[All_corr_perform$Response == 'Log_New_cases', 'R.squared'])

mean(All_corr_perform[All_corr_perform$Response == 'Lag7_Log_InFlow_Weight', 'R.squared'])
min(All_corr_perform[All_corr_perform$Response == 'Lag7_Log_InFlow_Weight', 'R.squared'])
max(All_corr_perform[All_corr_perform$Response == 'Lag7_Log_InFlow_Weight', 'R.squared'])


ggplot(All_corr_[[2]], aes(x = Date, y = Estimate)) +
  geom_ribbon(aes(ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), alpha = 0.2, colour = NA) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "In-sample coefficient (±1 SE)") +
  theme_bw()

#All_corr_date <- zoo(All_corr_[[2]]$Estimate, All_corr_[[2]]$Date)
#plot(rollmean(All_corr_date, 7))

write.csv(All_corr_[[2]], 'National.csv')
write.csv(All_corr_[[3]], 'National_1.csv')
Inter_cases <- All_corr_[[1]][All_corr_[[1]]$Response == 'Log_New_cases' &
                                All_corr_[[1]]$Predictor == '(Intercept)',] #All_corr_[[1]]$P.Value < 0.1
write.csv(Inter_cases, 'National_interc_cases.csv')
Inter_FLOW <- All_corr_[[1]][All_corr_[[1]]$Response == 'Lag7_Log_InFlow_Weight' &
                               All_corr_[[1]]$Predictor == '(Intercept)',] #All_corr_[[1]]$P.Value < 0.1
write.csv(Inter_FLOW, 'National_interc_flow.csv')


# How other coefficient looks like
All_corr_Reopen <- All_corr_[[1]]
All_corr_1_Reopen <- All_corr_Reopen[(All_corr_Reopen$Date > as.Date('2020-03-10')) &
                                       TRUE,]
Coeff_Other <- select(All_corr_1_Reopen, Response, Predictor, Estimate) %>%
  group_by(Response, Predictor) %>%
  summarise_each(funs(mean, median, sd, min, max, sum(!is.na(.))))
write.csv(Coeff_Other, 'Coeff_Other.csv')

# Split the state
Split_State_SEM_Panel <- function(Max_Day, Agg_Trips_1, time_window, xvar, Idea_Reopen_State) {
  fit_panel_windows(Max_Day, Agg_Trips_1, time_window, xvar, Idea_Reopen_State)
}

# c(12, 6, 22, 13, 1, 17, 4, 47, 37, 45, 32, 51)
# c(1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40,45, 46, 47, 48, 49)
All_corr_ <- Split_State_SEM_Panel(Max_Day, Agg_Trips_1, 7,
                                   xvar = 'Lag7_Log_InFlow_Weight',
                                   Idea_Reopen_State = c(1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40, 45, 46, 47, 48, 49))
#All_corr_date_open <- zoo(All_corr_[[3]]$Estimate, All_corr_[[3]]$Date)
#All_corr_date_close <- zoo(All_corr_[[4]]$Estimate, All_corr_[[4]]$Date)
#plot(rollmean(All_corr_date_open, 7), col = 'blue', ylim = c(0.1, 0.35))
#lines(rollmean(All_corr_date_close, 7), col = 'red')

#str(All_corr_[[4]])
All_corr_perform <- do.call(rbind.data.frame, All_corr_[[8]])
mean(All_corr_perform[All_corr_perform$Response == 'Log_New_cases', 'R.squared'])
min(All_corr_perform[All_corr_perform$Response == 'Log_New_cases', 'R.squared'])
max(All_corr_perform[All_corr_perform$Response == 'Log_New_cases', 'R.squared'])

mean(All_corr_perform[All_corr_perform$Response == 'Lag7_Log_InFlow_Weight', 'R.squared'])
min(All_corr_perform[All_corr_perform$Response == 'Lag7_Log_InFlow_Weight', 'R.squared'])
max(All_corr_perform[All_corr_perform$Response == 'Lag7_Log_InFlow_Weight', 'R.squared'])


All_corr_[[3]]$Std.Error <- as.numeric(All_corr_[[3]]$Std.Error)
ggplot() +
  #geom_ribbon(data = All_corr_[[3]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), alpha = 0.2, colour = 'red') +
  geom_errorbar(data = All_corr_[[3]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.5) +
  geom_line(data = All_corr_[[3]], aes(x = Date, y = Estimate, colour = "Reopen"), size = 1) +
  geom_point(data = All_corr_[[3]], aes(x = Date, y = Estimate)) +
  #geom_ribbon(data = All_corr_[[4]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), alpha = 0.2, colour = 'green') +
  geom_errorbar(data = All_corr_[[4]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.5) +
  geom_line(data = All_corr_[[4]], aes(x = Date, y = Estimate, colour = "Lock-Down"), size = 1) +
  geom_point(data = All_corr_[[4]], aes(x = Date, y = Estimate)) +
  labs(x = "Date", y = "In-sample coefficient (±1 SE)") +
  theme_bw()

write.csv(All_corr_[[3]], 'Reopen.csv')
write.csv(All_corr_[[4]], 'Close.csv')

ggplot() +
  #geom_ribbon(data = All_corr_[[3]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), alpha = 0.2, colour = 'red') +
  geom_errorbar(data = All_corr_[[5]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.5) +
  geom_line(data = All_corr_[[5]], aes(x = Date, y = Estimate, colour = "Reopen"), size = 1) +
  geom_point(data = All_corr_[[5]], aes(x = Date, y = Estimate)) +
  #geom_ribbon(data = All_corr_[[4]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), alpha = 0.2, colour = 'green') +
  geom_errorbar(data = All_corr_[[6]], aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.5) +
  geom_line(data = All_corr_[[6]], aes(x = Date, y = Estimate, colour = "Lock-Down"), size = 1) +
  geom_point(data = All_corr_[[6]], aes(x = Date, y = Estimate)) +
  labs(x = "Date", y = "In-sample coefficient (±1 SE)") +
  theme_bw()

write.csv(All_corr_[[5]], 'Reopen_1.csv')
write.csv(All_corr_[[6]], 'Close_1.csv')

# Intercept
Inter_open <- All_corr_[[1]][All_corr_[[1]]$Response == 'Log_New_cases' &
                               All_corr_[[1]]$Predictor == '(Intercept)',] #All_corr_[[1]]$P.Value < 0.1
Inter_close <- All_corr_[[2]][All_corr_[[2]]$Response == 'Log_New_cases' &
                                All_corr_[[2]]$Predictor == '(Intercept)',]
ggplot() +
  geom_errorbar(data = Inter_open, aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.5) +
  geom_line(data = Inter_open, aes(x = Date, y = Estimate, colour = "Reopen"), size = 1) +
  geom_point(data = Inter_open, aes(x = Date, y = Estimate)) +
  geom_errorbar(data = Inter_close, aes(x = Date, y = Estimate, ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.5) +
  geom_line(data = Inter_close, aes(x = Date, y = Estimate, colour = "Lock-Down"), size = 1) +
  geom_point(data = Inter_close, aes(x = Date, y = Estimate)) +
  labs(x = "Date", y = "In-sample coefficient (±1 SE)") +
  theme_bw()

write.csv(Inter_open, 'Reopen_Inter_cases.csv')
write.csv(Inter_close, 'Close_Inter_cases.csv')

Inter_open <- All_corr_[[1]][All_corr_[[1]]$Response == 'Lag7_Log_InFlow_Weight' &
                               All_corr_[[1]]$Predictor == '(Intercept)',] #All_corr_[[1]]$P.Value < 0.1
Inter_close <- All_corr_[[2]][All_corr_[[2]]$Response == 'Lag7_Log_InFlow_Weight' &
                                All_corr_[[2]]$Predictor == '(Intercept)',]
write.csv(Inter_open, 'Reopen_Inter_flow.csv')
write.csv(Inter_close, 'Close_Inter_flow.csv')