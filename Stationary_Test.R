# load the required libraries
library('zoo')
library('fpp')
library('forecast')

# Read DATA
Agg_Trips_1 <- read.csv('D:/COVID-19/PNAS_SECOND/All_XY_Features_To_R_County_Level_0731_toR.csv')
Agg_Trips_1 <- Agg_Trips_1[with(Agg_Trips_1, order(CTFIPS, Date)),]
Agg_Trips_1$Is_ReopenState <- TRUE
Agg_Trips_1$Date <- as.Date(Agg_Trips_1$Date)
Start_date <- as.Date('2020-03-10')
Max_Day <- as.numeric(difftime(max(Agg_Trips_1$Date), Start_date, units = 'days'))
time_window <- 7
Agg_Trips_1$Pct_Age_25_65 <- Agg_Trips_1$Pct_Age_25_40 + Agg_Trips_1$Pct_Age_40_65
Agg_Trips_1$National_Cases <- Agg_Trips_1$National_Cases / 1e3

# Dickey-Fuller (ADF) t-test Small p-values suggest that the data is stationary
AllCounty <- unique(Agg_Trips_1$CTFIPS)

# Entire stationary for nation
plot(Agg_Trips_1[(Agg_Trips_1$CTFIPS == 1003) & (Agg_Trips_1$Date >= Start_date), 'National_Cases'])
adf.test(Agg_Trips_1[(Agg_Trips_1$CTFIPS == 1003) & (Agg_Trips_1$Date >= Start_date), 'National_Cases'], alternative = 'stationary')


# Moving bandwidth, each county
ccount <- 1
County_P <- c()
for (jj in AllCounty) {
  #jj <- 1001
  print(jj)
  dat_Each <- Agg_Trips_1[Agg_Trips_1$CTFIPS == jj,]
  Start_date <- as.Date('2020-03-10')
  for (ii in (1:(Max_Day - 7))) {
    Agg_Trips_tem <- dat_Each[(dat_Each$Date <= Start_date + time_window) &
                                (dat_Each$Date > Start_date), 'Log_New_cases']
    Agg_Trips_tem <- na.omit(Agg_Trips_tem)
    if (length(Agg_Trips_tem) > 3) {
      tem <- adf.test(Agg_Trips_tem, alternative = 'stationary')
      if (!(is.na(tem$p.value))) { County_P[[ccount]] <- c(ii, jj, tem$p.value) }
      else { County_P[[ccount]] <- c(ii, jj, 0) }
    }
    else { County_P[[ccount]] <- c(ii, jj, 0) }
    ccount <- ccount + 1
    Start_date <- Start_date + 1
  }
}
County_P_Fianl <- data.frame(do.call(rbind, County_P))

summary(County_P_Fianl)
1 - nrow(County_P_Fianl[County_P_Fianl$X3 >= 0.05,]) / nrow(County_P_Fianl)

County_P_Fianl_95 <- County_P_Fianl[County_P_Fianl$X3 >= 0.05,]
TimeCount <- as.data.frame(table(County_P_Fianl_95[,1]))
CountyCount <- as.data.frame(table(County_P_Fianl_95[,2]))
TimeCount <- TimeCount[with(TimeCount, order(Freq)),]
CountyCount <- CountyCount[with(CountyCount, order(Freq)),]
barplot(table(County_P_Fianl_95[,1]))

## Entire stationary for each county
#AllCounty_P <- c()
#ccount <- 1
#Agg_Trips_1 <- Agg_Trips_1[Agg_Trips_1$Date >= as.Date('2020-03-10'),]
#for (jj in AllCounty) {
#  #jj <- 1001
#  dat_Each <- Agg_Trips_1[Agg_Trips_1$CTFIPS == jj,]
#  tem <- dat_Each[dat_Each$Date >= dat_Each[dat_Each$New_cases > 0, 'Date'][1], 'New_cases']
#  tem <- na.omit(tem)
#  if (length(tem) > 7) {
#    test_sta <- adf.test(tem, alternative = 'stationary')
#    AllCounty_P[[ccount]] <- test_sta$p.value
#    ccount <- ccount + 1 }
#}
#AllCounty_P_Fianl <- (do.call(rbind, AllCounty_P))
#summary(AllCounty_P_Fianl)
#length(AllCounty_P_Fianl[AllCounty_P_Fianl >= 0.05]) / length(AllCounty_P_Fianl)
#
## Moving bandwidth, nationwide
#Nation_p <- c()
#Start_date <- as.Date('2020-03-10')
#for (jj in (1:(Max_Day - 7))) {
#  print(jj)
#  Agg_Trips_tem <- Agg_Trips_1[(Agg_Trips_1$CTFIPS == 1003) &
#                                 (Agg_Trips_1$Date <= Start_date + time_window) &
#                                 (Agg_Trips_1$Date > Start_date),]
#
#  tem <- adf.test(Agg_Trips_tem[, 'Log_National_Cases'], alternative = 'stationary')
#  plot(Agg_Trips_tem[, 'Log_National_Cases'])
#  Nation_p[[jj]] <- tem$p.value
#  Start_date <- Start_date + 1
#}
#Nation_p_Fianl <- (do.call(rbind, Nation_p))
#summary(Nation_p_Fianl)
