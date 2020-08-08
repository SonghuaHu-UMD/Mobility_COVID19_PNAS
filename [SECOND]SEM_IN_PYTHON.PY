import pandas as pd
import os
import datetime
import matplotlib.pyplot as plt
import numpy as np
from semopy import Model
from semopy import Optimizer
from semopy.inspector import inspect
import matplotlib.dates as mdates

os.chdir(r'D:\COVID-19\PNAS_SECOND')
myFmt = mdates.DateFormatter('%b-%d')


def sem_function(mod, Agg_Trips_tem, Start_date):
    model = Model(mod)
    model.load_dataset(Agg_Trips_tem)
    opt = Optimizer(model)
    objective_function_value = opt.optimize()
    coeff_tem = inspect(opt)
    coeff_tem['Date'] = Start_date
    return coeff_tem


def sem_all_state(Agg_Trips_1, mod, xvar, xvar_laglog):
    Agg_Trips_1['Is_ReopenState'] = True  # If all states
    # Moving-SEM
    Agg_Trips_1 = Agg_Trips_1.sort_values(by=['CTFIPS', 'Date']).reset_index(drop=True)
    Start_date = datetime.datetime(2020, 3, 10)
    All_corr_Reopen = pd.DataFrame()
    All_corr_Close = pd.DataFrame()
    for jj in range(0, (max(Agg_Trips_1['Date']) - Start_date).days - 7):
        print(jj)
        Agg_Trips_tem = \
            Agg_Trips_1[(Agg_Trips_1['Date'] <= Start_date + datetime.timedelta(time_window)) &
                        (Agg_Trips_1['Date'] > Start_date) & (Agg_Trips_1['New_cases'] > 1 / 1e3)
                        & (Agg_Trips_1[xvar] > 0.01)]
        Agg_Trips_tem = Agg_Trips_tem.dropna().reset_index(drop=True)
        Reopen_tem = Agg_Trips_tem[Agg_Trips_tem['Is_ReopenState']]
        try:
            coeff_tem0 = sem_function(mod, Reopen_tem, Start_date)
            coeff_tem0['Date'] = Start_date
            All_corr_Reopen = All_corr_Reopen.append(coeff_tem0)
        except:
            pass
        Start_date = Start_date + datetime.timedelta(1)

    All_corr_1_Reopen = All_corr_Reopen[
        (All_corr_Reopen['lval'] == 'Log_New_cases') & (All_corr_Reopen['rval'] == xvar_laglog) & (
                All_corr_Reopen['Date'] > datetime.datetime(2020, 3, 10)) & (All_corr_Reopen['P-value'] < 0.1)]
    fig, ax = plt.subplots(ncols=1, nrows=1, figsize=(8, 5))
    ax.errorbar(All_corr_1_Reopen['Date'], All_corr_1_Reopen['Value'], All_corr_1_Reopen['SE'], fmt='-',
                color='#10375c', capsize=1, markersize=0, alpha=0.8)
    plt.ylabel('Coefficient (moving average)')
    plt.xlabel('Date')
    plt.tight_layout()
    return All_corr_Reopen


def sem_split_state(Agg_Trips_1, mod, xvar, Idea_Reopen_State, xvar_laglog, ntail=3000):
    # PICK TOP CASES
    MAX_CT = Agg_Trips_1[Agg_Trips_1['Date'] == max(Agg_Trips_1['Date'])][['CTFIPS', 'Agg_cases']]
    MAX_CT = MAX_CT.sort_values(by='Agg_cases').reset_index(drop=True)
    Agg_Trips_11 = Agg_Trips_1[Agg_Trips_1['CTFIPS'].isin(MAX_CT.tail(ntail)['CTFIPS'])]
    # SPLIT THE STATE
    Agg_Trips_11['Is_ReopenState'] = Agg_Trips_11['STFIPS'].isin(Idea_Reopen_State)
    # Moving-SEM
    Agg_Trips_11 = Agg_Trips_11.sort_values(by=['CTFIPS', 'Date']).reset_index(drop=True)
    Start_date = datetime.datetime(2020, 3, 15)
    All_corr_Reopen = pd.DataFrame()
    All_corr_Close = pd.DataFrame()
    for jj in range(0, (max(Agg_Trips_11['Date']) - Start_date).days - 7):
        print(jj)
        Agg_Trips_tem = \
            Agg_Trips_11[(Agg_Trips_11['Date'] <= Start_date + datetime.timedelta(time_window)) &
                         (Agg_Trips_11['Date'] > Start_date) & (Agg_Trips_11['New_cases'] > 1 / 1e3)
                         & (Agg_Trips_11[xvar] > 0.01)]
        Agg_Trips_tem = Agg_Trips_tem.dropna().reset_index(drop=True)
        Reopen_tem = Agg_Trips_tem[Agg_Trips_tem['Is_ReopenState']]
        Close_tem = Agg_Trips_tem[~Agg_Trips_tem['Is_ReopenState']]
        try:
            coeff_tem0 = sem_function(mod, Reopen_tem, Start_date)
            coeff_tem1 = sem_function(mod, Close_tem, Start_date)
            coeff_tem0['Date'] = Start_date
            All_corr_Reopen = All_corr_Reopen.append(coeff_tem0)
            coeff_tem1['Date'] = Start_date
            All_corr_Close = All_corr_Close.append(coeff_tem1)
        except:
            pass
        Start_date = Start_date + datetime.timedelta(1)

    All_corr_1_Reopen = All_corr_Reopen[
        (All_corr_Reopen['lval'] == 'Log_New_cases') & (All_corr_Reopen['rval'] == xvar_laglog) & (
                All_corr_Reopen['Date'] > datetime.datetime(2020, 3, 10)) & (All_corr_Reopen['P-value'] < 0.1)]
    All_corr_1_Close = All_corr_Close[
        (All_corr_Close['lval'] == 'Log_New_cases') & (All_corr_Close['rval'] == xvar_laglog) & (
                All_corr_Close['Date'] > datetime.datetime(2020, 3, 14)) & (All_corr_Close['P-value'] < 0.1)]
    fig, ax = plt.subplots(ncols=1, nrows=1, figsize=(8, 5))
    ax.errorbar(All_corr_1_Reopen['Date'], All_corr_1_Reopen['Value'], All_corr_1_Reopen['SE'], fmt='-',
                color='#10375c', capsize=1, markersize=0, alpha=0.8)
    ax.errorbar(All_corr_1_Close['Date'], All_corr_1_Close['Value'], All_corr_1_Close['SE'], fmt='-', color='#b83b5e',
                capsize=1, markersize=0, alpha=0.8)
    plt.legend(['"Reopened" counties', '"Locked-down" counties'], frameon=False)
    plt.ylabel('Coefficient (moving average)')
    plt.xlabel('Date')
    plt.tight_layout()
    return All_corr_Reopen, All_corr_Close


# Read input data
Agg_Trips_1 = pd.read_csv('All_XY_Features_To_R_County_Level_0731_toR.csv', index_col=0)
Agg_Trips_1['Date'] = pd.to_datetime(Agg_Trips_1['Date'])
Agg_Trips_1['CTFIPS'] = Agg_Trips_1['CTFIPS'].astype(str).apply(lambda x: x.zfill(5))

# # Split by reopen
# # 1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40,45, 46, 47, 48, 49
# Agg_Trips_1.columns
# temp = Agg_Trips_1[['STFIPS', 'New_cases', 'Date']]
# temp_reopen = temp[temp['STFIPS'].isin([1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40, 45, 46, 47, 48, 49])]
# temp_close = temp[~temp['STFIPS'].isin([1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40, 45, 46, 47, 48, 49])]
# sum_temp_reopen = temp_reopen.groupby(['Date']).sum()['New_cases'].reset_index()
# sum_temp_close = temp_close.groupby(['Date']).sum()['New_cases'].reset_index()
# sum_temp_reopen['Date'] = pd.to_datetime(sum_temp_reopen['Date'])
# sum_temp_close['Date'] = pd.to_datetime(sum_temp_close['Date'])
# plt.plot(sum_temp_reopen['Date'], sum_temp_reopen['New_cases'])
# plt.plot(sum_temp_close['Date'], sum_temp_close['New_cases'])
# sum_temp_reopen.columns = ['Date', 'National_Cases_Reopen']
# sum_temp_close.columns = ['Date', 'National_Cases_Close']
#
# Agg_Trips_1 = Agg_Trips_1.merge(sum_temp_reopen, on='Date', how='left')
# Agg_Trips_1 = Agg_Trips_1.merge(sum_temp_close, on='Date', how='left')
# # Agg_Trips_1.groupby(['Date']).sum()[['National_Cases_Reopen', 'National_Cases_Close']].plot()
#
# # Lag and Log
# time_window = 7
# time_lag = 7
# # Log
# for colu in ['National_Cases', 'National_Cases_Reopen', 'National_Cases_Close', 'New_cases', 'InFlow_Weight',
#              'Risked_WInput', 'Enter_Weight', 'OutFlow_Weight']:
#     Agg_Trips_1['Log_' + colu] = np.log(Agg_Trips_1[colu])
# # Lag1
# for colu in ['National_Cases', 'National_Cases_Reopen', 'National_Cases_Close', 'New_cases', 'InFlow_Weight',
#              'Risked_WInput', 'Enter_Weight', 'OutFlow_Weight']:
#     Agg_Trips_1['Lag1_Log_' + colu] = Agg_Trips_1.groupby(['CTFIPS']).shift(1)['Log_' + colu]
# # Lag7
# for colu in ['National_Cases', 'National_Cases_Reopen', 'National_Cases_Close', 'New_cases', 'InFlow_Weight',
#              'Risked_WInput', 'Enter_Weight', 'OutFlow_Weight']:
#     Agg_Trips_1['Lag7_Log_' + colu] = Agg_Trips_1.groupby(['CTFIPS']).shift(7)['Log_' + colu]
# Agg_Trips_1 = Agg_Trips_1.replace([np.inf, -np.inf], np.nan)
# Agg_Trips_1.columns
#
# Agg_Trips_1.to_csv('All_XY_Features_To_R_County_Level_0731_toR.csv')

# ALL STATE, Log_InFlow_Weight
All_corr_Reopen = sem_all_state(Agg_Trips_1, """ Log_New_cases ~ Log_InFlow_Weight + Lag1_Log_New_cases + Is_Weekend  + Population_density  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65
        Log_InFlow_Weight ~  Log_National_Cases + Lag1_Log_InFlow_Weight + Is_Weekend + Population_density + Employment_density + PRCP + TMAX  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 +  Med_House_Income  + Pct_Black + Pct_White
        """, 'InFlow_Weight', 'Log_InFlow_Weight')
# ALL STATE, RISK_INFLOW
All_corr_Reopen = sem_all_state(Agg_Trips_1, """ Log_New_cases ~ Log_Risked_WInput + Lag1_Log_New_cases + Is_Weekend  + Population_density  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65
        Log_Risked_WInput ~  Log_National_Cases + Lag1_Log_Risked_WInput + Is_Weekend + Population_density + Employment_density + PRCP_NEW + TMAX  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 +  Med_House_Income  + Pct_Black + Pct_White
        """, 'Risked_WInput', 'Log_Risked_WInput')
# ALL STATE, LAG7_RISK_INFLOW
All_corr_Reopen = sem_all_state(Agg_Trips_1, """ Log_New_cases ~ Lag7_Log_Risked_WInput + Lag1_Log_New_cases + Is_Weekend  + Population_density  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65
        Log_Risked_WInput ~  Log_National_Cases + Lag1_Log_Risked_WInput + Is_Weekend + Population_density + Employment_density + PRCP_NEW + TMAX  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 +  Med_House_Income  + Pct_Black + Pct_White
        """, 'Risked_WInput', 'Lag7_Log_Risked_WInput')

# SPLIT STATE, Log_InFlow_Weight
# ['51', '37', '27', '49', '04', '48', '12', '28', '01', '06', '19']
# ['01', '04', '08', '13', '16', '17', '18', '19', '23', '27', '28', '35', '38', '40', '45', '46', '47', '48', '49']
# ['12', '06', '22', '13', '01', '17', '04', '47', '37', '45', '32', '51']
All_corr_Reopen, All_corr_Close = sem_split_state(Agg_Trips_1, """ Log_New_cases ~ Log_InFlow_Weight + Lag1_Log_New_cases + Is_Weekend  + Population_density  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65
        Log_InFlow_Weight ~  Log_National_Cases + Lag1_Log_InFlow_Weight + Is_Weekend + Population_density + Employment_density + PRCP + TMAX  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 +  Med_House_Income  + Pct_Black + Pct_White
        """, 'InFlow_Weight', ['12', '06', '22', '13', '01', '17', '04', '47', '37', '45', '32', '51'],
                                                  'Log_InFlow_Weight')
# SPLIT STATE, RISK_INFLOW
All_corr_Reopen, All_corr_Close = sem_split_state(Agg_Trips_1, """ Log_New_cases ~ Log_Risked_WInput + Lag1_Log_New_cases + Is_Weekend  + Population_density  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65
        Log_Risked_WInput ~  Log_National_Cases + Lag1_Log_Risked_WInput + Is_Weekend + Population_density + Employment_density + PRCP_NEW + TMAX  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 +  Med_House_Income  + Pct_Black + Pct_White
        """, 'Risked_WInput', ['51', '37', '27', '49', '04', '48', '12', '28', '01', '06', '19'], 'Log_Risked_WInput')
# SPLIT STATE, LAG7_RISK_INFLOW
All_corr_Reopen, All_corr_Close = sem_split_state(Agg_Trips_1, """ Log_New_cases ~ Lag7_Log_Risked_WInput + Lag1_Log_New_cases + Is_Weekend  + Population_density  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65
        Log_Risked_WInput ~  Log_National_Cases + Lag1_Log_Risked_WInput + Is_Weekend + Population_density + Employment_density + PRCP_NEW + TMAX  + Pct_Age_0_24 + Pct_Age_25_40 + Pct_Age_40_65 +  Med_House_Income  + Pct_Black + Pct_White
        """, 'Risked_WInput', ['51', '37', '27', '49', '04', '48', '12', '28', '01', '06', '19'],
                                                  'Lag7_Log_Risked_WInput')
# Other factors
All_corr_Reopen[All_corr_Reopen['P-value'] < 0.1].groupby(['lval', 'op', 'rval']).describe()['Value'][
    ['count', 'mean', '50%']]
