import pandas as pd
import os
import datetime
import matplotlib.pyplot as plt
import numpy as np
from semopy import Model
import matplotlib.dates as mdates

from pathlib import Path
myFmt = mdates.DateFormatter('%b-%d')


def sem_function(mod, Agg_Trips_tem, Start_date):
    model = Model(mod)
    result = model.fit(Agg_Trips_tem)
    if not result.success:
        raise ValueError("SEM optimization failed: " + str(result.message))
    coeff_tem = model.inspect().rename(columns={'Estimate': 'Value', 'Std. Err': 'SE', 'p-value': 'P-value'})
    for col in ['Value', 'SE', 'P-value']:
        coeff_tem[col] = pd.to_numeric(coeff_tem[col], errors='coerce')
    coeff_tem['Date'] = Start_date
    return coeff_tem


def _moving_sem(data, mod, xvar, xvar_laglog, time_window, states=None, ntail=3000):
    if isinstance(time_window, bool) or int(time_window) != time_window or time_window < 1:
        raise ValueError("time_window must be a positive integer")
    data = data.copy().sort_values(['CTFIPS', 'Date']).reset_index(drop=True)
    if states is not None:
        # Preserve the declared top-county study population and disclose it.
        last = data[data.Date == data.Date.max()].sort_values('Agg_cases')
        data = data[data.CTFIPS.isin(last.tail(ntail).CTFIPS)].copy()
    start = pd.Timestamp('2020-03-10' if states is None else '2020-03-15')
    groups = ['National'] if states is None else ['Reopen', 'Close']
    frames = {group: [] for group in groups}
    logs = []
    for day in range(max(0, (data.Date.max() - start).days - int(time_window))):
        date = start + pd.Timedelta(days=day)
        window = data[(data.Date > date) & (data.Date <= date + pd.Timedelta(days=time_window))
                      & (data.New_cases > 1 / 1e3) & (data[xvar] > .01)]
        for group in groups:
            part = window
            if states is not None:
                opened = part.STFIPS.isin(states)
                part = part[opened if group == 'Reopen' else ~opened]
            complete = part.dropna().reset_index(drop=True)
            error = ''
            try:
                if complete.empty:
                    raise ValueError('No complete observations')
                coeff = sem_function(mod, complete, date)
                success = True
            except Exception as exc:
                error = str(exc)
                success = False
                coeff = pd.DataFrame([{'lval': 'Log_New_cases', 'rval': xvar_laglog,
                    'Value': np.nan, 'SE': np.nan, 'P-value': np.nan, 'Date': date}])
            coeff['Significant_0_1'] = pd.to_numeric(coeff['P-value'], errors='coerce') < .1
            frames[group].append(coeff)
            logs.append(dict(Date=date, Group=group, Eligible=len(part), Complete=len(complete),
                             Success=success, Error=error, Evaluation='in_sample'))
    pd.DataFrame(logs).to_csv(('National' if states is None else 'Split') + '_python_fit_status.csv', index=False)
    results = []
    _, ax = plt.subplots(figsize=(8, 5))
    for group in groups:
        full = pd.concat(frames[group], ignore_index=True) if frames[group] else pd.DataFrame(
            columns=['lval', 'rval', 'Date', 'Value', 'SE', 'P-value', 'Significant_0_1'])
        selected = full[(full.lval == 'Log_New_cases') & (full.rval == xvar_laglog)]
        ax.errorbar(selected.Date, selected.Value, selected.SE, fmt='-', label=group)
        full.to_csv(group + '_python_coefficients.csv', index=False)
        results.append(full)
    ax.legend(frameon=False)
    ax.set(xlabel='Date', ylabel='In-sample coefficient (±1 SE)')
    plt.tight_layout()
    return results


def sem_all_state(Agg_Trips_1, mod, xvar, xvar_laglog, time_window=7):
    return _moving_sem(Agg_Trips_1, mod, xvar, xvar_laglog, time_window)[0]


def sem_split_state(Agg_Trips_1, mod, xvar, Idea_Reopen_State, xvar_laglog, ntail=3000, time_window=7):
    return tuple(_moving_sem(Agg_Trips_1, mod, xvar, xvar_laglog, time_window,
                            states=Idea_Reopen_State, ntail=ntail))


# Read input data
Agg_Trips_1 = pd.read_csv(os.environ.get('PNAS_DATA_PATH', str(Path(__file__).resolve().parent / 'Data/All_XY_Features_To_R_County_Level_0731_toR.csv.gz')))
Agg_Trips_1['Date'] = pd.to_datetime(Agg_Trips_1['Date'])
Agg_Trips_1['CTFIPS'] = Agg_Trips_1['CTFIPS'].astype(str).apply(lambda x: x.zfill(5))

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
