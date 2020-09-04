import pandas as pd
import os
import datetime
import matplotlib.pyplot as plt
from pandas.tseries.holiday import USFederalHolidayCalendar
import numpy as np
import matplotlib.dates as mdates

os.chdir(r'D:\COVID-19\PNAS_SECOND')

# Read Cases
url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
Case_num = pd.read_csv(url, error_bad_lines=False, dtype={'FIPS': str}).rename(
    columns={'Confirmed': 'Confirmed_Cases', 'FIPS': 'CTFIPS'})
Case_num = Case_num.dropna(subset=['CTFIPS']).reset_index(drop=True)
Case_num = Case_num[Case_num['Country_Region'] == 'US']
Case_num = Case_num.dropna(subset=['CTFIPS']).reset_index(drop=True)
Case_num['CTFIPS'] = Case_num['CTFIPS'].astype(float).astype(int).astype(str).apply(lambda x: x.zfill(5))
Case_num = Case_num.drop(['UID', 'iso2', 'iso3', 'code3', 'Admin2', 'Province_State',
                          'Country_Region', 'Lat', 'Long_', 'Combined_Key'], axis=1)
Case_num = Case_num.melt(id_vars=['CTFIPS'], var_name='date')
Case_num['value'] = Case_num['value'].astype(int)
Case_num['date'] = pd.to_datetime(Case_num['date'])
Case_num = Case_num.sort_values(by=['CTFIPS', 'date']).reset_index(drop=True)
Case_num['New_cases'] = Case_num.groupby('CTFIPS')['value'].diff()
Case_num.loc[Case_num['New_cases'].isna(), 'New_cases'] = Case_num.loc[Case_num['New_cases'].isna(), 'value']
Case_num.rename({'value': 'Agg_cases', 'date': 'Date'}, axis=1, inplace=True)
# Some cases is negative, fill with 0
num = Case_num._get_numeric_data()
num[num < 0] = 0
del num

# Read OD
'''
data_range = list(pd.date_range('2020-01-01', '2020-06-13', freq='d').strftime("%m%d"))
All_OD = pd.DataFrame()
for date in data_range:
    print('___________' + date + '___________')
    tem = pd.read_pickle(r'D:\COVID-19\OD_RAW_1\OD_Count_' + date)
    All_OD = All_OD.append(tem)
All_OD.to_pickle('All_OD_0613')
'''
All_OD = pd.read_pickle(r'D:\COVID-19\All_OD_0613')

# Calculate Inflow trips
All_OD_Ex = All_OD[All_OD['CTFIPS_1'] != All_OD['CTFIPS_2']]
Agg_In = All_OD_Ex.groupby(['CTFIPS_2', 'Date']).sum().reset_index()
Agg_In.columns = ['CTFIPS', 'Date', 'In_Flow', 'InFlow_Weight']
# Internal trips
All_OD_In = All_OD[All_OD['CTFIPS_1'] == All_OD['CTFIPS_2']]
Agg_Inter = All_OD_In.groupby(['CTFIPS_1', 'Date']).sum().reset_index()
Agg_Inter.columns = ['CTFIPS', 'Date', 'Enter_Flow', 'Enter_Weight']
# Outer trips
Agg_Out = All_OD_Ex.groupby(['CTFIPS_1', 'Date']).sum().reset_index()
Agg_Out.columns = ['CTFIPS', 'Date', 'Out_Flow', 'OutFlow_Weight']
# Merge
Agg_In = Agg_In.merge(Agg_Inter, on=['CTFIPS', 'Date'], how='outer')
Agg_In = Agg_In.merge(Agg_Out, on=['CTFIPS', 'Date'], how='outer')
Agg_In = Agg_In.fillna(0)

# Merge with cases
Agg_Trips_1 = Agg_In.copy()
Agg_Trips_1['Date'] = pd.to_datetime(Agg_Trips_1['Date'])
Agg_Trips_1['CTFIPS'] = Agg_Trips_1['CTFIPS'].astype(str).apply(lambda x: x.zfill(5))
# Merge with cases
Agg_Trips_1 = Agg_Trips_1.merge(Case_num, on=['CTFIPS', 'Date'], how='outer')
Agg_Trips_1 = Agg_Trips_1[Agg_Trips_1['Date'] < datetime.datetime(2020, 6, 14)].reset_index(drop=True)
Agg_Trips_1 = Agg_Trips_1.sort_values(by=['CTFIPS', 'Date']).reset_index(drop=True)
Agg_Trips_1['STFIPS'] = Agg_Trips_1['CTFIPS'].str[0:2]


# Drop the outliers
# Agg_Trips_1['STFIPS'].value_counts()
def changeNY(ColName, Agg_Trips_1):
    Agg_Trips_1.loc[Agg_Trips_1['CTFIPS'] == '36061', ColName] = \
        list(Agg_Trips_1.loc[Agg_Trips_1['CTFIPS'] == '36061', ColName].reset_index(drop=True) + \
             Agg_Trips_1.loc[Agg_Trips_1['CTFIPS'] == '36005', ColName].reset_index(drop=True) + \
             Agg_Trips_1.loc[Agg_Trips_1['CTFIPS'] == '36047', ColName].reset_index(drop=True) + \
             Agg_Trips_1.loc[Agg_Trips_1['CTFIPS'] == '36081', ColName].reset_index(drop=True))
    return Agg_Trips_1


for columns in ['In_Flow', 'InFlow_Weight', 'Enter_Flow', 'Enter_Weight', 'Out_Flow', 'OutFlow_Weight']:
    Agg_Trips_1 = changeNY(columns, Agg_Trips_1)

Agg_Trips_1 = Agg_Trips_1[~Agg_Trips_1['CTFIPS'].isin(['36005', '36047', '36081'])].reset_index(drop=True)
Agg_Trips_1 = Agg_Trips_1[~Agg_Trips_1['STFIPS'].isin(['02', '15', 'Ou', '99', '88', '78', '00', '90', '80', '72'])]

# Agg_Trips_1.info()
Agg_Trips_1 = Agg_Trips_1.fillna(0)
Agg_Trips_1 = Agg_Trips_1.sort_values(by=['CTFIPS', 'Date']).reset_index(drop=True)
# Agg_Trips_1.to_pickle('Agg_Trips_Cases_0613')
Agg_Trips_1.groupby('Date').sum()['In_Flow'].plot()
plt.title('Total_Inflow')
Agg_Trips_1.groupby('Date').sum()['InFlow_Weight'].plot()
plt.title('Weight_Inflow')

# Read other data
Others = pd.read_csv(r'All_XY_Features_To_R_County_Level_0708_NoRef.csv', index_col=0)
Others['CTFIPS'] = Others['CTFIPS'].astype(str).apply(lambda x: x.zfill(5))  # zfill with 0
Others['Date'] = pd.to_datetime(Others['Date'])
Others = Others[['CTFIPS', 'CTNAME', 'social_distance', 'Pct_staying_home',
                 'Trips_person', 'Pct_out-of-county', 'Miles_person',
                 'Work_trips_person', 'Non-work_trips_person', 'Date', 'Month', 'Week',
                 'Adj_New_cases', 'Adj_Agg_Cases', 'Enforcement', 'Is_ReOpen', 'PRCP', 'TMAX', 'TMIN', 'Pct_Male',
                 'Pct_Age_0_24', 'Pct_Age_25_40', 'Pct_Age_40_65', 'Pct_White',
                 'Pct_Black', 'Pct_Indian', 'Pct_Asian', 'Pct_Unemploy', 'Total_Population', 'Med_House_Income', 'LAT',
                 'LNG', 'LAND', 'Is_Weekend', 'Time_Index', 'National_Cases', 'Population_density',
                 'Employment_density']]
Others1 = Others.merge(Agg_Trips_1, on=['CTFIPS', 'Date'], how='outer')
# RE-UNIT
for colu in ['National_Cases', 'Employment_density', 'Adj_New_cases', 'Adj_Agg_Cases', 'Total_Population',
             'Med_House_Income', 'Population_density', ]:
    Others1[colu] = Others1[colu] * 1e3
Others1['TMAX'] = Others1['TMAX'] * 0.1
Others1['TMIN'] = Others1['TMIN'] * 0.1
Others1['PRCP'] = Others1['PRCP'] * 0.1
Others1.describe().T

# Calculate based on some transfer
# Drop the holidays
cal = USFederalHolidayCalendar()
holidays = cal.holidays(start='2020-01-01', end='2020-01-31').to_pydatetime()
Others1 = Others1[~Others1['Date'].isin(holidays)].reset_index(drop=True)
All_Y_Ref = Others1[Others1['Month'] == 1].groupby(['CTFIPS', 'Week']).mean().reset_index()
All_Y_Ref.columns = [var + '_ref' for var in list(All_Y_Ref.columns)]
All_Y_Ref.rename({'CTFIPS_ref': 'CTFIPS', 'Week_ref': 'Week'}, axis=1, inplace=True)

# Calculate the flow reference
Others1 = Others1[Others1['Month'] != 1]
Others1 = Others1.merge(All_Y_Ref, on=['CTFIPS', 'Week'])

for each in ['In_Flow', 'InFlow_Weight', 'Enter_Flow', 'Enter_Weight', 'Out_Flow', 'OutFlow_Weight']:
    # All_Y[each] = (All_Y[each] - All_Y[each + '_ref']) / All_Y[each + '_ref']
    Others1.loc[Others1[each] == 0, each] = 1
    Others1[each + '_rate'] = Others1[each] / Others1[each + '_ref']
Others1.groupby(['Date']).mean()['In_Flow_rate'].plot()

# Calculate the case rate
# avg(3)/avg(7) and log
Others1 = Others1.sort_values(by=['CTFIPS', 'Date']).reset_index(drop=True)
Others1['Shift_sum_case_3'] = Others1.groupby(['CTFIPS'])['New_cases'].rolling(3).sum().reset_index()['New_cases']
Others1['Shift_sum_case_7'] = Others1.groupby(['CTFIPS'])['New_cases'].rolling(7).sum().reset_index()['New_cases']
Others1['New_cases_rate'] = np.log(Others1['Shift_sum_case_3'] / 3) / np.log(Others1['Shift_sum_case_7'] / 7)
# tem = Others1[['CTFIPS', 'New_cases', 'Shift_sum_case_7', 'Shift_sum_case_3', 'New_cases_rate']]
# Others1 = Others1.fillna(0)
Others1 = Others1[
    ['CTFIPS', 'CTNAME', 'social_distance', 'Pct_staying_home', 'Trips_person', 'Pct_out-of-county', 'Miles_person',
     'Work_trips_person', 'Non-work_trips_person', 'Date', 'Month', 'Week', 'Adj_New_cases', 'Adj_Agg_Cases',
     'Enforcement', 'Is_ReOpen', 'PRCP', 'TMAX', 'TMIN', 'Pct_Male', 'Pct_Age_0_24', 'Pct_Age_25_40', 'Pct_Age_40_65',
     'Pct_White', 'Pct_Black', 'Pct_Indian', 'Pct_Asian', 'Pct_Unemploy', 'Total_Population', 'Med_House_Income', 'LAT',
     'LNG', 'LAND', 'Is_Weekend', 'Time_Index', 'National_Cases', 'Population_density', 'Employment_density', 'In_Flow',
     'InFlow_Weight', 'Enter_Flow', 'Enter_Weight', 'Out_Flow', 'OutFlow_Weight', 'Agg_cases', 'New_cases', 'STFIPS',
     'In_Flow_rate', 'InFlow_Weight_rate', 'Enter_Flow_rate', 'Enter_Weight_rate', 'Out_Flow_rate',
     'OutFlow_Weight_rate', 'New_cases_rate']]

# Weighted trips by cases
All_OD_Ex['Date'] = pd.to_datetime(All_OD_Ex['Date'], format='%Y%m%d')
Columns_list = list(Case_num.columns)
Case_num.columns = [var + '_1' for var in Columns_list]
Case_num.rename({'Date_1': "Date"}, axis=1, inplace=True)
All_OD_Ex_cases = All_OD_Ex.merge(Case_num, on=['CTFIPS_1', 'Date'], how='left')
Case_num.columns = [var + '_2' for var in Columns_list]
Case_num.rename({'Date_2': "Date"}, axis=1, inplace=True)
All_OD_Ex_cases = All_OD_Ex_cases.merge(Case_num, on=['CTFIPS_2', 'Date'], how='left')
All_OD_Ex_cases.fillna(0, inplace=True)
All_OD_Ex_cases = All_OD_Ex_cases.sort_values(by=['CTFIPS_1', 'Date']).reset_index(drop=True)

CT_POP = Others1[['CTFIPS', 'Total_Population']]
CT_POP = CT_POP.drop_duplicates()
CT_POP.columns = ['CTFIPS_1', 'Population_1']
All_OD_Ex_cases = All_OD_Ex_cases.merge(CT_POP, on=['CTFIPS_1'])
CT_POP.columns = ['CTFIPS_2', 'Population_2']
All_OD_Ex_cases = All_OD_Ex_cases.merge(CT_POP, on=['CTFIPS_2'])
All_OD_Ex_cases['Risked_WInput'] \
    = (All_OD_Ex_cases['Agg_cases_1'] / (All_OD_Ex_cases['Population_1'])) * All_OD_Ex_cases['Weight']
Agg_Weight = All_OD_Ex_cases.groupby(['CTFIPS_2', 'Date']).sum()['Risked_WInput'].reset_index()
Agg_Weight.rename({'CTFIPS_2': 'CTFIPS'}, axis=1, inplace=True)

plt.plot(Agg_Weight['Risked_WInput'])

Agg_Weight = changeNY('Risked_WInput', Agg_Weight)
Agg_Weight = Agg_Weight[~Agg_Weight['CTFIPS'].isin(['36005', '36047', '36081'])].reset_index(drop=True)
Agg_Weight = Agg_Weight[
    ~Agg_Weight['CTFIPS'].str[0:2].isin(['02', '15', 'Ou', '99', '88', '78', '00', '90', '80', '72'])]

# All_OD_Ex_cases = All_OD_Ex_cases.sort_values(by=['CTFIPS_1', 'CTFIPS_2', 'Date']).reset_index(drop=True)
# len(Others1)/len(set(Others1['Date']))
# len(set(Agg_Weight['Date']))
Others1 = Others1.merge(Agg_Weight, on=['CTFIPS', 'Date'], how='left')
Others1['Risked_WInput'] = Others1['Risked_WInput'].fillna(0)

# Read State info
State_info = pd.read_csv(r'D:\COVID-19\Second_Paper_County_Stay_at_Home\state-geocodes-v2017.csv')
State_info['STFIPS'] = State_info['STFIPS'].astype(str).apply(lambda x: x.zfill(2))
State_info = State_info.drop_duplicates(subset=['STFIPS']).reset_index(drop=True)
Others1 = Others1.merge(State_info, on='STFIPS', how='left')

# PRCP should be relative
PRCP_RELT = Others1.groupby('CTFIPS').mean()['PRCP'].reset_index()
PRCP_RELT.columns = ['CTFIPS', 'REF_PRCP']
PRCP_RELT.loc[PRCP_RELT['REF_PRCP'] == 0, 'REF_PRCP'] = 0.1
Others1 = Others1.merge(PRCP_RELT, on='CTFIPS')
Others1['PRCP_NEW'] = Others1['PRCP'] / Agg_Trips_1['REF_PRCP']

Others1.to_csv('All_XY_Features_To_R_County_Level_0731.csv')

# Add lags/plot variables
os.chdir(r'D:\COVID-19\PNAS_SECOND')
# Read input data
Agg_Trips_1 = pd.read_csv('All_XY_Features_To_R_County_Level_0731.csv', index_col=0)
Agg_Trips_1['Date'] = pd.to_datetime(Agg_Trips_1['Date'])
Agg_Trips_1['CTFIPS'] = Agg_Trips_1['CTFIPS'].astype(str).apply(lambda x: x.zfill(5))

# Split by reopen
# 1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40,45, 46, 47, 48, 49
myFmt = mdates.DateFormatter('%b-%d')
Agg_Trips_1.columns
temp = Agg_Trips_1[['STFIPS', 'New_cases', 'Date']]
temp_reopen = temp[temp['STFIPS'].isin([1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40, 45, 46, 47, 48, 49])]
temp_close = temp[~temp['STFIPS'].isin([1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40, 45, 46, 47, 48, 49])]
sum_temp_reopen = temp_reopen.groupby(['Date']).sum()['New_cases'].reset_index()
sum_temp_close = temp_close.groupby(['Date']).sum()['New_cases'].reset_index()
sum_temp_reopen['Date'] = pd.to_datetime(sum_temp_reopen['Date'])
sum_temp_close['Date'] = pd.to_datetime(sum_temp_close['Date'])
sum_temp_reopen.columns = ['Date', 'National_Cases_Reopen']
sum_temp_close.columns = ['Date', 'National_Cases_Close']

Agg_Trips_1 = Agg_Trips_1.merge(sum_temp_reopen, on='Date', how='left')
Agg_Trips_1 = Agg_Trips_1.merge(sum_temp_close, on='Date', how='left')
# Agg_Trips_1.groupby(['Date']).sum()[['National_Cases_Reopen', 'National_Cases_Close']].plot()

# Lag and Log
time_window = 7
time_lag = 7
# Log
for colu in ['National_Cases', 'National_Cases_Reopen', 'National_Cases_Close', 'New_cases', 'InFlow_Weight',
             'Risked_WInput', 'Enter_Weight', 'OutFlow_Weight']:
    Agg_Trips_1['Log_' + colu] = np.log(Agg_Trips_1[colu])
# Lag1
for colu in ['National_Cases', 'National_Cases_Reopen', 'National_Cases_Close', 'New_cases', 'InFlow_Weight',
             'Risked_WInput', 'Enter_Weight', 'OutFlow_Weight']:
    Agg_Trips_1['Lag1_Log_' + colu] = Agg_Trips_1.groupby(['CTFIPS']).shift(1)['Log_' + colu]
# Lag7
for colu in ['National_Cases', 'National_Cases_Reopen', 'National_Cases_Close', 'New_cases', 'InFlow_Weight',
             'Risked_WInput', 'Enter_Weight', 'OutFlow_Weight']:
    Agg_Trips_1['Lag7_Log_' + colu] = Agg_Trips_1.groupby(['CTFIPS']).shift(7)['Log_' + colu]
Agg_Trips_1 = Agg_Trips_1.replace([np.inf, -np.inf], np.nan)
Agg_Trips_1.columns

Agg_Trips_1.to_csv('All_XY_Features_To_R_County_Level_0731_toR.csv')

# Plot cases and flow
# How many counties
Agg_Trips_1 = pd.read_csv('All_XY_Features_To_R_County_Level_0731_toR.csv')
temp_county = Agg_Trips_1[['STFIPS', 'CTFIPS', 'New_cases', 'Date']]
print(len(set(
    temp_county[temp_county['STFIPS'].isin([1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40, 45, 46, 47, 48, 49])][
        'CTFIPS'])))
print(len(set(
    temp_county[~temp_county['STFIPS'].isin([1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40, 45, 46, 47, 48, 49])][
        'CTFIPS'])))

temp = Agg_Trips_1[['STFIPS', 'New_cases', 'InFlow_Weight', 'Date']]
temp_reopen = temp[temp['STFIPS'].isin([1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40, 45, 46, 47, 48, 49])]
temp_close = temp[~temp['STFIPS'].isin([1, 4, 8, 13, 16, 17, 18, 19, 23, 27, 28, 35, 38, 40, 45, 46, 47, 48, 49])]
sum_temp_reopen = temp_reopen.groupby(['Date']).sum()[['New_cases', 'InFlow_Weight']].reset_index()
sum_temp_close = temp_close.groupby(['Date']).sum()[['New_cases', 'InFlow_Weight']].reset_index()
sum_temp_reopen['Date'] = pd.to_datetime(sum_temp_reopen['Date'])
sum_temp_close['Date'] = pd.to_datetime(sum_temp_close['Date'])

myFmt = mdates.DateFormatter('%b-%d')
fig, ax = plt.subplots(ncols=1, nrows=1, figsize=(8, 5))
ln1 = ax.plot(sum_temp_reopen['Date'], sum_temp_reopen['InFlow_Weight'] / 1480, '-o', color='#10375c', markersize=3,
              alpha=1)
ln2 = ax.plot(sum_temp_close['Date'], sum_temp_close['InFlow_Weight'] / 1625, '->', color='#b83b5e', markersize=3,
              alpha=1)
plt.legend(['Inflow ("Reopened" counties as of 05/01)', 'Inflow ("Locked-down" counties)'], frameon=False,
           loc=(.45, .86))
ax.xaxis.set_major_formatter(myFmt)
ax.ticklabel_format(axis="y", style="sci", scilimits=(0, 0), useMathText=True)
ax2 = ax.twinx()
ln3 = ax2.bar(sum_temp_reopen['Date'], sum_temp_reopen['New_cases'] / 1480, color='#10375c', alpha=0.2)
ln4 = ax2.bar(sum_temp_close['Date'], sum_temp_close['New_cases'] / 1625, bottom=sum_temp_reopen['New_cases'] / 1480,
              color='#b83b5e', alpha=0.2)
ax.set_xlim([datetime.date(2020, 2, 1), datetime.date(2020, 6, 10)])
ax.xaxis.set_major_locator(mdates.WeekdayLocator(interval=3))
# fig.autofmt_xdate()
plt.legend(['Cases ("Reopened" counties as of 05/01)', 'Cases ("Locked-down" counties)'], frameon=False, loc=(.45, .75))
ax2.set_ylabel('Number of new cases per county (Daily)')
ax.set_ylabel('Number of inflow per county (Daily)')
ax.set_xlabel('Date')
ax.set_ylim([0.2 * 1e5, 1.1 * 1e5])
ax2.set_ylim([0, 30])
plt.tight_layout()
plt.savefig('Figure 2a.pdf', dpi=1000)
plt.savefig('Figure 2a.png', dpi=1000)

# Output
Agg_Trips_1 = pd.read_csv('All_XY_Features_To_R_County_Level_0731_toR.csv', index_col=0)
Agg_Trips_1.columns
Agg_Trips_1[['CTFIPS', 'CTNAME', 'Date', 'Month', 'Week',
             'Adj_New_cases', 'Adj_Agg_Cases', 'Enforcement', 'Is_ReOpen', 'PRCP',
             'TMAX', 'TMIN', 'Pct_Male', 'Pct_Age_0_24', 'Pct_Age_25_40',
             'Pct_Age_40_65', 'Pct_White', 'Pct_Black', 'Pct_Indian', 'Pct_Asian',
             'Total_Population', 'Med_House_Income', 'LAT', 'LNG',
             'LAND', 'Is_Weekend', 'Time_Index', 'National_Cases',
             'Population_density', 'Employment_density', 'In_Flow', 'InFlow_Weight',
             'Agg_cases', 'New_cases', 'STFIPS', 'Risked_WInput', 'STNAME',
             'PRCP_NEW', 'National_Cases_Reopen', 'National_Cases_Close',
             'Log_National_Cases', 'Log_National_Cases_Reopen',
             'Log_National_Cases_Close', 'Log_New_cases', 'Log_InFlow_Weight',
             'Log_Risked_WInput',
             'Lag1_Log_National_Cases', 'Lag1_Log_National_Cases_Reopen',
             'Lag1_Log_National_Cases_Close', 'Lag1_Log_New_cases',
             'Lag1_Log_InFlow_Weight', 'Lag1_Log_Risked_WInput',
             'Lag7_Log_National_Cases', 'Lag7_Log_National_Cases_Reopen',
             'Lag7_Log_National_Cases_Close', 'Lag7_Log_New_cases',
             'Lag7_Log_InFlow_Weight', 'Lag7_Log_Risked_WInput']].to_csv(
    'All_XY_Features_To_R_County_Level_0731_toR.csv.gz', compression='gzip', index=False)
