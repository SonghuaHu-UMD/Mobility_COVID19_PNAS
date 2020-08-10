# This script plots the related coeff. figures
import pandas as pd
import os
import datetime
import matplotlib.pyplot as plt

os.chdir(r'C:\Users\huson\PycharmProjects\Mobility_COVID19')

All_corr_0 = pd.read_csv(r'Reopen.csv', index_col=0)
All_corr_1 = pd.read_csv(r'Close.csv', index_col=0)
All_corr_2 = pd.read_csv(r'National.csv', index_col=0)
All_corr_0['Date'] = pd.to_datetime(All_corr_0['Date'])
All_corr_1['Date'] = pd.to_datetime(All_corr_1['Date'])
All_corr_2['Date'] = pd.to_datetime(All_corr_2['Date'])
All_corr_0 = All_corr_0.sort_values(by='Date').reset_index(drop=True)
All_corr_1 = All_corr_1.sort_values(by='Date').reset_index(drop=True)
All_corr_2 = All_corr_2.sort_values(by='Date').reset_index(drop=True)
All_corr_0 = All_corr_0[All_corr_0['Date'] < datetime.datetime(2020, 6, 10)]
All_corr_1 = All_corr_1[All_corr_1['Date'] < datetime.datetime(2020, 6, 10)]
All_corr_2 = All_corr_2[All_corr_2['Date'] < datetime.datetime(2020, 6, 10)]

fig, ax = plt.subplots(ncols=1, nrows=1, figsize=(8, 5))
ax.plot(All_corr_0['Date'], All_corr_0['Estimate'], '-o', color='#10375c', markersize=3, alpha=1)
ax.plot(All_corr_1['Date'], All_corr_1['Estimate'], '->', color='#b83b5e', markersize=3, alpha=1)
ax.plot(All_corr_2['Date'], All_corr_2['Estimate'], '--', color='k', markersize=3, alpha=0.5)
ax.fill_between(All_corr_0['Date'], All_corr_0['Estimate'] - All_corr_0['Std.Error'],
                All_corr_0['Estimate'] + All_corr_0['Std.Error'],
                facecolor='#10375c', alpha=0.15)
ax.fill_between(All_corr_1['Date'], All_corr_1['Estimate'] - All_corr_1['Std.Error'],
                All_corr_1['Estimate'] + All_corr_1['Std.Error'], facecolor='#b83b5e', alpha=0.15)
# ax.fill_between(All_corr_2['Date'], All_corr_2['Estimate'] - All_corr_2['Std.Error'],
#                 All_corr_2['Estimate'] + All_corr_2['Std.Error'], facecolor='k', alpha=0.1)
ax.errorbar(All_corr_2['Date'], All_corr_2['Estimate'], All_corr_2['Std.Error'], fmt='--', color='k',
            capsize=0.2, markersize=0, alpha=0.5)

plt.legend(
    ['"Reopened" counties as of 05/01', '"Locked-down" counties', 'Nationwide'], frameon=False)
plt.ylabel('Coefficient (moving average)')
plt.xlabel('Date')
ax.plot([datetime.datetime(2020, 3, 13), datetime.datetime(2020, 3, 13)], [0.05, 0.35], '--', color='royalblue')
ax.annotate('National \nEmergency', xy=(datetime.datetime(2020, 3, 13), 0.05),
            xytext=(datetime.datetime(2020, 3, 13) + datetime.timedelta(days=1), 0.05), color='royalblue')

ax.plot([datetime.datetime(2020, 4, 1), datetime.datetime(2020, 4, 1)], [0.05, 0.35], '--', color='peru')
ax.annotate('33 States \nlocked down', xy=(datetime.datetime(2020, 4, 1), 0.1),
            xytext=(datetime.datetime(2020, 4, 1) + datetime.timedelta(days=1), 0.1), color='peru')

ax.plot([datetime.datetime(2020, 4, 16), datetime.datetime(2020, 4, 16)], [0.05, 0.35], '--', color='seagreen')
ax.annotate('Reopen \nGuideline', xy=(datetime.datetime(2020, 4, 16), 0.05),
            xytext=(datetime.datetime(2020, 4, 16) + datetime.timedelta(days=1), 0.05), color='seagreen')

ax.plot([datetime.datetime(2020, 5, 1), datetime.datetime(2020, 5, 1)], [0.05, 0.35], '--', color='firebrick')
ax.annotate('19 States \nreopened', xy=(datetime.datetime(2020, 5, 1), 0.1),
            xytext=(datetime.datetime(2020, 5, 1) + datetime.timedelta(days=1), 0.1), color='firebrick')
# ax.set_ylim([0, 0.83])
plt.tight_layout()
plt.savefig('INFLOW_COEF.pdf', dpi=1000)

# # Plot another
# All_corr_0 = pd.read_csv(r'Reopen_1.csv', index_col=0)
# All_corr_1 = pd.read_csv(r'Close_1.csv', index_col=0)
# All_corr_2 = pd.read_csv(r'National_1.csv', index_col=0)
#
# All_corr_0['Date'] = pd.to_datetime(All_corr_0['Date'])
# All_corr_1['Date'] = pd.to_datetime(All_corr_1['Date'])
# All_corr_2['Date'] = pd.to_datetime(All_corr_2['Date'])
# All_corr_0 = All_corr_0.sort_values(by='Date').reset_index(drop=True)
# All_corr_1 = All_corr_1.sort_values(by='Date').reset_index(drop=True)
# All_corr_2 = All_corr_2.sort_values(by='Date').reset_index(drop=True)
# All_corr_0 = All_corr_0[All_corr_0['Date'] < datetime.datetime(2020, 6, 1)]
# All_corr_1 = All_corr_1[All_corr_1['Date'] < datetime.datetime(2020, 6, 1)]
# All_corr_2 = All_corr_2[All_corr_2['Date'] < datetime.datetime(2020, 6, 1)]
#
# fig, ax = plt.subplots(ncols=1, nrows=1, figsize=(8, 5))
# ax.plot(All_corr_0['Date'], All_corr_0['Estimate'], '-o', color='#10375c', markersize=5, alpha=0.7)
# ax.plot(All_corr_1['Date'], All_corr_1['Estimate'], '->', color='#b83b5e', markersize=5, alpha=0.7)
# ax.plot(All_corr_2['Date'], All_corr_2['Estimate'], '-', color='k', markersize=5, alpha=0.7)
# ax.fill_between(All_corr_0['Date'], All_corr_0['Estimate'] - All_corr_0['Std.Error'],
#                 All_corr_0['Estimate'] + All_corr_0['Std.Error'],
#                 facecolor='#10375c', alpha=0.2)
# ax.fill_between(All_corr_1['Date'], All_corr_1['Estimate'] - All_corr_1['Std.Error'],
#                 All_corr_1['Estimate'] + All_corr_1['Std.Error'], facecolor='#b83b5e', alpha=0.2)
# ax.fill_between(All_corr_2['Date'], All_corr_2['Estimate'] - All_corr_2['Std.Error'],
#                 All_corr_2['Estimate'] + All_corr_2['Std.Error'], facecolor='k', alpha=0.2)
# plt.legend(
#     ['"Reopened" counties as of 05/01', '"Locked-down" counties', 'Nationwide'], frameon=False)
# plt.ylabel('Coefficient (moving average)')
# plt.xlabel('Date')
#
# ax.plot([datetime.datetime(2020, 3, 13), datetime.datetime(2020, 3, 13)], [-2.5, 0.5], '--', color='royalblue')
# ax.annotate('National \nEmergency', xy=(datetime.datetime(2020, 3, 13), -2.5),
#             xytext=(datetime.datetime(2020, 3, 13) + datetime.timedelta(days=1), -2.5), color='royalblue')
#
# ax.plot([datetime.datetime(2020, 4, 1), datetime.datetime(2020, 4, 1)], [-2.5, 0.5], '--', color='peru')
# ax.annotate('33 States \nlocked down', xy=(datetime.datetime(2020, 4, 1), -2.4),
#             xytext=(datetime.datetime(2020, 4, 1) + datetime.timedelta(days=1), -2.4), color='peru')
#
# ax.plot([datetime.datetime(2020, 4, 16), datetime.datetime(2020, 4, 16)], [-2.5, 0.5], '--', color='seagreen')
# ax.annotate('Reopen \nGuideline', xy=(datetime.datetime(2020, 4, 16), -2),
#             xytext=(datetime.datetime(2020, 4, 16) + datetime.timedelta(days=1), -2), color='seagreen')
#
# ax.plot([datetime.datetime(2020, 5, 1), datetime.datetime(2020, 5, 1)], [-2.5, 0.5], '--', color='firebrick')
# ax.annotate('19 States \nreopened', xy=(datetime.datetime(2020, 5, 1), -2),
#             xytext=(datetime.datetime(2020, 5, 1) + datetime.timedelta(days=1), -2), color='firebrick')
# # ax.set_ylim([0, 0.83])
# plt.tight_layout()
# plt.savefig('Cases_COEF.png', dpi=1000)

# PLOT INTERCEPT
os.chdir(r'C:\Users\huson\PycharmProjects\Mobility_COVID19')

All_corr_0 = pd.read_csv(r'Reopen_Inter_cases.csv', index_col=0)
All_corr_1 = pd.read_csv(r'Close_Inter_cases.csv', index_col=0)
All_corr_2 = pd.read_csv(r'National_interc_cases.csv', index_col=0)
All_corr_0['Date'] = pd.to_datetime(All_corr_0['Date'])
All_corr_1['Date'] = pd.to_datetime(All_corr_1['Date'])
All_corr_2['Date'] = pd.to_datetime(All_corr_2['Date'])
All_corr_0 = All_corr_0.sort_values(by='Date').reset_index(drop=True)
All_corr_1 = All_corr_1.sort_values(by='Date').reset_index(drop=True)
All_corr_2 = All_corr_2.sort_values(by='Date').reset_index(drop=True)
All_corr_0 = All_corr_0[All_corr_0['Date'] < datetime.datetime(2020, 6, 10)]
All_corr_1 = All_corr_1[All_corr_1['Date'] < datetime.datetime(2020, 6, 10)]
All_corr_2 = All_corr_2[All_corr_2['Date'] < datetime.datetime(2020, 6, 10)]

fig, ax = plt.subplots(ncols=1, nrows=1, figsize=(8, 5))
ax.plot(All_corr_0['Date'], All_corr_0['Estimate'], '-o', color='#10375c', markersize=3, alpha=1)
ax.plot(All_corr_1['Date'], All_corr_1['Estimate'], '->', color='#b83b5e', markersize=3, alpha=1)
ax.plot(All_corr_2['Date'], All_corr_2['Estimate'], '--', color='k', markersize=3, alpha=0.5)
ax.fill_between(All_corr_0['Date'], All_corr_0['Estimate'] - All_corr_0['Std.Error'],
                All_corr_0['Estimate'] + All_corr_0['Std.Error'],
                facecolor='#10375c', alpha=0.15)
ax.fill_between(All_corr_1['Date'], All_corr_1['Estimate'] - All_corr_1['Std.Error'],
                All_corr_1['Estimate'] + All_corr_1['Std.Error'], facecolor='#b83b5e', alpha=0.15)
# ax.fill_between(All_corr_2['Date'], All_corr_2['Estimate'] - All_corr_2['Std.Error'],
#                 All_corr_2['Estimate'] + All_corr_2['Std.Error'], facecolor='k', alpha=0.1)
ax.errorbar(All_corr_2['Date'], All_corr_2['Estimate'], All_corr_2['Std.Error'], fmt='--', color='k',
            capsize=0.2, markersize=0, alpha=0.5)

plt.legend(
    ['"Reopened" counties as of 05/01', '"Locked-down" counties', 'Nationwide'], frameon=False)
plt.ylabel('Coefficient (moving average)')
plt.xlabel('Date')
ax.plot([datetime.datetime(2020, 3, 13), datetime.datetime(2020, 3, 13)], [-5, 0], '--', color='royalblue')
ax.annotate('National \nEmergency', xy=(datetime.datetime(2020, 3, 13), -4.8),
            xytext=(datetime.datetime(2020, 3, 13) + datetime.timedelta(days=1), -4.8), color='royalblue')

ax.plot([datetime.datetime(2020, 4, 1), datetime.datetime(2020, 4, 1)], [-5, 0], '--', color='peru')
ax.annotate('33 States \nlocked down', xy=(datetime.datetime(2020, 4, 1), -4.5),
            xytext=(datetime.datetime(2020, 4, 1) + datetime.timedelta(days=1), -4.5), color='peru')

ax.plot([datetime.datetime(2020, 4, 16), datetime.datetime(2020, 4, 16)], [-5, 0], '--', color='seagreen')
ax.annotate('Reopen \nGuideline', xy=(datetime.datetime(2020, 4, 16), -4.8),
            xytext=(datetime.datetime(2020, 4, 16) + datetime.timedelta(days=1), -4.8), color='seagreen')

ax.plot([datetime.datetime(2020, 5, 1), datetime.datetime(2020, 5, 1)], [-5, 0], '--', color='firebrick')
ax.annotate('19 States \nreopened', xy=(datetime.datetime(2020, 5, 1), -4.5),
            xytext=(datetime.datetime(2020, 5, 1) + datetime.timedelta(days=1), -4.5), color='firebrick')
ax.set_xlim([datetime.date(2020, 3, 12), datetime.date(2020, 6, 10)])
ax.set_ylim([-5, 1])
plt.tight_layout()
plt.savefig('INTERCEPT_CASES.png', dpi=1000)

# PLOT INTERCEPT
os.chdir(r'C:\Users\huson\PycharmProjects\Mobility_COVID19')

All_corr_0 = pd.read_csv(r'Reopen_Inter_flow.csv', index_col=0)
All_corr_1 = pd.read_csv(r'Close_Inter_flow.csv', index_col=0)
All_corr_2 = pd.read_csv(r'National_interc_flow.csv', index_col=0)
All_corr_0['Date'] = pd.to_datetime(All_corr_0['Date'])
All_corr_1['Date'] = pd.to_datetime(All_corr_1['Date'])
All_corr_2['Date'] = pd.to_datetime(All_corr_2['Date'])
All_corr_0 = All_corr_0.sort_values(by='Date').reset_index(drop=True)
All_corr_1 = All_corr_1.sort_values(by='Date').reset_index(drop=True)
All_corr_2 = All_corr_2.sort_values(by='Date').reset_index(drop=True)
All_corr_0 = All_corr_0[All_corr_0['Date'] < datetime.datetime(2020, 6, 10)]
All_corr_1 = All_corr_1[All_corr_1['Date'] < datetime.datetime(2020, 6, 10)]
All_corr_2 = All_corr_2[All_corr_2['Date'] < datetime.datetime(2020, 6, 10)]

fig, ax = plt.subplots(ncols=1, nrows=1, figsize=(8, 5))
ax.plot(All_corr_0['Date'], All_corr_0['Estimate'], '-o', color='#10375c', markersize=3, alpha=1)
ax.plot(All_corr_1['Date'], All_corr_1['Estimate'], '->', color='#b83b5e', markersize=3, alpha=1)
ax.plot(All_corr_2['Date'], All_corr_2['Estimate'], '--', color='k', markersize=3, alpha=0.5)
ax.fill_between(All_corr_0['Date'], All_corr_0['Estimate'] - All_corr_0['Std.Error'],
                All_corr_0['Estimate'] + All_corr_0['Std.Error'],
                facecolor='#10375c', alpha=0.15)
ax.fill_between(All_corr_1['Date'], All_corr_1['Estimate'] - All_corr_1['Std.Error'],
                All_corr_1['Estimate'] + All_corr_1['Std.Error'], facecolor='#b83b5e', alpha=0.15)
# ax.fill_between(All_corr_2['Date'], All_corr_2['Estimate'] - All_corr_2['Std.Error'],
#                 All_corr_2['Estimate'] + All_corr_2['Std.Error'], facecolor='k', alpha=0.1)
ax.errorbar(All_corr_2['Date'], All_corr_2['Estimate'], All_corr_2['Std.Error'], fmt='--', color='k',
            capsize=0.2, markersize=0, alpha=0.5)

plt.legend(
    ['"Reopened" counties as of 05/01', '"Locked-down" counties', 'Nationwide'], frameon=False)
plt.ylabel('Coefficient (moving average)')
plt.xlabel('Date')
ax.plot([datetime.datetime(2020, 3, 13), datetime.datetime(2020, 3, 13)], [-5, 0], '--', color='royalblue')
ax.annotate('National \nEmergency', xy=(datetime.datetime(2020, 3, 13), -4.8),
            xytext=(datetime.datetime(2020, 3, 13) + datetime.timedelta(days=1), -4.8), color='royalblue')

ax.plot([datetime.datetime(2020, 4, 1), datetime.datetime(2020, 4, 1)], [-5, 0], '--', color='peru')
ax.annotate('33 States \nlocked down', xy=(datetime.datetime(2020, 4, 1), -4.5),
            xytext=(datetime.datetime(2020, 4, 1) + datetime.timedelta(days=1), -4.5), color='peru')

ax.plot([datetime.datetime(2020, 4, 16), datetime.datetime(2020, 4, 16)], [-5, 0], '--', color='seagreen')
ax.annotate('Reopen \nGuideline', xy=(datetime.datetime(2020, 4, 16), -4.8),
            xytext=(datetime.datetime(2020, 4, 16) + datetime.timedelta(days=1), -4.8), color='seagreen')

ax.plot([datetime.datetime(2020, 5, 1), datetime.datetime(2020, 5, 1)], [-5, 0], '--', color='firebrick')
ax.annotate('19 States \nreopened', xy=(datetime.datetime(2020, 5, 1), -4.5),
            xytext=(datetime.datetime(2020, 5, 1) + datetime.timedelta(days=1), -4.5), color='firebrick')
ax.set_xlim([datetime.date(2020, 3, 12), datetime.date(2020, 6, 10)])
ax.set_ylim([-5, 1])
plt.tight_layout()
plt.savefig('INTERCEPT_FLOW.png', dpi=1000)
