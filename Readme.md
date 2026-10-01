# Mobile device data reveals the dynamics in a positive relationship between human mobility and COVID-19 infections
### Chenfeng Xiong, Songhua Hu, Mofeng Yang, Weiyu Luo, and Lei Zhang
Accurately estimating human mobility and gauging its relationship with virus transmission is critical for the control of COVID-19 spreading. Using mobile device location data of over 100 million monthly active samples, we compute origin-destination travel demand and aggregate mobility inflow at each U.S. county from Mar 01 to May 08, 2020. Then, we quantify the change of mobility inflow across the nation and statistically model the time-varying relationship between inflow and the infections. We find that external travel to other counties has decreased by 35% soon after the nation entered the emergency situation, but recovered rapidly during the partial-reopening phase. Moreover, our simultaneous equations analysis highlights the dynamics in a positive relationship between mobility inflow and the number of infections during the COVID-19 onset. This relationship is found increasingly stronger in partially reopened regions. Our study provides a quick reference and timely data availability for researchers and decision-makers to understand the national mobility trends before and during the pandemic. The modeling results can be used to predict mobility and transmissions risks and integrated with epidemics models to further assess the public health outcomes.

## Data
* Feature matrix used to build the model is available at:
https://github.com/SonghuaHu-UMD/Mobility_COVID19_PNAS/tree/master/Data
* Other mobility data can be found at:
https://data.covid.umd.edu/
* COVID-19 cases data can be download from:
https://coronavirus.jhu.edu/us-map

## Code
* 1-Data_Prepare.py: Finish the data preprocessing.
* 2-SEM_Fit.R: Build the dynamic SEM model in R.
* 3-Plot.py: Visualize the results.
* Optimal_Lag.R: Find the optimal lag for SEM models.
* Prediction_Compare.R: Compare the model performance with/without mobility features, also commpare the prediction performance of different models.
* SEM_IN_PYTHON.py: A similar SEM model fit processing in Python.
* SEM_PANEL.R: Build a mixed-effect panel model with SEM structure.
* Stationary_Test.R: Test the time-series stationary.

## Methodology
To capture the time-varying relationship between the number of infection and mobility inflow, we have developed a simultaneous equations model with time-varying coefficients. The main results are reported in the main manuscripts, while the details are reported in the supplementary, including:
* Details of mobility metrics
* Number of New cases and Inflow varying in reopen states 
* Optimal Lag
* Model performance 
* Model Interpretation
* Other methods: BSTS 
* Other variables: the risked inflow

