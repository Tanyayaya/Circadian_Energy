# Circadian energy

## Software requirement
All analysis were implemented in R4.1.2. 
R packages used in this study are available from CRAN (https://cran.r-project.org/web/packages/available_packages_by_name.html), including:
- **Rssa**
- **lubridate**

##  Data requirement
###  Individual accelerometer  data
Individual accelerometer records should be saved in a csv file, with two columns, i.e., time and activity. In the **simulation_data** folder, we provided 7 simulated samples of accelerometer data. 

## Running analysis
### Preprocess accelerometer data
**preprocess_accelerometer_data.R**: Rscript to preprocess individual accelerometer records into a list of activity data frames;
### Calculate circadian energy
**circadian_amplitude_calculation.R**: Rscript to calculate circadian energy and relative amplitude based on preprocessed accelerometer data list; circadian energy and nonparametric results are saved in the **results** folder.
