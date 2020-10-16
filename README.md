# interpolationApp
Linearly interpolate data at 1Hz and manually remove cycles.

## Disclaimer
This application is designed load and read files cleaned and outputted from <bold> Silo - Interactive Signal Averaging App </bold>. Using files that have not gone through Silo may require additional formatting to be read by this interpolation app.

# interpDataCreate
## Data loading
Data must be in a folder titled "rawData". 
Ventilatory data file names must begin with "breath" (i.e. breath_study01), cardiovascular data file names must begin with "beat (i.e. beat_study01). There must be both a breath and a beat file for each participant. The application will otherwise crash.

## File Navigation Panel
### Choose File
This panel allows you to browse and select the data files that are in the "rawData" folder. 

### File Start Time
Allows you to trim the file by change the start time of the file.

### File End Time
Allows you to trim the file by change the end time of the file.

### Start of Protocol
This will change the "0 time point" of the file. Typically the onset of a stimulus.

## "Plots" Tab
This tab allows you to visualize what ventilatory and cardiovascular data you have in your selected file and ensure data is adequately cleaned. Please note: you can not clean data in this app. All cleaning must be done in Silo.

## "Select Variables" Tab
Here you can choose which variables from the original file will be included in the interpolated data file. You are also able to choose how long each "cycle" or "bout" is in seconds.  

## "Quality Check" Tab
This allows you to visualize interpolated data for selected variables. This is designed to ensure the interpolation worked properly. Hovering the cursor over a the data will return the cycle number. Once happy, click "Export Data". Interpolated data will be saved in "cleanData". <bold>Please Note</bold>: this folder must be created manually prior to exporting data.

In R-Studio, hit "stop" in order to terminate the interpCreate.R

# Cleaning Interpolated Data by Cycle or Bout Number (interpSelect.R).
 "interpSelect.R" must be launched manually by hitting "Run App"
 
 Much of the interpCreate.R interface functions similarly to interpSelect.R <bold>However, interpCreate.R loads the interpolated data from cleanData</bold>.
 
 ## "Interpolated Plots" Tab
 ### Variable
 Allows you to select which variable to view and clean
 
 ### First Bout
 Select the first/lowest bout or cycle number (<italics> not the time </italics>) you'd like to be included in the plot. 
 
 ### Last Bout
 Select the last/highest bout or cycle number (<italics> not the time </italics>) you'd like to be included in the plot. 
 
 ### Plot
 You can hover over each line of data to see the cycle number associated with it.
 An individual bout can be removed by clicking on the desired bout (i.e an outlier). Bouts are removed programatically by bout number, therefore you can adjust the
 number of bouts being displayed on the plot as needed.
 bTime that is returned refers to the time along the x-axis, not the actual "Time" point in the file.
 
 ### Reset Cleared Bouts
 Resets data to include all bouts. Useful if you accidentally delete a bout.
 
 ### Export Cleaned Data
 Exports cleaned interpolated data to folder "appData". Note: appData must be created manually prior to exporting cleaned data.
 <bold>Please Note: Whatever bouts are visualized in the plot when you select "Export Clean Data" will be written to the cleaned data file. </bold>
 
 ### Download Report
 Downloads IH paradigm sepecific report to get basic metrics of an IH exposure (i.e. time spent below 95% SpO2, average desaturation). Includes plot for quality control.  
 
 When finished hit "stop" in R-Studio to terminate interpSelect.R
