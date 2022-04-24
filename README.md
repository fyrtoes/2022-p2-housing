# Local housing market analysis
* Extracted local real estate market data
* Interpolated missing time-series data to account for all span of time (1-day frequency)
* Missing data on the outer bounds was kept constant
* Calculated monthly payment from data provided
* Simplified visualization to a single axis by ploting time vs magnitude from original datapoint @ t=0
* Median home price, interest rate, and monthly payment are all relative values to initial datapoint

## Code and Resources Used
**R version:** 4.1.3
**Packages:** tidyverse, forecastML

## Effect of Quantitative Easing/Tightening on local housing market
![](/images/SA_housing_Final.png)
