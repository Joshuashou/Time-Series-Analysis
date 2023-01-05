#Final Project TDD


```{r}
#install.packages("tseries")
library(tseries)
library("reshape2") 
library(zoo)
library(ggplot2)
library(astsa)

#Justify Jones & Bradley (1992a), Benner (1999) and Vaidyanathan (2016)?
temp_data <- read.csv(file = 'Desktop/Temperature.csv')

colnames(temp_data) <- c('Year', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', 'Annual')

annual_temp_data = temp_data['Annual']

temp_data <- subset(temp_data, select = -c(Annual) )  #Drop the Annual results




```


```{r}
#Create Plot of October Data


#plot(c(1:364), temp_data['Oct'])
#temp_ts = ts(temp_data)#, frequency=12)
temp_long = melt(temp_data, id.vars = "Year")


temp_long = temp_long[order(temp_long$Year), ]

temp_long$Date <- as.yearmon(paste(temp_long$Year, temp_long$variable), "%Y %m")

ggplot(head(temp_long, 100), 
       aes(x=Date,
           y=value,
           )) +
  geom_line() + ggtitle("Monthly Time Series Temperature Plots")

temp_long
```


```{r}
  #We want to look at the ACF and Periodgram of fluctuations of time changes within the Climate. 

Total_temp_ts <- ts(temp_long['value'], start=c(1659, 1), end=c(2022, 10), frequency = 12)

annual_ts <- ts(annual_temp_data, start = 1659, end = 2022)

temp_acf <- acf2(Total_temp_ts, max.lag = 30)

#PeriodGram to see frequency 

adf.test(Total_temp_ts, k=0)

pp.test(Total_temp_ts)

adf.test(Total_temp_ts)

```


```{r}
  #We see that total time series gives stationary results, we will try to truncate the time series. 

recent_total_temp = window(annual_ts, start = 2000, end = 2021)

ts.plot(recent_total_temp)

adf.test(recent_total_temp, k=0)

pp.test(recent_total_temp)

adf.test(recent_total_temp)

```


