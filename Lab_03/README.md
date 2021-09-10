---
title: "Lab-03"
author: "NickyNie"
date: "9/10/2021"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(leaflet)
library(lubridate)
```

## Load dataset
```{r}
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "met_all.gz", method="libcurl", timeout = 60)
met <- data.table::fread("met_all.gz")
```

## Dimension
Hence there are 2377343 rows and 30 columns in this dataset
```{r}
dim(met)
```

## Check for the number of rows and coluumns
Number of rows 
```{r}
nrow(met)
```
Number of columns
```{r}
ncol(met)
```

## Headers and Footers
```{r}
head(met)
```

```{r}
tail(met)
```

## Variables
```{r}
str(met)
```
## Key variables
```{r}
table(met$year)
```

```{r}
table(met$day)
```

```{r}
table(met$month)
```

```{r}
summary(met$temp)
```

```{r}
summary(met$elev)
```

```{r}
summary(met$wind.sp)
```

Replace missing data with NA
```{r}
NA -> met[met$elev==9999.0]
summary(met$elev)
```

Remove temperatures that are lower than -40`C
```{r}
met <- met[temp>-40]
met2 <- met[order(temp)]
head(met2)
```

## Calculate summary statistics
Select the weather station with maximum elevation
```{r}
elev <- met[elev==max(elev)]
summary(elev)
```

Correlation between temperature and wind speed
```{r}
cor(elev$temp, elev$wind.sp, use="complete")
```

Correlation between temperature and wind speed with hour and day of the month
```{r}
cor(elev$temp, elev$day, use="complete")
```


```{r}
cor(elev$temp, elev$hour, use="complete")
```

```{r}
cor(elev$wind.sp, elev$day, use="complete")
```

```{r}
cor(elev$wind.sp, elev$hour, use="complete")
```

## Graph
Histogram
```{r}
hist(met$elev, breaks=100)
```

```{r}
hist(met$temp)
```

```{r}
hist(met$wind.sp)
```

The location of the weather station with highest elevation
```{r}
leaflet(elev) %>%
  addProviderTiles('OpenStreetMap') %>% 
  addCircles(lat=~lat,lng=~lon, opacity=1, fillOpacity=1, radius=100)
```
Look at the time series of temperature and wind speed at this location.
```{r}
elev$date <- with(elev, ymd_h(paste(year, month, day, hour, sep= ' ')))
summary(elev$date)
```

```{r}
elev <- elev[order(date)]
head(elev)
```
Plot the time series of temperature and wind speed
```{r}
plot(elev$date, elev$temp, type='l')
```
```{r}
plot(elev$date, elev$wind.sp, type='l')
```

Before 8.18, the general trend is increasing, but decreases rapidly decreases around 8.18 or 8.19. After that, it recovers until 8.25 and then drops again.
