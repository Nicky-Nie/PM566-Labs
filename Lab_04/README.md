---
output: 
  html_document: 
    keep_md: yes
editor_options: 
  chunk_output_type: inline
---
---
title: "Lab_04"
author: "NickyNie"
date: "9/17/2021"
output: html_document
---
```{r packages}
library(data.table)
library(tidyverse)
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Read Data
```{r data.read,cache=TRUE}
if (!file.exists("met_all.gz"))
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "met_all.gz",
    method = "libcurl",
    timeout = 60
  )
met <- data.table::fread("met_all.gz")
```
## Prepare data
```{r data_prep}
#Remove temperatures less than -17C
met <- met[temp >= -17]

#Make sure there are no missing data in the key variables coded as 9999, 999, etc
#temp, rh, wind.sp, vis.dist, dew.point, lat, lon, and elev
met[, range(temp)]
met[, range(rh,na.rm = TRUE)]
met[, range(wind.sp,na.rm = TRUE)]
met[, range(vis.dist,na.rm = TRUE)]
met[, range(dew.point,na.rm = TRUE)]
met[, range(lat,na.rm = TRUE)]
met[, range(lon,na.rm = TRUE)]
met[, range(elev,na.rm = TRUE)]
met[elev == 9999, elev := NA]

#Generate a date variable using the functions as.Date() (hint: You will need the following to create a date paste(year, month, day, sep = "-")).
met[, ymd := as.Date(paste(year, month, day, sep = "-"))]

#Using the data.table::week function, keep the observations of the first week of the month.
met[,table(week(ymd))]
met <- met[week(ymd) == 31]

#Compute the mean by station of the variables temp, rh, wind.sp, vis.dist, dew.point, lat, lon, and elev.
met_avg <- met[, .(
  temp      = mean(temp, na.rm =TRUE),
  rh        = mean(rh, na.rm =TRUE),
  wind.sp   = mean(wind.sp, na.rm =TRUE),
  vis.dist  = mean(vis.dist, na.rm =TRUE),
  dew.point = mean(dew.point, na.rm =TRUE),
  lat       = mean(lat, na.rm =TRUE),
  lon       = mean(lon, na.rm =TRUE),
  elev      = mean(elev, na.rm =TRUE), USAFID
), by = "USAFID"]

#Create a region variable for NW, SW, NE, SE based on lon = -98.00 and lat = 39.71 degrees
met_avg[lat >= 39.71 & lon <= -98, region:="Northwest"]
met_avg[lat <39.71 & lon <= -98, region:="Southwest"]
met_avg[lat >= 39.71 & lon > -98, region:="Northeast"]
met_avg[lat < 39.71 & lon > -98,region:="Southeast"]
met_avg[, table(region, useNA = "always")]

met_avg[, region2 := fifelse(lat >= 39.71 & lon <= -98, "Northwest",
        fifelse(lat < 39.71  & lon <= -98, "Southwest",
                fifelse(lat >= 39.71 & lon > -98, "Northeast",
                        fifelse(lat < 39.71  & lon > -98, "Southeast", NA_character_))))]

met_avg[, table(region, region2, useNA = "always")]

#Create a categorical variable for elevation as in the lecture slides
met_avg[, elev_cat := fifelse(elev > 252, "high", "low")]

#Deleting extra column
met_avg <- met_avg[, -1]
```

## Use geom_violin to examine the wind speed and dew point temperature by region
```{r violin_temp}
ggplot(met_avg, mapping = aes(y = wind.sp, x = 1)) + 
  geom_violin() +
  facet_grid(~region)
```
There seems to be an outlier for data in Northeast. Generally the order of wind speed in these four regions (from fastest to slowest): Southwest > Northwest > Northeast = Southeast

```{r}
ggplot(met_avg, mapping = aes(y = dew.point, x = 1)) + 
  geom_violin() +
  facet_grid(~region)
```
The data of dew point temperature is more concentrated in Southeast and more variated in Northwest and Southwest.

## Use geom_jitter with stat_smooth to examine the association between dew point temperature and wind speed by region
```{r geom_jitter}
ggplot( 
  met_avg[!is.na(wind.sp) & !is.na(dew.point)], 
  mapping = aes(y = wind.sp, x = dew.point, color = region, linetype = region)) +
  geom_jitter() +
  stat_smooth(method = lm, col = "black")
```
The association between dew point temperature and wind speed is positive in Northwest, Southeast and Northeast but negative in Southwest. And wind speed is more influenced by dew point temperature in Southeast.

## Use geom_bar to create barplots of the weather stations by elevation category coloured by region
```{r barplot}
ggplot( 
  met_avg[!is.na(elev_cat)], 
  mapping = aes( x = elev_cat, fill = region, position="dodge")) +
  geom_bar() + 
  scale_fill_brewer(palette = "RdPu") +
  labs(title = "Weather stations by elevation category coloured by region")
```
In the category of high elevation, the count is similar between Northeast and Southwest, Northwest and Southeast. But in low elevation category, the count is extremely high for Southeast and extremely low for Northwest.

## Use stat_summary to examine mean dew point and wind speed by region with standard deviation error bars

```{r plot_stat_summ}
ggplot(
  met_avg[!is.na(wind.sp) & !is.na(dew.point)], 
  mapping = aes(x=region, y = wind.sp)) +
  stat_summary(fun.data="mean_sdl") +
  stat_summary(fun.data="mean_sdl", geom="errorbar")
```
The mean wind speed of Northeast and Southeast is around 1.8, of Northwest is around 2.9 and of Southwest is around 3.0. 

```{r}
ggplot(
  met_avg[!is.na(wind.sp) & !is.na(dew.point)], 
  mapping = aes(x=region, y = dew.point)) +
  stat_summary(fun.data="mean_sdl") +
  stat_summary(fun.data="mean_sdl", geom="errorbar")
```
The mean dew point temperature of Northeast is around 16, od Northwest is around 12.5, of Southeast is around 21 and of Southwest is around 13.

## Make a map showing the spatial trend in relative h in the US
```{r leaflet}
library(leaflet)

temp.pal <- colorNumeric(c('darkgreen','goldenrod','brown'), domain=met_avg$rh)

leaflet(met_avg) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(
    lat = ~lat, lng=~lon,
                                                  # HERE IS OUR PAL!
    label = ~paste0(round(rh,2), ' rh'), color = ~ temp.pal(rh),
    opacity = 1, fillOpacity = 1, radius = 500
    ) %>%
  # And a pretty legend
  addLegend('bottomleft', pal=temp.pal, values=met_avg$rh,
          title='Temperature, C', opacity=1)
```
## Use a ggplot extension
```{r ggforce}
library(ggforce)
ggplot(met_avg, aes(wind.sp, dew.point, colour = region)) +
  geom_point() +
  facet_zoom(y = region == "Southeast")
```

