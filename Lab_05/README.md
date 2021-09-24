Lab\_05
================
NickyNie
9/24/2021

``` r
library(data.table)
library(dplyr)
```

## Preprocessing data

``` r
# Download the data
met <- fread("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz")
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): 强制改变过程中产生了NA

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

## Merging data

``` r
met <- merge(
  x     = met,
  y     = stations,
  by.x  = "USAFID",
  by.y  = "USAF",
  all.x = TRUE,
  all.y = FALSE
)
```

# Question 1

``` r
station_avg <- met[,.(
  temp = mean(temp, na.rm = TRUE),
  wind.sp = mean(wind.sp, na.rm = TRUE),
  atm.press = mean(atm.press, na.rm = TRUE)
), by = USAFID]
```

identify the median per variable

``` r
station_avg[,.(
  temp_50 = quantile(temp, probs = 0.5, na.rm = TRUE),
  wind.sp_50 = quantile(wind.sp, probs = 0.5, na.rm = TRUE),
  atm.press_50 = quantile(atm.press, probs = 0.5, na.rm = TRUE)
) ]
```

    ##     temp_50 wind.sp_50 atm.press_50
    ## 1: 23.68406   2.461838     1014.691
