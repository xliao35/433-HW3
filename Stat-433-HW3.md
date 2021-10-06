424 HW3
================
Xucen Liao
2021/10/4

``` r
rm(list = ls())
library(nycflights13)
```

    ## Warning: package 'nycflights13' was built under R version 3.6.2

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.6.2

    ## ─ Attaching packages ──────────────────── tidyverse 1.3.1 ─

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 3.6.2

    ## Warning: package 'tibble' was built under R version 3.6.2

    ## Warning: package 'tidyr' was built under R version 3.6.2

    ## Warning: package 'readr' was built under R version 3.6.2

    ## Warning: package 'purrr' was built under R version 3.6.2

    ## Warning: package 'dplyr' was built under R version 3.6.2

    ## Warning: package 'forcats' was built under R version 3.6.2

    ## ─ Conflicts ───────────────────── tidyverse_conflicts() ─
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 3.6.2

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
flights
```

    ## # A tibble: 336,776 × 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # … with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
weather
```

    ## # A tibble: 26,115 × 15
    ##    origin  year month   day  hour  temp  dewp humid wind_dir wind_speed
    ##    <chr>  <int> <int> <int> <int> <dbl> <dbl> <dbl>    <dbl>      <dbl>
    ##  1 EWR     2013     1     1     1  39.0  26.1  59.4      270      10.4 
    ##  2 EWR     2013     1     1     2  39.0  27.0  61.6      250       8.06
    ##  3 EWR     2013     1     1     3  39.0  28.0  64.4      240      11.5 
    ##  4 EWR     2013     1     1     4  39.9  28.0  62.2      250      12.7 
    ##  5 EWR     2013     1     1     5  39.0  28.0  64.4      260      12.7 
    ##  6 EWR     2013     1     1     6  37.9  28.0  67.2      240      11.5 
    ##  7 EWR     2013     1     1     7  39.0  28.0  64.4      240      15.0 
    ##  8 EWR     2013     1     1     8  39.9  28.0  62.2      250      10.4 
    ##  9 EWR     2013     1     1     9  39.9  28.0  62.2      260      15.0 
    ## 10 EWR     2013     1     1    10  41    28.0  59.6      260      13.8 
    ## # … with 26,105 more rows, and 5 more variables: wind_gust <dbl>, precip <dbl>,
    ## #   pressure <dbl>, visib <dbl>, time_hour <dttm>

``` r
flights %>% 
  group_by(hour) %>% 
  summarise(dep_delay = mean(dep_delay, na.rm = T)) %>% 
  #arrange(dep_delay) %>% 
  ggplot(aes(hour, dep_delay)) + geom_col()
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

![](Stat-433-HW3_files/figure-gfm/unnamed-chunk-1-1.png)<!-- --> From
the graph, we can see that if we choose flights which departure at 5 am,
we can effectively avoid delay. However, even we choose to departure at
5 am, there are also some factors that can affect the probability of
being late : quarters, wind speed, and origins.

**Quarter Impact**

``` r
flights %>% 
 filter(hour == 5, between(month, 1,3)) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = T))  #1st quarter
```

    ## # A tibble: 1 × 1
    ##   dep_delay
    ##       <dbl>
    ## 1      1.74

``` r
flights %>% 
 filter(hour == 5, between(month, 4,6)) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = T))  #2nd quarter
```

    ## # A tibble: 1 × 1
    ##   dep_delay
    ##       <dbl>
    ## 1    -0.529

``` r
flights %>% 
 filter(hour == 5, between(month, 7,9)) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = T))  #3rd quarter
```

    ## # A tibble: 1 × 1
    ##   dep_delay
    ##       <dbl>
    ## 1     0.465

``` r
flights %>% 
 filter(hour == 5, between(month, 10,12)) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = T))  #4th quarter
```

    ## # A tibble: 1 × 1
    ##   dep_delay
    ##       <dbl>
    ## 1      1.14

``` r
#From these code, we can know that, for the flights scheduled departure between 5 am and 6am, the mean time of delay for the four quarters are (1.74, -0.53, 0.47, 1.14) 

barplot(c(1.74, -0.53, 0.47, 1.14), names.arg = c("Q1", "Q2", "Q3", "Q4"), ylab = "mean time of delay")
```

![](Stat-433-HW3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- --> From
the graph, we can find that in the first quarter, the mean delay time is
the most among the whole year, and the Q4 is the second one. The
possible reason could be that in this two quarter, the weather is
relatively bad and cold, which may bring some effect to the time of
on-time departure.

**Wind Speed impact**

``` r
flights %>% 
  left_join(y = weather, by = c("origin","time_hour")) %>%
  filter(hour.x == 5) %>%
  group_by(wind_speed) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = T)) %>%
  ggplot(aes(wind_speed, dep_delay)) + geom_col()
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

![](Stat-433-HW3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> From
this graph, we can see that wind speed can influence the delay time at 5
am a lot. When the wind speed is about 22 mph, the mean delay time is
the most.

**Origin Impact**

``` r
flights %>% 
  filter(hour == 5) %>%
  group_by(origin) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = T)) %>%
  ggplot(aes(origin, dep_delay)) + geom_col()
```

![](Stat-433-HW3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> From
this graph, we can know that LGA has the most mean delay time at 5 am.
This is reasonable because LGA is at New York, which is a really busy
city. As a result, its air might be also busy, so it is easy for this
city to have departure delay even at 5 am.
