p8105_hw3_ar4459_practice
================
ASHLEY ROMO
2023-10-05

## Problem 1

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)
data("instacart")
```

There are a total of 1,384,617 rows and 15 columns. The structure of the
data is contains many ids including order_id, product_id, user_id,
aisle_id, and department_id. Some key variables include product names,
order_hour_of_day, and reordered because they describe at what time
during the day specific products are purchased or reordered, which can
help the store advertise the most sold items during the hours they are
most often purchased or reordered.

``` r
instacart_df = instacart

aisles_df = instacart_df |> 
  select(order_id, aisle_id, aisle) 
```

# Problem 2

First, I load the BRFSS data.

``` r
library(p8105.datasets)
data("brfss_smart2010")
```

Now, I will do some data cleaning

``` r
brfss_df = brfss_smart2010 |> 
  janitor::clean_names() |> 
  select( year, loc_abbr = locationabbr, loc_des = locationdesc, everything()) |> 
  filter(response == "Excellent" | response == "Poor")
```
