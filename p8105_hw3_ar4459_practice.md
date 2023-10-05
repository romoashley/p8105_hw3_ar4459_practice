p8105_hw3_ar4459_practice
================
ASHLEY ROMO
2023-10-05

## Problem 1

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