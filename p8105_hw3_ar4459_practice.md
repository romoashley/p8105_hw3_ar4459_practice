P8105 HOMEWORK 3
================
ASHLEY ROMO
2023-10-05

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
# setting the width and height of the plots
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
```

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

``` r
instacart_df = instacart
 
# number of aisles 
total_aisle = instacart_df |> 
  select(aisle) |> distinct()
nrow(total_aisle)
```

    ## [1] 134

``` r
# aisles with the most items ordered from
most_ordered = instacart_df |> 
  group_by(aisle) |> 
  summarise(n_count=n()) |> 
  arrange(-n_count) |> 
  filter(row_number() == 1)
```

The total number of aisles is 134. The aisle with the most ordered from
is fresh vegetables, 150609.

### Problem 1 (ANSWER KEY)

#### Read in the data

``` r
data("instacart")

instacart = 
  instacart |> 
  as_tibble()
```

#### Answer questions about the data

This dataset contains 1384617 rows and 15 columns, with each row
resprenting a single product from an instacart order. Variables include
identifiers for user, order, and product; the order in which each
product was added to the cart. There are several order-level variables,
describing the day and time of the order, and number of days since prior
order. Then there are several item-specific variables, describing the
product name (e.g. Yogurt, Avocado), department (e.g. dairy and eggs,
produce), and aisle (e.g. yogurt, fresh fruits), and whether the item
has been ordered by this user in the past. In total, there are 39123
products found in 131209 orders from 131209 distinct users.

Below is a table summarizing the number of items ordered from aisle. In
total, there are 134 aisles, with fresh vegetables and fresh fruits
holding the most items ordered by far.

``` r
instacart |> 
  count(aisle) |> 
  arrange(desc(n))
```

    ## # A tibble: 134 × 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # ℹ 124 more rows

Next is a plot that shows the number of items ordered in each aisle.
Here, aisles are ordered by ascending number of items.

``` r
instacart |> 
  count(aisle) |> 
  filter(n > 10000) |> 
  mutate(aisle = fct_reorder(aisle, n)) |> 
  ggplot(aes(x = aisle, y = n)) + 
  geom_point() + 
  labs(title = "Number of items ordered in each aisle") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

<img src="p8105_hw3_ar4459_practice_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

Our next table shows the three most popular items in aisles
`baking ingredients`, `dog food care`, and `packaged vegetables fruits`,
and includes the number of times each item is ordered in your table.

``` r
instacart |> 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) |>
  group_by(aisle) |> 
  count(product_name) |> 
  mutate(rank = min_rank(desc(n))) |> 
  filter(rank < 4) |> 
  arrange(desc(n)) |>
  knitr::kable()
```

| aisle                      | product_name                                  |    n | rank |
|:---------------------------|:----------------------------------------------|-----:|-----:|
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |

Finally is a table showing the mean hour of the day at which Pink Lady
Apples and Coffee Ice Cream are ordered on each day of the week. This
table has been formatted in an untidy manner for human readers. Pink
Lady Apples are generally purchased slightly earlier in the day than
Coffee Ice Cream, with the exception of day 5.

``` r
instacart |>
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) |>
  group_by(product_name, order_dow) |>
  summarize(mean_hour = mean(order_hour_of_day)) |>
  pivot_wider(
    names_from = order_dow, 
    values_from = mean_hour) |>
  knitr::kable(digits = 2)
```

    ## `summarise()` has grouped output by 'product_name'. You can override using the
    ## `.groups` argument.

| product_name     |     0 |     1 |     2 |     3 |     4 |     5 |     6 |
|:-----------------|------:|------:|------:|------:|------:|------:|------:|
| Coffee Ice Cream | 13.77 | 14.32 | 15.38 | 15.32 | 15.22 | 12.26 | 13.83 |
| Pink Lady Apples | 13.44 | 11.36 | 11.70 | 14.25 | 11.55 | 12.78 | 11.94 |

# Problem 2

First, I load the BRFSS data.

``` r
library(p8105.datasets)
data("brfss_smart2010")
```

Now, I will do some data cleaning

``` r
brfss_df = 
  brfss_smart2010 |> 
  janitor::clean_names() |> 
  select(
    year, state = locationabbr, county = locationdesc, everything()
    ) |> 
  filter(
    response == "Poor" | response == "Fair"| response == "Good"| response == "Very good"| response == "Excellent"
    ) |> 
  mutate(
    response = factor(response, levels = c("Poor", "Fair", "Good", "Very good", "Excellent"))
    )
```

``` r
#states observed at 7 or more locations in 2002
states_2002 =
  brfss_df |> 
  filter(year == 2002) |> 
  group_by(state) |> 
  summarize(n_obs = n()) |> 
  filter(n_obs >= 7) 
  
nrow(states_2002)
```

    ## [1] 36

``` r
#states observed at 7 or more location in 2010
states_2010 =
  brfss_df |> 
  filter(year == 2010) |> 
  group_by(state) |> 
  summarize(n_obs = n()) |> 
  filter(n_obs >= 7) 

nrow(states_2010)
```

    ## [1] 45

In 2002, 36 were observed at 7 or more locations.

In 2010, 45 were observed at 7 or more locations.

Spaghetti plot of the average data value over time within a state.

``` r
excellent_df = 
  brfss_df |> 
  filter(response == "Excellent") |> 
  select(year, state, data_value) |> 
  group_by(year, state) |> 
  mutate(
   avg_data_val = mean(data_value)
  ) |> 
  ggplot(aes(x = year, y = avg_data_val, color = state)) +
  geom_line() + 
  theme(legend.position ="bottom")


excellent_df
```

    ## Warning: Removed 65 rows containing missing values (`geom_line()`).

<img src="p8105_hw3_ar4459_practice_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" />

Two-panel plot

``` r
val_2006 =
  brfss_df |> 
  filter(year == 2006, state == "NY") |> 
  group_by(data_value, response) |> 
  ggplot(aes(x = response, y = data_value, color = county)) +
  geom_boxplot() +
  labs(
    title = "2006",
    color = "Locations in NY State",
    x = "Response",
    y = "Value"
  )

val_2006
```

<img src="p8105_hw3_ar4459_practice_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" />

``` r
val_2010 =
  brfss_df |> 
  filter(year == 2010, state == "NY") |> 
  group_by(data_value, response) |> 
  ggplot(aes(x = response, y = data_value, color = county)) +
  geom_boxplot() +
  labs(
    title = "2010",
    color = "Locations in NY State",
    x = "Response",
    y = "Value"
  )
```

## Problem 3

First, load the data

``` r
accel_df = read_csv("data/nhanes_accel.csv") |> 
  janitor::clean_names() |> 
  pivot_longer(
    min1:min1440, 
    names_prefix = "min",
    names_to = "min",
    values_to = "phys_act"
  )
```

    ## Rows: 250 Columns: 1441
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (1441): SEQN, min1, min2, min3, min4, min5, min6, min7, min8, min9, min1...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
covar_df = read_csv("data/nhanes_covar.csv", skip = 4) |> 
  janitor::clean_names() |> 
  drop_na() |> 
  mutate(
     sex = case_match(
       sex,
        1 ~ "male",
       2 ~ "female"
     ),
     education = case_match(
        education,
       1 ~ "Less than high school",
       2 ~ "High school equivalent",
       3 ~ "More than high school"
     )
  ) |> 
  mutate(
    education = factor(
      education, 
      levels = c("Less than high school", "High school equivalent", "More than high school")
      )
    ) |> 
  filter(age >= "21") 
```

    ## Rows: 250 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (5): SEQN, sex, age, BMI, education
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Now, we merge the two datasets.

``` r
merged_df = left_join(accel_df, covar_df)
```

    ## Joining with `by = join_by(seqn)`

Tables for men and women

``` r
#table
sex_df = 
  covar_df |>
  group_by(sex, education) |> 
  count(sex, education) |> 
  pivot_wider(
    names_from = education,
    values_from = n
  ) |> 
  knitr::kable()

sex_df
```

| sex    | Less than high school | High school equivalent | More than high school |
|:-------|----------------------:|-----------------------:|----------------------:|
| female |                    28 |                     23 |                    59 |
| male   |                    27 |                     35 |                    56 |

``` r
#visualization
# one plot for each education level with male and female 
male_plot = 
  covar_df |>
  group_by(sex, education) |> 
  mutate(n_obs = n()) |> 
  filter(sex == "male") |> 
  ggplot(aes(x = age, fill = sex )) +
  geom_density()

male_plot
```

<img src="p8105_hw3_ar4459_practice_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" />

``` r
fem_plot = 
  covar_df |>
  group_by(sex, education) |> 
  mutate(n_obs = n()) |> 
  filter(sex == "female") |> 
  ggplot(aes(x = age, fill = sex)) +
  geom_density()

fem_plot
```

<img src="p8105_hw3_ar4459_practice_files/figure-gfm/unnamed-chunk-16-2.png" width="90%" />

Total activity

``` r
merged_df |> 
  group_by(seqn, phys_act) |> 
  summarize(sum(phys_act))
```

    ## `summarise()` has grouped output by 'seqn'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 349,275 × 3
    ## # Groups:   seqn [250]
    ##     seqn phys_act `sum(phys_act)`
    ##    <dbl>    <dbl>           <dbl>
    ##  1 62161   0.0163          0.0163
    ##  2 62161   0.0185          0.0185
    ##  3 62161   0.0243          0.0243
    ##  4 62161   0.0245          0.0245
    ##  5 62161   0.0258          0.0517
    ##  6 62161   0.0263          0.0263
    ##  7 62161   0.027           0.027 
    ##  8 62161   0.0273          0.0547
    ##  9 62161   0.0277          0.0277
    ## 10 62161   0.0277          0.0277
    ## # ℹ 349,265 more rows
