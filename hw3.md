hw3
================
Yu Si
10/20/2021

## Problem 1

``` r
data("instacart")
instacart = as_tibble(instacart)
```

In this dataset, there are 1384617 rows and 15 columns. Variable
represents users, order, product and related information. Each row
represent each product from order. order-related variable included
number, day and hour, such as order\_number and
days\_since\_prior\_order. product-related variable included product
name, aisle and department, such as fresh fruit, Banana, dairy eggs.

### 1.a

``` r
instacart %>% 
  count(aisle, sort = TRUE)
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
    ## # … with 124 more rows

There are 134 aisles and the most item is fresh vegetable.
