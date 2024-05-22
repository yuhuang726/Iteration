Iteration and Listcols
================

## Lists

You can put anything in a list. We can’t put them in the dataframe, but
we can put them in a list.

``` r
l = list(vec_numeric = 5:8,
vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
mat = matrix(1:8, nrow = 2, ncol = 4),
summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.10958 -0.65567  0.07760  0.02653  0.65410  1.92941

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

Look at/ do some calcualtion on particular element in the list.

## `for` loop

Create a new list.

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = .2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

Pause and get my old function.

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if(length(x) < 3) {
    stop("Input must have at least three numbers")
  }

  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
 
}
```

I can apply that function to each list element.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.17 0.912

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.41  4.80

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.200

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.22  1.08

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for(i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```

## map

``` r
output = map(list_norm, mean_and_sd)
```

what if you want a different function?

``` r
output = map(list_norm, median)
output = map(list_norm, IQR)
```

``` r
output = map_dbl(list_norm, median)
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

.id = “input”: put the a, b, c, d name into a column named input.

## List columns

``` r
listcol_df = 
  tibble(
    name = c ("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 4.938860 3.463315 2.789211 3.319418 5.055293 3.448938 3.185408 2.462145
    ##  [9] 1.811275 4.220982 1.892613 2.426407 2.479025 3.861924 3.110338 2.364191
    ## [17] 3.401583 2.293278 2.943030 3.983458
    ## 
    ## $b
    ##  [1]  -5.74577199  -3.77126517   4.77364235   2.12620806  -1.26426033
    ##  [6]  -1.28970460   1.52394057   5.63122420  -3.84966905  -1.60551939
    ## [11]  -1.03806665  11.44319820  -1.56529760  -5.76043401  -0.08402536
    ## [16]  -1.86071082  -9.96608228   1.89490663  -3.31346463   2.22218422
    ## [21]   3.83670591  -3.45752582  -4.11629891  -2.37402010   1.85541401
    ## [26]   0.90678211  -3.88518254 -10.06538798  -1.98659766 -11.61172972
    ## 
    ## $c
    ##  [1] 10.211879 10.067635 10.270766 10.243081 10.449361 10.136373 10.295074
    ##  [8] 10.174721 10.007312  9.989412 10.122602  9.917922  9.785502  9.949069
    ## [15]  9.872749 10.034441  9.979845  9.932956 10.051741  9.582959  9.831268
    ## [22] 10.108916  9.965944  9.887056 10.080493 10.075994 10.089716 10.225575
    ## [29]  9.965424  9.928377 10.322269  9.900076  9.651333  9.828949  9.947869
    ## [36] 10.214015  9.934231 10.518909  9.746783 10.196100
    ## 
    ## $d
    ##  [1] -4.158826 -2.484193 -2.509170 -2.631401 -4.697978 -1.909775 -3.362066
    ##  [8] -3.096701 -4.256290 -3.152882 -2.620696 -2.598792 -4.089733 -2.355653
    ## [15] -3.993801 -5.888692 -1.483899 -3.124475 -3.972401 -2.058636

``` r
  listcol_df %>% 
    filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations.

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.17 0.912

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.41  4.80

\$samp: pull out the column samp

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.17 0.912
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.41  4.80
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.200
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.22  1.08

implement mean_and_sd function on the samp column in listcol_df.

So … can I add a list column??

``` r
listcol_df =
  listcol_df %>% 
   mutate(
     summary = map(samp, mean_and_sd),
     medians = map_dbl(samp, median))
```

## Weather Data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") %>% 
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10)  %>% 
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/yu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-05-18 14:02:16.709216 (8.605)

    ## file min/max dates: 1869-01-01 / 2024-05-31

    ## using cached file: /Users/yu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2024-05-18 14:02:38.87762 (3.896)

    ## file min/max dates: 1949-10-01 / 2024-05-31

    ## using cached file: /Users/yu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2024-05-18 14:02:43.732044 (1.022)

    ## file min/max dates: 1999-09-01 / 2024-05-31

Get our list columns ..

``` r
weather_nest = 
  weather_df %>% 
  nest(data = date: tmin)
```

nest: nest a subset of columns in a data frame into a single
list-column. In this example, we nest data, prcp, tmax, and tmin into
the list-column named data.

``` r
weather_nest %>% pull(name)
```

    ## [1] "CentralPark_NY" "Molokai_HI"     "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   157   4.4   0.6
    ##  2 2021-01-02    13  10.6   2.2
    ##  3 2021-01-03    56   3.3   1.1
    ##  4 2021-01-04     5   6.1   1.7
    ##  5 2021-01-05     0   5.6   2.2
    ##  6 2021-01-06     0   5     1.1
    ##  7 2021-01-07     0   5    -1  
    ##  8 2021-01-08     0   2.8  -2.7
    ##  9 2021-01-09     0   2.8  -4.3
    ## 10 2021-01-10     0   5    -1.6
    ## # ℹ 720 more rows
    ## 
    ## [[2]]
    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01     0  27.8  22.2
    ##  2 2021-01-02     0  28.3  23.9
    ##  3 2021-01-03     0  28.3  23.3
    ##  4 2021-01-04     0  30    18.9
    ##  5 2021-01-05     0  28.9  21.7
    ##  6 2021-01-06     0  27.8  20  
    ##  7 2021-01-07     0  29.4  21.7
    ##  8 2021-01-08     0  28.3  18.3
    ##  9 2021-01-09     0  27.8  18.9
    ## 10 2021-01-10     0  28.3  18.9
    ## # ℹ 720 more rows
    ## 
    ## [[3]]
    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   254   3.2   0  
    ##  2 2021-01-02   152   0.9  -3.2
    ##  3 2021-01-03     0   0.2  -4.2
    ##  4 2021-01-04   559   0.9  -3.2
    ##  5 2021-01-05    25   0.5  -3.3
    ##  6 2021-01-06    51   0.8  -4.8
    ##  7 2021-01-07     0   0.2  -5.8
    ##  8 2021-01-08    25   0.5  -8.3
    ##  9 2021-01-09     0   0.1  -7.7
    ## 10 2021-01-10   203   0.9  -0.1
    ## # ℹ 720 more rows

``` r
weather_nest$data[[1]]
```

    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   157   4.4   0.6
    ##  2 2021-01-02    13  10.6   2.2
    ##  3 2021-01-03    56   3.3   1.1
    ##  4 2021-01-04     5   6.1   1.7
    ##  5 2021-01-05     0   5.6   2.2
    ##  6 2021-01-06     0   5     1.1
    ##  7 2021-01-07     0   5    -1  
    ##  8 2021-01-08     0   2.8  -2.7
    ##  9 2021-01-09     0   2.8  -4.3
    ## 10 2021-01-10     0   5    -1.6
    ## # ℹ 720 more rows

weather_nest\$data\[\[1\]\]: get the tibble for center park

Suppose I want to regress `tmax` on `tmin` for each station.

``` r
lm(tmax ~ tmin, data = weather_nest$data[[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222

lm(tmax ~ tmin, data = weather_nest\$data\[\[1\]\]): do a linear
regression using tmin to estimate tmax in center park.

Let’s write a function.

``` r
weather_lm = function(df) {
  
  lm (tmax ~ tmin, data = df)
  
}

output = vector("list", 3)

for (i in 1:3) {
  
  output[[i]] = weather_lm(weather_nest$data[[i]])
  
}
```

What about map?

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

What about a map in a list column?

``` r
weather_nest =
  weather_nest %>% 
  mutate(models = map(data, weather_lm))
```
