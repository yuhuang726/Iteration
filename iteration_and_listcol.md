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
    ## -2.92274 -0.63422 -0.05024 -0.07248  0.58750  2.22628

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
    ## 1  2.99 0.787

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.281  5.24

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.205

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.96 0.989

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
    ##  [1] 2.502151 2.805902 2.740839 3.217991 1.143544 3.613321 3.330446 3.249492
    ##  [9] 3.442481 3.106090 2.012978 3.058362 2.693235 3.828859 2.687488 3.997764
    ## [17] 4.565407 3.468262 2.639207 1.763901
    ## 
    ## $b
    ##  [1]  -1.07755437   9.99030580  -0.79798468   5.66266973   6.94332815
    ##  [6]  -2.51901706  -1.75247427   5.75411899  -0.98338057  -1.54897883
    ## [11]  -0.77109381  -2.34217099   1.21848632   0.08767426  -3.51511519
    ## [16]  -0.57211943  -3.34370413   6.99835920   2.18036178  -6.64884866
    ## [21]  -3.02751132  -1.84581404   6.11995071  -1.44418750  -3.18292905
    ## [26]   8.08159696  11.25310554  -1.99642597  -6.70815513 -11.79336726
    ## 
    ## $c
    ##  [1] 10.066784 10.003791 10.077187  9.465988 10.214760 10.234461 10.031782
    ##  [8]  9.717603 10.084092 10.151088 10.320908 10.000426  9.896274 10.003895
    ## [15]  9.945945  9.685318  9.960325  9.939541  9.950378 10.240573 10.000351
    ## [22]  9.920394 10.249629 10.098558  9.708703  9.945031 10.044219 10.381324
    ## [29] 10.375989 10.057684  9.806454  9.793217  9.661368 10.102818  9.884474
    ## [36]  9.924467  9.962246 10.021406 10.356726 10.227541
    ## 
    ## $d
    ##  [1] -2.177019 -3.273055 -2.579015 -2.745956 -1.885702 -1.227254 -1.226918
    ##  [8] -3.801098 -4.183486 -2.395039 -4.147323 -3.316305 -1.841041 -2.877896
    ## [15] -3.369954 -3.393040 -2.970634 -3.217213 -3.516405 -5.078793

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
    ## 1  2.99 0.787

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.281  5.24

\$samp: pull out the column samp

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.99 0.787
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.281  5.24
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.205
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.96 0.989

implement mean_and_sd function on the samp column in listcol_df.

So … can I add a list column??

``` r
listcol_df =
  listcol_df %>% 
   mutate(
     summary = map(samp, mean_and_sd),
     medians = map_dbl(samp, median))
```
