Writing Function
================

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.10043310 -0.65323900 -0.27803729 -0.88592058  0.98826474  0.56084804
    ##  [7] -1.08565338 -0.16450831 -0.33584901 -0.76860762 -0.78000245  0.19483362
    ## [13] -1.35780020 -0.30212410 -2.36917388  0.30972579  2.43132373 -0.02407392
    ## [19]  1.16549744  0.38710363  0.34873286  0.45375530  1.80334990  1.06111950
    ## [25]  0.12636227 -1.78952924  0.41973143  0.03154997  0.75562398 -0.34373633

I want a function to compute z-score

``` r
z_score = function(x) {
  
  if(!is.numeric(x)) {
    stop("Imput must be numeric")
  }
  
  if(length(x) < 3) {
    stop("Imput must have at least three numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}

z_score(x_vec)
```

    ##  [1]  0.10043310 -0.65323900 -0.27803729 -0.88592058  0.98826474  0.56084804
    ##  [7] -1.08565338 -0.16450831 -0.33584901 -0.76860762 -0.78000245  0.19483362
    ## [13] -1.35780020 -0.30212410 -2.36917388  0.30972579  2.43132373 -0.02407392
    ## [19]  1.16549744  0.38710363  0.34873286  0.45375530  1.80334990  1.06111950
    ## [25]  0.12636227 -1.78952924  0.41973143  0.03154997  0.75562398 -0.34373633

Try my function on some other things. This should give errors.

``` r
z_score(3)
```

    ## Error in z_score(3): Imput must have at least three numbers

``` r
z_score("my name is jeff")
```

    ## Error in z_score("my name is jeff"): Imput must be numeric

``` r
z_score(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_score(c(TRUE, TRUE, FALSE, TRUE)): Imput must be numeric

## Multiple outputs

``` r
mean_and_sd= function(x) {
  
  if(!is.numeric(x)) {
    stop("Imput must be numeric")
  }
  
  if(length(x) < 3) {
    stop("Imput must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )

}
```

Check that the function works

``` r
x_vec = rnorm(100, mean =3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.14  4.18

## Multiple inputs

I’d like to do this with a function.

``` r
sim_data = 
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.01  2.94

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 3) {
  
  sim_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
}
```

``` r
sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.24  3.03

``` r
sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.62  2.57

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.52  3.04

## Let’s review Napoleon Dyamite

``` r
url ="https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_on1_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)  

review_titles <- dynamite_html %>%
  html_nodes(xpath = "//a[@data-hook='review-title']/span") %>% 
  html_text(trim = TRUE)

review_titles <- review_titles[review_titles != "" ]

review_stars =  
    dynamite_html %>%   
    html_nodes("#cm_cr-review_list .review-rating") %>% 
    html_text() %>% 
    str_extract("^\\d") %>% 
    as.numeric()    

review_text =
    dynamite_html %>%   
  html_nodes(".review-text-content span") %>%   
    html_text() %>% 
    str_replace_all("\n", "") %>%   
    str_trim()  

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews? Let’s turn that code into a
function

``` r
read_page_reviews = function(url) {

  html = read_html(url)

review_titles <- html %>%
  html_nodes(xpath = "//a[@data-hook='review-title']/span") %>% 
  html_text(trim = TRUE)

review_titles <- review_titles[review_titles != "" ]

  review_stars =    
    html %>%    
    html_nodes("#cm_cr-review_list .review-rating") %>% 
    html_text() %>% 
    str_extract("^\\d") %>% 
    as.numeric()    
  
  review_text =
    html %>%    
    html_nodes(".review-text-content span") %>% 
    html_text() %>% 
    str_replace_all("\n", "") %>%   
    str_trim()  
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )

  reviews
}
```

Let me try my function.

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_on1_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

Let’s read a few pages of reviews.

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_on1_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

all_reviews =
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5]),
)
```

## Mean scoping example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

## Function as arguments

``` r
my_summary = function(x, summ_func) {
  
  summ_func(x)
  
}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 2.956942

``` r
median(x_vec)
```

    ## [1] 3.328733

``` r
my_summary(x_vec, mean)
```

    ## [1] 2.956942

``` r
my_summary(x_vec, IQR)
```

    ## [1] 9.962571
