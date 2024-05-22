Writing Function
================

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -1.372247252  0.968525076 -0.769951221  0.706012336 -0.994282316
    ##  [6] -0.423093472 -0.437647069 -0.500228177 -0.492345372 -1.256839045
    ## [11] -0.640593393 -1.030175051  1.534153193  0.756585151 -1.381710433
    ## [16]  0.648909044  0.921035745  2.304828278 -0.869457204 -0.506831824
    ## [21]  0.841805953  1.245993588  0.859837326 -0.454896196  1.562536029
    ## [26] -0.832665551 -1.054203217  0.131271937  0.533038494  0.002634642

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

    ##  [1] -1.372247252  0.968525076 -0.769951221  0.706012336 -0.994282316
    ##  [6] -0.423093472 -0.437647069 -0.500228177 -0.492345372 -1.256839045
    ## [11] -0.640593393 -1.030175051  1.534153193  0.756585151 -1.381710433
    ## [16]  0.648909044  0.921035745  2.304828278 -0.869457204 -0.506831824
    ## [21]  0.841805953  1.245993588  0.859837326 -0.454896196  1.562536029
    ## [26] -0.832665551 -1.054203217  0.131271937  0.533038494  0.002634642

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
    ## 1  2.43  3.75

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
    ## 1  4.23  2.99

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
    ## 1  6.51  2.77

``` r
sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.62  2.95

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.13  2.79

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
