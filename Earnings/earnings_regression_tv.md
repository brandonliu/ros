Regression and Other Stories: Earnings
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-01-15

-   [Chapter 6](#chapter-6)
    -   [Data](#data)
    -   [Linear regression of earnings on height and sex with no
        interaction](#linear-regression-of-earnings-on-height-and-sex-with-no-interaction)
    -   [Linear regression of earnings on height and sex with
        interaction](#linear-regression-of-earnings-on-height-and-sex-with-interaction)

Tidyverse version by Bill Behrman.

Predict respondents’ yearly earnings using survey data from 1990. See
Chapters 6 and 12 in Regression and Other Stories.

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)
library(rstanarm)

# Parameters
  # Seed
SEED <- 7783
  # Earnings data
file_earnings <- here::here("Earnings/data/earnings.csv")
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

# Chapter 6

## Data

``` r
earnings <- 
  file_earnings %>% 
  read_csv() %>% 
  mutate(
    sex = 
      case_when(
        male == 0 ~ "Female",
        male == 1 ~ "Male",
        TRUE ~ NA_character_
      )
  )

earnings %>% 
  select(height, sex, earn)
```

    #> # A tibble: 1,816 x 3
    #>    height sex     earn
    #>     <dbl> <chr>  <dbl>
    #>  1     74 Male   50000
    #>  2     66 Female 60000
    #>  3     64 Female 30000
    #>  4     65 Female 25000
    #>  5     63 Female 50000
    #>  6     68 Female 62000
    #>  7     63 Female 51000
    #>  8     64 Female  9000
    #>  9     62 Female 29000
    #> 10     73 Male   32000
    #> # … with 1,806 more rows

## Linear regression of earnings on height and sex with no interaction

``` r
fit_2 <- 
  stan_glm(earn ~ height + sex, data = earnings, refresh = 0, seed = SEED)

print(fit_2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      earn ~ height + sex
    #>  observations: 1816
    #>  predictors:   3
    #> ------
    #>             Median   MAD_SD  
    #> (Intercept) -26129.1  11683.7
    #> height         650.6    179.7
    #> sexMale      10630.6   1445.1
    #> 
    #> Auxiliary parameter(s):
    #>       Median  MAD_SD 
    #> sigma 21393.9   360.2
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

``` r
fit_params <- 
  tribble(
    ~sex, ~intercept, ~slope,
    "Female", coef(fit_2)[["(Intercept)"]], coef(fit_2)[["height"]],
    "Male", 
      coef(fit_2)[["(Intercept)"]] + coef(fit_2)[["sexMale"]],
      coef(fit_2)[["height"]]
  )

offset <- 0.2

earnings %>% 
  mutate(
    height =
      case_when(
        sex == "Female" ~ height - offset,
        sex == "Male" ~ height + offset,
        TRUE ~ NA_real_
      )
  ) %>% 
  ggplot(aes(height, earn, color = sex)) +
  geom_count() +
  geom_abline(
    aes(slope = slope, intercept = intercept, color = sex),
    data = fit_params
  ) +
  coord_cartesian(ylim = c(0, 1e5)) +
  scale_x_continuous(breaks = scales::breaks_width(1), minor_breaks = NULL) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme(legend.position = "bottom") +
  labs(
    title = 
      "Linear regression of earnings on height and sex with no interaction",
    x = "Height",
    y = "Earnings",
    color = "Sex",
    size = "Count"
  )
```

<img src="earnings_regression_tv_files/figure-gfm/unnamed-chunk-4-1.png" width="100%" />

The equations for the regression lines are:

    Men:   y = -15499 + 651 x
    Women: y = -26129 + 651 x

## Linear regression of earnings on height and sex with interaction

``` r
fit_3 <- 
  stan_glm(
    earn ~ height + sex + height:sex,
    data = earnings,
    refresh = 0,
    seed = SEED
  )

print(fit_3)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      earn ~ height + sex + height:sex
    #>  observations: 1816
    #>  predictors:   4
    #> ------
    #>                Median   MAD_SD  
    #> (Intercept)     -9779.4  15218.0
    #> height            399.0    234.7
    #> sexMale        -28439.8  23449.0
    #> height:sexMale    577.6    345.4
    #> 
    #> Auxiliary parameter(s):
    #>       Median  MAD_SD 
    #> sigma 21401.5   351.6
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

``` r
lines <- 
  tribble(
    ~sex, ~intercept, ~slope,
    "Female", coef(fit_3)[["(Intercept)"]], coef(fit_3)[["height"]],
    "Male", 
      coef(fit_3)[["(Intercept)"]] + coef(fit_3)[["sexMale"]],
      coef(fit_3)[["height"]] + coef(fit_3)[["height:sexMale"]]
  )

offset <- 0.2

earnings %>% 
  mutate(
    height =
      case_when(
        sex == "Female" ~ height - offset,
        sex == "Male" ~ height + offset,
        TRUE ~ NA_real_
      )
  ) %>% 
  ggplot(aes(height, earn, color = sex)) +
  geom_count() +
  geom_abline(
    aes(slope = slope, intercept = intercept, color = sex), 
    data = lines
  ) +
  coord_cartesian(ylim = c(0, 1e5)) +
  scale_x_continuous(breaks = scales::breaks_width(1), minor_breaks = NULL) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme(legend.position = "bottom") +
  labs(
    title = 
      "Linear regression of earnings on height and sex with interaction",
    x = "Height",
    y = "Earnings",
    color = "Sex",
    size = "Count"
  )
```

<img src="earnings_regression_tv_files/figure-gfm/unnamed-chunk-6-1.png" width="100%" />

The equations for the regression lines are:

    Men:   y = -15499 + 651 x
    Women: y =  -26129 + 651 x

From the plots, we can see that many more women than men have no
earnings.

``` r
earnings %>% 
  count(earn, sex) %>% 
  group_by(sex) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  filter(earn == 0)
```

    #> # A tibble: 2 x 4
    #>    earn sex        n   prop
    #>   <dbl> <chr>  <int>  <dbl>
    #> 1     0 Female   172 0.151 
    #> 2     0 Male      15 0.0222

15% of women have no earnings, whereas only 2% of men have no earnings.
