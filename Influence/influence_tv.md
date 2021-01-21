Regression and Other Stories: Influence
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-01-20

Tidyverse version by Bill Behrman.

Plot influence of individual points in a fitted regression. See Chapter
8 in Regression and Other Stories.

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)
library(rstanarm)

# Parameters
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

Simulated data.

``` r
set.seed(264)

a <- 1
b <- 2
sigma <- 5

data <- 
  tibble(
    x = 2:12,
    y = rnorm(length(x), mean = a + b * x, sd = sigma)
  )
```

Linear regression fit of data.

``` r
set.seed(148)

fit <- stan_glm(y ~ x, data = data, refresh = 0)
```

Plot of linear regression with residuals.

``` r
intercept <- coef(fit)[["(Intercept)"]]
slope <- coef(fit)[["x"]]

v <- 
  data %>% 
  mutate(pred = intercept + slope * x)

v %>% 
  ggplot(aes(x, y)) +
  geom_abline(slope = slope, intercept = intercept) +
  geom_segment(aes(xend = x, yend = pred), color = "gray60") +
  geom_point() +
  scale_x_continuous(breaks = scales::breaks_width(2)) +
  labs(title = "Linear regression with residuals")
```

<img src="influence_tv_files/figure-gfm/unnamed-chunk-4-1.png" width="100%" />
