Regression and Other Stories: Coop
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2020-12-17

-   [Data](#data)
-   [Plots](#plots)
    -   [Cummulative vote percentages at different points in
        counting](#cummulative-vote-percentages-at-different-points-in-counting)
    -   [Vote percentages of new votes at different points in
        counting](#vote-percentages-of-new-votes-at-different-points-in-counting)
    -   [Standard deviation of vote proportions of new votes for each
        candidate](#standard-deviation-of-vote-proportions-of-new-votes-for-each-candidate)

Tidyverse version by Bill Behrman.

Coop - Example of hypothesis testing. See Chapter 4 in Regression and
Other Stories.

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)

# Parameters
  # Votes in an election
file_votes <- here::here("Coop/data/Riverbay.csv")
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

## Data

``` r
votes <- 
  file_votes %>% 
  read_csv(
    col_names = 
      c("name_1", "600", "1200", "2444", "3444", "4444", "5553", "name_2")
  ) %>% 
  pivot_longer(
    cols = !starts_with("name"),
    names_to = "voters",
    names_transform = list(voters = as.double),
    values_to = "votes"
  )
```

Candidates who received the most votes

``` r
voters <- 
  votes %>% 
  pull(voters) %>% 
  unique() %>% 
  sort()
voters_total <- max(voters)

votes %>% 
  filter(voters == voters_total) %>% 
  select(!voters) %>% 
  arrange(desc(votes))
```

    #> # A tibble: 27 x 3
    #>    name_1          name_2           votes
    #>    <chr>           <chr>            <dbl>
    #>  1 Al Shapiro      Hal Spitzer       3040
    #>  2 Marie Heath     Margie Best       2300
    #>  3 Craig Williams  Greg Stevens      2176
    #>  4 Saul Weber      Josh Walker       2131
    #>  5 Othelia Jones   Clotelia Smith    2020
    #>  6 Tom Barrett     Dave Barron       1973
    #>  7 Alonzo Newton   Alphonse Preston  1855
    #>  8 Tony Illis      Andy Willis       1571
    #>  9 Alan Berger     <NA>              1519
    #> 10 Ruben Berkowitz <NA>              1346
    #> # … with 17 more rows

``` r
candidates_top_8 <- 
  votes %>% 
  filter(voters == voters_total) %>% 
  arrange(desc(votes)) %>% 
  slice(1:8) %>% 
  pull(name_2)
```

## Plots

### Cummulative vote percentages at different points in counting

``` r
v <- 
  votes %>% 
  filter(name_2 %in% candidates_top_8) %>% 
  mutate(
    name = ordered(name_2, levels = candidates_top_8),
    vote_prop = votes / voters
  )

v %>% 
  ggplot(aes(voters, vote_prop)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  facet_wrap(facets = vars(name), ncol = 4) +
  theme(axis.text = element_text(size = rel(0.6))) +
  labs(
    title = "Cummulative vote percentages at different points in counting",
    subtitle = "For top 8 candidates",
    x = "Cummulative voters",
    y = "Vote percentage",
    caption = "Voters could vote for up to 6 candidates"
  )
```

<img src="riverbay_tv_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

### Vote percentages of new votes at different points in counting

``` r
v <- 
  votes %>% 
  filter(name_2 %in% candidates_top_8) %>% 
  mutate(name = ordered(name_2, levels = candidates_top_8)) %>% 
  group_by(name) %>% 
  mutate(
    new_vote_prop = 
      (votes - lag(votes, default = 0, order_by = voters)) / 
      (voters - lag(voters, default = 0, order_by = voters))
  ) %>% 
  ungroup()

v %>% 
  ggplot(aes(voters, new_vote_prop)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  facet_wrap(facets = vars(name), ncol = 4) +
  theme(axis.text = element_text(size = rel(0.6))) +
  labs(
    title = "Vote percentages of new votes at different points in counting",
    subtitle = "For top 8 candidates",
    x = "Cummulative voters",
    y = "Vote percentage of new votes",
    caption = "Voters could vote for up to 6 candidates"
  )
```

<img src="riverbay_tv_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

### Standard deviation of vote proportions of new votes for each candidate

``` r
v <- 
  votes %>% 
  group_by(name_1) %>% 
  mutate(
    new_vote_prop = 
      (votes - lag(votes, default = 0, order_by = voters)) / 
      (voters - lag(voters, default = 0, order_by = voters))
  ) %>% 
  summarize(
    votes_final = votes[voters == voters_total],
    new_vote_prop_sd = sd(new_vote_prop),
  )

new_voters <- voters - lag(voters, default = 0)
new_vote_prop_sd_expected <- 
  tibble(
    votes_final = seq(min(v$votes_final), max(v$votes_final), length.out = 101),
    vote_prop_final = votes_final / voters_total,
    new_vote_prop_sd = 
      map_dbl(vote_prop_final, ~ sqrt(mean(. * (1 - .) / new_voters)))
  )

ggplot(mapping = aes(votes_final, new_vote_prop_sd)) +
  geom_line(data = new_vote_prop_sd_expected) +
  geom_point(data = v) +
  scale_x_continuous(breaks = scales::breaks_width(500)) +
  labs(
    title = 
      "Standard deviation of vote proportions of new votes for each candidate",
    subtitle = 
      "Line is theoretical standard deviation expected under null hypothesis",
    x = "Total votes for candidate",
    y = "Standard deviation of vote proportions"
  )
```

<img src="riverbay_tv_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />
