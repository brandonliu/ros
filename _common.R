set.seed(42)

options(
  digits = 3,
  dplyr.print_max = 10,
  dplyr.print_min = 10,
  dplyr.summarise.inform = FALSE
)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = FALSE,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold",
  out.width = "100%"
)
