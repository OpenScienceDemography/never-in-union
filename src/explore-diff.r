library(tidyverse)
library(diffdf)

aggregate_file_old <-
  "out/d_all_minage35_edu2_.csv" |>
  read_csv() |>
  arrange(country, sex, bc_cate, edu2)

aggregate_file <-
  "c:/Users/DstMove/Downloads/foo/d_all_minage35_edu2_.csv" |>
  read_csv() |>
  arrange(country, sex, bc_cate, edu2)


diffdf::diffdf(aggregate_file_old, aggregate_file)

diffdf::diffdf(
  aggregate_file_old |>
    filter(country == "Canada"),
  aggregate_file |>
    filter(country == "Canada")
)


left_join(
  aggregate_file_old |>
    filter(country == "Canada") |>
    select(country, sex, bc_cate, edu2, prop_childless),
  aggregate_file_old |>
    filter(country == "Canada") |>
    select(country, sex, bc_cate, edu2, prop_childless),
  by = c("country", "sex", "bc_cate", "edu2")
) |>
  view()
