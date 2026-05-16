#===============================================================================
# 2023-03-18 -- never-in-union (Refactored 2026-04-03)
# prepare data
# Ryo Mogi, rymo@sdu.dk
# Ewa Batyra, ebatyra@ced.uab.es
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

source("src/0-prepare-session.R")

# # NOTE: for full reproducibility get access to the raw data (ESS, GGS, and DHS) and place the needed data files in the project sub-directory, adapt the paths below as needed. See README.md for more details on data needed for full replication and instructions on obtaining access to these datasets.

# # Variables for external data paths
# path_ess3 <- "data-raw/ESS/ESS3e03_7.sav"
# path_ess9 <- "data-raw/ESS/ESS9e03_2.sav"
# path_hh0 <- "data-raw/GGS_Harmonized/HH/HARMONIZED-HISTORIES_ALL_GGSaccess.dta"
# path_hh1 <- "data-raw/GGS_Harmonized/HH1/HARMONIZED-HISTORIES_I.dta"
# path_hh2 <- "data-raw/GGS_Harmonized/HH2/HarmonizedHistoriesII_2023_07_10.dta"
# path_dhs <- "data-raw/DHS/ucp_red_edu.dta"

path_un <- "dat/un_data_90001020.dta"
aggregate_path <- "out/aggregate_dataset.csv"

# -------------------------------------------------------------------------
# 1. Helper Function for Aggregation (formerly 01_function_clean.R) ----
# -------------------------------------------------------------------------
func_makedata2 <- function(oridata, minage) {
  d_total <- oridata |>
    filter(age >= minage) |>
    group_by(country, sex, bc_cate, edu2) |>
    summarise(total_n = sum(weight, na.rm = TRUE), .groups = "drop")

  d_childless <- oridata |>
    filter(age >= minage) |>
    group_by(country, sex, bc_cate, edu2) |>
    summarise(
      childless_n = sum(weight[everbirth == 0], na.rm = TRUE),
      .groups = "drop"
    )

  d_union <- oridata |>
    filter(age >= minage, everbirth == 0) |>
    group_by(country, sex, bc_cate, edu2) |>
    summarise(
      never_in_union_n = sum(weight[everunion == 0], na.rm = TRUE),
      .groups = "drop"
    )

  d <- d_total |>
    left_join(d_childless, by = c("country", "sex", "bc_cate", "edu2")) |>
    left_join(d_union, by = c("country", "sex", "bc_cate", "edu2"))

  d_alledu <- d |>
    filter(edu2 %in% c("High", "Low")) |>
    group_by(country, sex, bc_cate) |>
    summarise(
      total_n = sum(total_n, na.rm = TRUE),
      childless_n = sum(childless_n, na.rm = TRUE),
      never_in_union_n = sum(never_in_union_n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(edu2 = "All")

  d |> bind_rows(d_alledu)
}

# -------------------------------------------------------------------------
# 2. Ingest, Clean, and Aggregate Data ----
# -------------------------------------------------------------------------
# Bypass slow reading and processing if aggregate already exists.
# Delete the aggregate file if you wish to re-pull from source raw data.
if (!file.exists(aggregate_path)) {
  message("Aggregate file not found. Rebuilding from raw source data...")

  # --- 2a. ESS ---
  message("Processing ESS data...")
  ess3 <- foreign::read.spss(
    path_ess3,
    to.data.frame = TRUE,
    use.value.labels = FALSE
  )
  ess9 <- foreign::read.spss(
    path_ess9,
    to.data.frame = TRUE,
    use.value.labels = FALSE
  )

  d_ess <- bind_rows(
    ess3 |>
      select(
        idno,
        cntry,
        agea,
        pspwght,
        pweight,
        evlvptn,
        gndr,
        yrbrn,
        edulvla,
        bthcld,
        eduyrs
      ) |>
      mutate(dataset = "ESS3"),
    ess9 |>
      select(
        idno,
        cntry,
        agea,
        pspwght,
        pweight,
        evlvptn,
        gndr,
        yrbrn,
        edulvla = edulvlb,
        bthcld,
        eduyrs
      ) |>
      mutate(dataset = "ESS9")
  ) |>
    mutate(
      agea = as.numeric(as.character(agea)),
      sex = ifelse(gndr == 1, "Men", "Women"),
      bc_cate = case_when(
        yrbrn %in% 1905:1909 ~ "1905-1909",
        yrbrn %in% 1910:1919 ~ "1910-1919",
        yrbrn %in% 1920:1929 ~ "1920-1929",
        yrbrn %in% 1930:1939 ~ "1930-1939",
        yrbrn %in% 1940:1949 ~ "1940-1949",
        yrbrn %in% 1950:1959 ~ "1950-1959",
        yrbrn %in% 1960:1969 ~ "1960-1969",
        yrbrn %in% 1970:1979 ~ "1970-1979",
        yrbrn %in% 1980:1989 ~ "1980-1989",
        yrbrn %in% 1990:1999 ~ "1990-1999",
        yrbrn %in% 2000:2009 ~ "2000-2009"
      ),
      country = countrycode::countrycode(
        cntry,
        origin = "genc2c",
        destination = "country.name"
      ),
      country = ifelse(country == "United Kingdom", "The UK", country),
      everbirth = ifelse(bthcld == 1, 1, 0),
      everunion = ifelse(evlvptn == 1, 1, 0),
      edulvla2 = case_when(
        edulvla %in% c(0, 1, 113, 129) ~ 1,
        edulvla %in% c(2, 212, 213, 221, 222, 223, 229) ~ 2,
        edulvla %in% c(3, 311, 312, 313, 321, 322, 323) ~ 3,
        edulvla %in% c(4, 412, 413, 421, 422, 423) ~ 4,
        edulvla %in% c(5, 510, 520, 610, 620, 710, 720, 800) ~ 5,
        edulvla %in% c(55, 555) ~ 0
      ),
      education = case_when(
        edulvla2 %in% c(0, 1, 2) ~ "Low",
        edulvla2 %in% c(3, 4) ~ "Medium",
        edulvla2 == 5 ~ "High"
      ),
      anweight = pspwght * pweight,
      datasetname = "ESS",
      id = paste(idno, idno, "-")
    ) |>
    group_by(country, sex, bc_cate) |>
    mutate(
      mean_eduy = mean(eduyrs, na.rm = TRUE),
      edu2 = ifelse(eduyrs >= mean_eduy, "High", "Low")
    ) |>
    ungroup() |>
    select(
      id,
      country,
      sex,
      age = agea,
      bc_cate,
      birthyear = yrbrn,
      education,
      edu2,
      everbirth,
      everunion,
      dataset,
      datasetname,
      weight = anweight
    )

  # --- 2b. Harmonized Histories ---
  message("Processing HH data...")
  hh0 <- readstata13::read.dta13(
    path_hh0,
    nonint.factors = TRUE,
    convert.factors = TRUE
  )
  hh1 <- readstata13::read.dta13(
    path_hh1,
    nonint.factors = TRUE,
    convert.factors = TRUE
  )
  hh2 <- readstata13::read.dta13(
    path_hh2,
    nonint.factors = TRUE,
    convert.factors = TRUE
  )

  hh_vars <- c(
    "IMONTH_S",
    "IBORN_M",
    "BORN_Y",
    "SEX",
    "YEAR_S",
    "COUNTRY",
    paste0("KID_", 1:16),
    paste0("UNION_", 1:9),
    "RESPID",
    "EDU_3",
    "IEDU_Y",
    "PERSWGT"
  )

  check_child <- function(x) {
    ifelse(is.na(x), 0, as.integer(grepl("Child of order", x)))
  }
  check_union <- function(x) {
    ifelse(is.na(x), 0, as.integer(grepl("Union of order", x)))
  }

  d_hh <- bind_rows(
    hh0 |> select(all_of(hh_vars)),
    hh1 |> select(all_of(hh_vars)),
    hh2 |> select(all_of(hh_vars))
  ) |>
    mutate(
      across(
        c(IMONTH_S, IBORN_M),
        ~ case_when(
          .x == "January" ~ 1,
          .x == "February" ~ 2,
          .x == "March" ~ 3,
          .x == "April" ~ 4,
          .x == "May" ~ 5,
          .x == "June" ~ 6,
          .x == "July" ~ 7,
          .x == "August" ~ 8,
          .x == "September" ~ 9,
          .x == "October" ~ 10,
          .x == "November" ~ 11,
          .x == "December" ~ 12
        )
      ),
      bc_cate = case_when(
        BORN_Y %in% 1908:1909 ~ "1908-1909",
        BORN_Y %in% 1910:1919 ~ "1910-1919",
        BORN_Y %in% 1920:1929 ~ "1920-1929",
        BORN_Y %in% 1930:1939 ~ "1930-1939",
        BORN_Y %in% 1940:1949 ~ "1940-1949",
        BORN_Y %in% 1950:1959 ~ "1950-1959",
        BORN_Y %in% 1960:1969 ~ "1960-1969",
        BORN_Y %in% 1970:1979 ~ "1970-1979",
        BORN_Y %in% 1980:1989 ~ "1980-1989",
        BORN_Y %in% 1990:1999 ~ "1990-1999",
        BORN_Y %in% 2000:2001 ~ "2000-2001"
      ),
      SEX = ifelse(SEX == "Female", "Women", "Men"),
      int = ((YEAR_S - 1900) * 12) + IMONTH_S,
      birth = ((BORN_Y - 1900) * 12) + IBORN_M,
      age = trunc((int - birth) / 12, 1),
      country = case_when(
        COUNTRY == "Austria GGS wave1" ~ "Austria",
        COUNTRY == "Belgium GGS wave1" ~ "Belgium",
        COUNTRY == "Bulgaria GGS wave1" ~ "Bulgaria",
        COUNTRY == "Belarus GGS wave 1" ~ "Belarus",
        COUNTRY %in% c("Canada GSS 2006", "Canada GSS 2011") ~ "Canada",
        COUNTRY %in%
          c(
            "Czech Republic GGS wave 1",
            "2032. Czech Republic GGSII wave1"
          ) ~ "Czechia",
        COUNTRY == "2081. Denmark GGSII wave1" ~ "Denmark",
        COUNTRY %in%
          c("Estonia GGS wave1", "2332. Estonia GGSII wave1") ~ "Estonia",
        COUNTRY == "France GGS wave1" ~ "France",
        COUNTRY == "Georgia GGS wave1" ~ "Georgia",
        COUNTRY %in% c("Germany GGS wave1", "Germany Pairfam") ~ "Germany",
        COUNTRY == "Hungary GGS wave1" ~ "Hungary",
        COUNTRY %in% c("Italy GGS wave1", "3802. Italy Fss 2016") ~ "Italy",
        COUNTRY == "Kazakhstan GGS 2018" ~ "Kazakhstan",
        COUNTRY == "Lithuania GGS wave1" ~ "Lithuania",
        COUNTRY == "Moldova GGS wave1" ~ "Moldova",
        COUNTRY %in%
          c("Netherlands FFS", "Netherlands OG 2013") ~ "Netherlands",
        COUNTRY %in%
          c("Norway GGS wave1", "5782. Norway GGSII wave1") ~ "Norway",
        COUNTRY == "Poland GGS wave1" ~ "Poland",
        COUNTRY == "Romania GGS wave1" ~ "Romania",
        COUNTRY == "Russia GGS wave1" ~ "Russia",
        COUNTRY %in% c("Spain SFS 2006", "Spain SFS 2018") ~ "Spain",
        COUNTRY == "Sweden GGS wave 1" ~ "Sweden",
        COUNTRY == "UK BHPS" ~ "The UK",
        COUNTRY %in% c("USA NSFG 1995", "USA NSFG 2007") ~ "The US",
        COUNTRY == "Uruguay ENCoR 2015" ~ "Uruguay"
      ),
      across(starts_with("KID_"), ~ check_child(.x)),
      across(starts_with("UNION_"), ~ check_union(.x))
    ) |>
    mutate(
      num_child = rowSums(pick(starts_with("KID_")), na.rm = TRUE),
      everbirth = ifelse(num_child > 0, 1, 0),
      num_union = rowSums(pick(starts_with("UNION_")), na.rm = TRUE),
      everunion = ifelse(num_union > 0, 1, 0),
      datasetname = "HH",
      RESPID = as.character(RESPID)
    ) |>
    group_by(country, SEX, bc_cate) |>
    mutate(
      median_eduy = median(IEDU_Y, na.rm = TRUE),
      edu2 = ifelse(IEDU_Y >= median_eduy, "High", "Low")
    ) |>
    ungroup() |>
    select(
      id = RESPID,
      country,
      sex = SEX,
      age,
      bc_cate,
      birthyear = BORN_Y,
      education = EDU_3,
      everbirth,
      everunion,
      dataset = COUNTRY,
      datasetname,
      edu2,
      weight = PERSWGT
    )

  # --- 2c. DHS ---
  message("Processing DHS data...")
  dhs <- readstata13::read.dta13(
    path_dhs,
    nonint.factors = TRUE,
    convert.factors = TRUE
  )
  d_dhs <- dhs |>
    select(-education) |>
    rename(
      id = caseid,
      country = country_name,
      birthyear = v010,
      age = v012,
      education = pc50,
      bc_cate = cohort
    ) |>
    group_by(country) |>
    filter(syear == max(syear)) |>
    mutate(
      sex = ifelse(gender == 1, "Women", "Men"),
      bc_cate = case_when(
        birthyear %in% 1910:1919 ~ "1910-1919",
        birthyear %in% 1920:1929 ~ "1920-1929",
        birthyear %in% 1930:1939 ~ "1930-1939",
        birthyear %in% 1940:1949 ~ "1940-1949",
        birthyear %in% 1950:1959 ~ "1950-1959",
        birthyear %in% 1960:1969 ~ "1960-1969",
        birthyear %in% 1970:1979 ~ "1970-1979",
        birthyear %in% 1980:1989 ~ "1980-1989",
        birthyear %in% 1990:1999 ~ "1990-1999",
        birthyear %in% 2000:2004 ~ "2000-2009"
      ),
      education = ifelse(education == 1, "High", "Low"),
      education = factor(education, levels = c("Low", "High")),
      dataset = paste0("DHS", country_survey),
      datasetname = "DHS"
    ) |>
    group_by(country, sex, bc_cate) |>
    mutate(
      mean_eduy = mean(yearss, na.rm = TRUE),
      edu2 = ifelse(yearss >= mean_eduy, "High", "Low")
    ) |>
    ungroup() |>
    select(
      id,
      country,
      sex,
      age,
      bc_cate,
      birthyear,
      education,
      everbirth,
      everunion,
      dataset,
      datasetname,
      weight = weigr,
      edu2
    )

  # --- 2d. Merge & Aggregate ---
  message("Merging and aggregating data...")
  d_all <- bind_rows(d_hh, d_dhs, d_ess)
  d_aggregate <- func_makedata2(d_all, minage = 35) |>
    mutate(edu2 = factor(edu2, levels = c("All", "Low", "High"))) |>
    janitor::clean_names()

  if (!dir.exists("out")) {
    dir.create("out")
  }
  write_csv(d_aggregate, aggregate_path)
  message("Data aggregated successfully.")
} else {
  message("Found existing aggregate file. Skipping deep raw reprocessing.")
}

# -------------------------------------------------------------------------
# 3. Final Preparation for Models & Visualizations -----
# -------------------------------------------------------------------------
raw_agg <- read_csv(aggregate_path, show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(
    country = country |>
      str_replace_all("The US", "United States") |>
      str_replace_all("The UK", "United Kingdom"),
    iso3 = countrycode::countrycode(
      country,
      origin = "country.name",
      destination = "iso3c"
    )
  )

# UN HDI / GII data
un <- readstata13::read.dta13(path_un)
d_un <- un |>
  filter(bc_cate %in% c("1960-1969", "1970-1979")) |>
  group_by(country) |>
  summarise(gii = mean(gii_, na.rm = TRUE), .groups = "drop") |>
  mutate(
    iso3 = countrycode::countrycode(
      country,
      origin = "country.name",
      destination = "iso3c"
    )
  )

# Gapminder regions
gap <- gapminder::gapminder_unfiltered |>
  janitor::clean_names() |>
  distinct(country, continent) |>
  transmute(
    iso3 = countrycode::countrycode(
      country,
      origin = "country.name",
      destination = "iso3c"
    ),
    continent = as.character(continent)
  ) |>
  drop_na() |>
  add_row(iso3 = "KGZ", continent = "FSU") |>
  mutate(
    name = countrycode::countrycode(
      iso3,
      origin = "iso3c",
      destination = "country.name.en"
    ),
    continent = case_when(
      continent == "Europe" ~ "Europe & North America",
      continent == "Americas" ~ "Latin America",
      TRUE ~ continent
    )
  ) |>
  mutate(
    continent = case_when(
      iso3 %in% c("AZE", "MDA") ~ "FSU",
      iso3 %in% c("CAN", "USA") ~ "Europe & North America",
      TRUE ~ continent
    )
  )

# Primary outcome format
never <- raw_agg |>
  drop_na(sex) |>
  replace_na(list(total_n = 0, childless_n = 0, never_in_union_n = 0)) |>
  filter(
    bc_cate %in% c("1960-1969", "1970-1979"),
    edu2 == "All",
    total_n >= 50
  ) |>
  group_by(iso3, sex, edu2) |>
  summarise(
    n_total = sum(total_n, na.rm = TRUE),
    n_childless = sum(childless_n, na.rm = TRUE),
    n_niu = sum(never_in_union_n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(iso3) |>
  filter(n() == 2) |>
  ungroup() |>
  left_join(gap, by = "iso3") |>
  mutate(
    p_childless = n_childless / n_total * 100,
    p_niu = n_niu / n_childless * 100
  ) |>
  left_join(d_un, by = "iso3") |>
  filter(!is.na(gii)) |>
  # group_by(continent) |>
  # mutate(cont_gii = mean(gii, na.rm = TRUE)) |>
  # ungroup() |>
  arrange(
    # cont_gii, 
    gii
  ) |>
  mutate(
    country = fct_rev(fct_inorder(as_factor(country))),
    continent = fct_rev(fct_inorder(as_factor(continent)))
  )

save(never, file = "out/never.rda")

# Education outcome format
never_edu2 <- raw_agg |>
  drop_na(sex) |>
  replace_na(list(total_n = 0, childless_n = 0, never_in_union_n = 0)) |>
  filter(
    bc_cate %in% c("1960-1969", "1970-1979"),
    edu2 %in% c("Low", "High"),
    total_n >= 30
  ) |>
  group_by(iso3, sex, edu2) |>
  summarise(
    n_total = sum(total_n, na.rm = TRUE),
    n_childless = sum(childless_n, na.rm = TRUE),
    n_niu = sum(never_in_union_n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(iso3) |>
  filter(n() == 4) |>
  ungroup() |>
  left_join(gap, by = "iso3") |>
  mutate(
    p_childless = n_childless / n_total * 100,
    p_niu = n_niu / n_childless * 100
  ) |>
  left_join(d_un, by = "iso3") |>
  filter(!is.na(gii)) |>
  group_by(continent) |>
  mutate(cont_gii = mean(gii, na.rm = TRUE)) |>
  ungroup() |>
  arrange(cont_gii, gii) |>
  mutate(
    country = fct_rev(fct_inorder(as_factor(country))),
    continent = fct_rev(fct_inorder(as_factor(continent)))
  )

save(never_edu2, file = "out/never_edu2.rda")

# 4. Geodata Mapping Precomputation
world_outline <- spData::world |>
  sf::st_as_sf() |>
  rmapshaper::ms_simplify(0.25)

world_outline_robinson <- world_outline |>
  filter(iso_a2 != "AQ") |>
  sf::st_transform(crs = "ESRI:54030") |>
  mutate(
    iso3 = countrycode::countrycode(
      name_long,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c("Kosovo" = "XKX")
    )
  ) |>
  select(-continent) |>
  left_join(never, by = "iso3")

country_borders <- world_outline |>
  rmapshaper::ms_innerlines() |>
  sf::st_transform(crs = "ESRI:54030")

save(world_outline_robinson, country_borders, file = "out/geodata.rda")
