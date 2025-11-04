###############################################################################
# data_cleaning_crspm.R
#
# Cleans and processes raw CRSPM dataset up to creation of CRSPM_JOIN.csv
###############################################################################

library(tidyverse)
library(lubridate)
library(haven)

# ---------------------------
# 1. File paths
# ---------------------------
paths <- list(
  crspm_sas = "data/crspm.sas7bdat",    # raw CRSPM SAS dataset
  crspm_join = "data/CRSPM_JOIN.csv"    # output file
)

# ---------------------------
# 2. Helper functions
# ---------------------------
normalize_name <- function(name) {
  name %>%
    toupper() %>%
    str_remove_all("[.,]") %>%
    str_squish() %>%
    str_trim()
}

exclude_terms <- c(
  "TEAM", "MGMT", "MANAGEMENT", "COMMITTEE", "MANAGED", "STEERING",
  "GROUP", "TASKFORCE", "PORTFOLIO", "POLICY", "COUNSELORS",
  "INC", "INCOME", "FUND", "LIFE"
)

compute_join_dt <- function(mgr_name, earliest_tbl) {
  if (is.na(mgr_name) || str_trim(mgr_name) == "") return(NA_Date_)
  parts <- str_split(normalize_name(mgr_name), "/", simplify = FALSE)[[1]] %>% str_trim()
  parts <- parts[parts != ""]
  eds <- earliest_tbl %>% filter(MGR_NAME_NORM %in% parts)
  if (nrow(eds) == 0) return(NA_Date_)
  as_date(min(eds$EARLIEST_DT, na.rm = TRUE))
}

# ---------------------------
# 3. Read raw data
# ---------------------------
df_raw <- read_sas(paths$crspm_sas)

# ---------------------------
# 4. Normalize manager names and compute earliest manager dates
# ---------------------------
df_norm <- df_raw %>%
  mutate(MGR_NAME_NORM = normalize_name(MGR_NAME)) %>%
  separate_rows(MGR_NAME_NORM, sep = "/") %>%
  mutate(MGR_NAME_NORM = str_trim(MGR_NAME_NORM)) %>%
  filter(MGR_NAME_NORM != "") %>%
  mutate(IS_TEAM = str_detect(MGR_NAME_NORM,
                              regex(paste0("\\b(", paste(exclude_terms, collapse = "|"), ")\\b"))))

earliest_dates <- df_norm %>%
  filter(!IS_TEAM) %>%
  group_by(MGR_NAME_NORM) %>%
  summarise(EARLIEST_DT = min(MGR_DT, na.rm = TRUE), .groups = "drop") %>%
  mutate(EARLIEST_DT = as_date(EARLIEST_DT))

# ---------------------------
# 5. Compute JOIN_DT and GROUP flag
# ---------------------------
df_raw <- df_raw %>%
  mutate(
    JOIN_DT = map_chr(MGR_NAME, ~ as.character(compute_join_dt(.x, earliest_dates))),
    JOIN_DT = as_date(JOIN_DT),
    JOIN_NULL = if_else(is.na(JOIN_DT), 1L, 0L),
    GROUP = as.integer(str_detect(
      toupper(str_remove_all(str_squish(MGR_NAME), "[.,]")),
      regex(paste0("\\b(", paste(exclude_terms, collapse = "|"), ")\\b"))
    ))
  )

# ---------------------------
# 6. Compute experience and tenure
# ---------------------------
df_raw <- df_raw %>%
  mutate(
    NAV_LATEST_DT = as_date(NAV_LATEST_DT),
    MGR_DT = as_date(MGR_DT),
    JOIN_DT = as_date(JOIN_DT),
    experience = if_else(
      !is.na(JOIN_DT) & !is.na(NAV_LATEST_DT),
      as.numeric(difftime(NAV_LATEST_DT, JOIN_DT, units = "days")) / 365.25,
      NA_real_
    ),
    tenure = if_else(
      !is.na(NAV_LATEST_DT) & !is.na(MGR_DT),
      as.numeric(difftime(NAV_LATEST_DT, MGR_DT, units = "days")) / 365.25,
      NA_real_
    )
  )

# ---------------------------
# 7. Save cleaned version
# ---------------------------
message("✅ Saving cleaned dataset as data/CRSPM_JOIN.csv ...")
write_csv(df_raw, paths$crspm_join)
message("✅ CRSPM_JOIN.csv saved successfully!")

###############################################################################
# End of Script 1
###############################################################################

