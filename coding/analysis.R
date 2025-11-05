library(dplyr)
library(fixest)
library(lubridate)      


#read data
df_join <- read_csv("data/CRSPM_JOIN.csv")
rf_raw  <- read_csv("data/risk_free.csv")


#filter data whose summary period is AQ
df_join<- df_join%>%
  filter(SUMMARY_PERIOD2 == "AQ")
# Compute annual rate_of_return (year-on-year) in df_join

df_join <- df_join %>%
  arrange(CRSP_FUNDNO, NAV_LATEST_DT) %>%
  group_by(CRSP_FUNDNO) %>%
  mutate(
    lag_NAV = lag(NAV_LATEST),
    lag_date = lag(NAV_LATEST_DT),
    days_diff = as.numeric(difftime(NAV_LATEST_DT, lag_date, units = "days")),
    lag_NAV_year = if_else(!is.na(days_diff) & days_diff >= 335 & days_diff <= 395, lag_NAV, NA_real_),
    rate_of_return = if_else(!is.na(lag_NAV_year), (NAV_LATEST - lag_NAV_year) / lag_NAV_year, NA_real_)
  ) %>%
  ungroup()

# Remove extreme outliers in rate_of_return (0.5% and 99.5% quantiles), plot the rate of return to see outliers
if (sum(!is.na(df_join$rate_of_return)) > 0) {
  lower_bound <- quantile(df_join$rate_of_return, 0.005, na.rm = TRUE)
  upper_bound <- quantile(df_join$rate_of_return, 0.995, na.rm = TRUE)
  df_join <- df_join %>% filter(rate_of_return > lower_bound & rate_of_return < upper_bound)
}

# Creating dummy variable for recession year according to the FED 

df_join <- df_join %>%
  mutate(year = year(NAV_LATEST_DT),
         crisis = if_else(year %in% c(2001, 2008, 2009), 1L, 0L))


# Risk-free / factor data: converting monthly into yearly estimates

rf_clean <- rf_raw %>%
  
  mutate(
    date = as_date(`Date..SAS...Last.Trading.Day.of.the.Month`, format = "%d-%m-%Y"),
    year = year(date),
    excess_ret = `Excess.Return.on.the.Market`,
    smb = `Small.Minus.Big.Return`,
    hml = `High.Minus.Low.Return`,
    rf_rate = `Risk.Free.Return.Rate..One.Month.Treasury.Bill.Rate.`,
    momentum = `Momentum.Factor`
  ) %>%
  group_by(year) %>%
  summarise(
    avg_excess_ret = mean(excess_ret, na.rm = TRUE),
    avg_smb = mean(smb, na.rm = TRUE),
    avg_hml = mean(hml, na.rm = TRUE),
    avg_rf = mean(rf_rate, na.rm = TRUE),
    avg_momentum = mean(momentum, na.rm = TRUE),
    .groups = "drop"
  )

# Merge RF factors into df_join by year
df_join <- df_join %>% left_join(rf_clean, by = "year")


# Fund-level SD and Sharpe ratio

fund_summary <- df_join %>%
  group_by(CRSP_FUNDNO) %>%
  summarise(
    sd_return_fund = sd(rate_of_return, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  )

df_join <- df_join %>% left_join(fund_summary, by = "CRSP_FUNDNO")

df_join <- df_join %>%
  mutate(sharpe_ratio = (rate_of_return - avg_rf) / sd_return_fund)


# 11. Build two analysis tables:
#     table_experience (uses 'experience') and table_tenure (uses 'tenure')

table_experience <- df_join %>%
  select(rate_of_return, CRSP_FUNDNO, year, sharpe_ratio, experience,
         GROUP, EXP_RATIO, TNA_LATEST, crisis,
         avg_excess_ret, avg_smb, avg_hml, avg_rf, avg_momentum, sd_return_fund) %>%
  mutate(experience = if_else(experience >= 0, experience, NA_real_)) %>%
  drop_na(experience)   # remove rows without experience

# If tenure is present in df_join:
table_tenure <- df_join %>%
  select(rate_of_return, CRSP_FUNDNO, year, sharpe_ratio, tenure,
         GROUP, EXP_RATIO, TNA_LATEST, crisis,
         avg_excess_ret, avg_smb, avg_hml, avg_rf, avg_momentum, sd_return_fund) %>%
  mutate(tenure = if_else(is.numeric(tenure) & tenure >= 0, tenure, NA_real_)) %>%
  drop_na(tenure)

# ---------------------------
# 12. CHECK & RESOLVE duplicates (CRSP_FUNDNO, year)
#     This resolves the "duplicate time values within id" error from plm()
# ---------------------------
resolve_duplicates <- function(tbl, id_col = "CRSP_FUNDNO", time_col = "year") {
  dups <- tbl %>%
    group_by(across(all_of(c(id_col, time_col)))) %>%
    filter(n() > 1) %>%
    ungroup()
  if (nrow(dups) > 0) {
    message(sprintf("Found %d duplicate rows for (%s, %s). Aggregating numeric cols by mean.", nrow(dups), id_col, time_col))
    # Aggregation rule:
    # - numeric columns -> mean
    # - non-numeric -> first non-NA
    tbl_clean <- tbl %>%
      group_by(across(all_of(c(id_col, time_col)))) %>%
      summarise(
        across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
        across(where(~ !is.numeric(.x)), ~ first(na.omit(.x))),
        .groups = "drop"
      )
    return(tbl_clean)
  } else {
    message("No duplicate (id, time) pairs found.")
    return(tbl)
  }
}

table_experience <- resolve_duplicates(table_experience)
table_tenure <- resolve_duplicates(table_tenure)


# Jensen's alpha (per fund) using fixest (1-factor and 4-factor) on table_experience dataset

# # Prepare excess return and market factor
# table_experience <- table_experience %>%
#   mutate(excess_return = rate_of_return - avg_rf,
#          market_excess_return = avg_excess_ret)

# # One-factor Jensen alpha (fund fixed effects)
# model_alpha_1 <- feols(excess_return ~ market_excess_return | CRSP_FUNDNO,
#                        data = table_experience, cluster = "CRSP_FUNDNO")
# jensen_1 <- fixef(model_alpha_1, effect = "CRSP_FUNDNO")[[1]]
# jensen_1_df <- tibble(CRSP_FUNDNO = as.integer(names(jensen_1)),
#                       jensen_alpha_one_factor = as.numeric(jensen_1))

# table_experience <- table_experience %>% left_join(jensen_1_df, by = "CRSP_FUNDNO")

# # Four-factor Jensen alpha
# model_alpha_4 <- feols(excess_return ~ market_excess_return + avg_smb + avg_hml + avg_momentum | CRSP_FUNDNO,
#                        data = table_experience, cluster = "CRSP_FUNDNO")
# jensen_4 <- fixef(model_alpha_4, effect = "CRSP_FUNDNO")[[1]]
# jensen_4_df <- tibble(CRSP_FUNDNO = as.integer(names(jensen_4)),
#                       jensen_alpha_four_factor = as.numeric(jensen_4))

# table_experience <- table_experience %>% left_join(jensen_4_df, by = "CRSP_FUNDNO")

table_experience <- table_experience %>%
  mutate(
    excess_return = rate_of_return - avg_rf  
  )

#  Create market excess return (R_m - R_f)
table_experience <- table_experience %>%
  mutate(
    market_excess_return = avg_excess_ret
  )


#Jensen's alpha 1 -1-factor
# Run panel regression with fund and year fixed effects
model_alpha <- feols(
  excess_return ~ market_excess_return | CRSP_FUNDNO,
  data = table_experience,
  cluster = "CRSP_FUNDNO"
)

jensen_alpha <- fixef(model_alpha, effect = "CRSP_FUNDNO")[[1]]

jensen_alpha_df <- data.frame(
  CRSP_FUNDNO = names(jensen_alpha),
  jensen_alpha_one_factor = as.numeric(jensen_alpha)
)
jensen_alpha_df$CRSP_FUNDNO <- as.integer(jensen_alpha_df$CRSP_FUNDNO)


table_experience <- table_experience %>%
  left_join(jensen_alpha_df, by = "CRSP_FUNDNO")


#Jensens 4 4-factor alpha

model_alpha <- feols(
  excess_return ~ market_excess_return + avg_smb + avg_hml+ avg_momentum | CRSP_FUNDNO,
  data = table_experience,
  cluster = "CRSP_FUNDNO"
)

jensen_alpha <- fixef(model_alpha, effect = "CRSP_FUNDNO")[[1]]

jensen_alpha_df <- data.frame(
  CRSP_FUNDNO = names(jensen_alpha),
  jensen_alpha_four_factor = as.numeric(jensen_alpha)
)
jensen_alpha_df$CRSP_FUNDNO <- as.integer(jensen_alpha_df$CRSP_FUNDNO)


library(dplyr)

table_experience <- table_experience %>%
  left_join(jensen_alpha_df, by = "CRSP_FUNDNO")




# Jensen's alpha (per fund) using fixest (1-factor and 4-factor) on table_tenure dataset

table_tenure <- table_tenure %>%
  mutate(
    excess_return = rate_of_return - avg_rf  
  )

#  Create market excess return (R_m - R_f)
table_tenure <- table_tenure %>%
  mutate(
    market_excess_return = avg_excess_ret
  )

# Jensen 1-factor alpha
model_alpha <- feols(
  excess_return ~ market_excess_return | CRSP_FUNDNO,
  data = table_tenure,
  cluster = "CRSP_FUNDNO"
)

jensen_alpha <- fixef(model_alpha, effect = "CRSP_FUNDNO")[[1]]

jensen_alpha_df <- data.frame(
  CRSP_FUNDNO = names(jensen_alpha),
  jensen_alpha_one_factor = as.numeric(jensen_alpha)
)
jensen_alpha_df$CRSP_FUNDNO <- as.integer(jensen_alpha_df$CRSP_FUNDNO)


library(dplyr)

table_tenure <- table_tenure %>%
  left_join(jensen_alpha_df, by = "CRSP_FUNDNO")


#Jensens 4 4-factor alpha

model_alpha <- feols(
  excess_return ~ market_excess_return + avg_smb + avg_hml+ avg_momentum | CRSP_FUNDNO,
  data = table_tenure,
  cluster = "CRSP_FUNDNO"
)

jensen_alpha <- fixef(model_alpha, effect = "CRSP_FUNDNO")[[1]]

jensen_alpha_df <- data.frame(
  CRSP_FUNDNO = names(jensen_alpha),
  jensen_alpha_four_factor = as.numeric(jensen_alpha)
)
jensen_alpha_df$CRSP_FUNDNO <- as.integer(jensen_alpha_df$CRSP_FUNDNO)




table_tenure <- table_tenure %>%
  left_join(jensen_alpha_df, by = "CRSP_FUNDNO")

'

# 14. Regressions (main analysis)



# -- A: Experience regressions (recommended: fixest)
#1. Rate of return ~ experience (fund + year FE), clustered by fund
model_feols_basic <- feols(rate_of_return ~ experience + I(experience^2) + experience:crisis | CRSP_FUNDNO + year,
                           data = table_experience, cluster = "CRSP_FUNDNO")
print(summary(model_feols_basic))

# 2. With controls (expense ratio, fund size)
model_feols_ctrl <- feols(rate_of_return ~ experience + I(experience^2) + EXP_RATIO + TNA_LATEST + experience:crisis | CRSP_FUNDNO + year,
                          data = table_experience, cluster = "CRSP_FUNDNO")
print(summary(model_feols_ctrl))

# 3. Sharpe ratio model
model_sharpe <- feols(sharpe_ratio ~ experience + I(experience^2) + EXP_RATIO + TNA_LATEST + experience:crisis | CRSP_FUNDNO + year,
                      data = table_experience, cluster = "CRSP_FUNDNO")
print(summary(model_sharpe))

# 4. Jensen alpha regressions (year FE, cluster by fund)
model_jensen1 <- feols(jensen_alpha_one_factor ~ experience + I(experience^2) + EXP_RATIO + TNA_LATEST + experience:crisis | year,
                       data = table_experience, cluster = "CRSP_FUNDNO")
print(summary(model_jensen1))

model_jensen4 <- feols(jensen_alpha_four_factor ~ experience + I(experience^2) + EXP_RATIO + TNA_LATEST + experience:crisis | year,
                       data = table_experience, cluster = "CRSP_FUNDNO")
print(summary(model_jensen4))

# -- B: Tenure regressions (if table_tenure has data)
if (nrow(table_tenure) > 0) {
  model_tenure_basic <- feols(rate_of_return ~ tenure + I(tenure^2) + tenure:crisis | CRSP_FUNDNO + year,
                              data = table_tenure, cluster = "CRSP_FUNDNO")
  print(summary(model_tenure_basic))
}

# -- C: Classical plm() example (requires unique id-year pairs -- we aggregated earlier)
pdata_exp <- pdata.frame(table_experience %>% mutate(year = as.integer(year)),
                         index = c("CRSP_FUNDNO", "year"))
plm_model <- plm(rate_of_return ~ experience + experience:crisis + GROUP,
                 data = pdata_exp, model = "within", effect = "twoways")
vcov_plm <- vcovHC(plm_model, method = "arellano", type = "HC1", cluster = "group")
print(coeftest(plm_model, vcov_plm))

# ---------------------------
# 15. Diagnostics & quick summaries
# ---------------------------
# Basic summary statistics
message("Basic counts:")
message("Number of funds (table_experience): ", n_distinct(table_experience$CRSP_FUNDNO))
message("Observations (table_experience): ", nrow(table_experience))

# Distribution of experience
if ("experience" %in% colnames(table_experience)) {
  print(summary(table_experience$experience))
}

# ---------------------------
# 16. (Optional) Save cleaned outputs locally
# ---------------------------
# Uncomment to save (local only; do not push proprietary outputs to public repos)
# dir.create("output", showWarnings = FALSE)
# write_rds(table_experience, "output/table_experience_clean.rds")
# write_csv(table_experience, "output/table_experience_clean.csv")

###############################################################################
# End of script
###############################################################################
