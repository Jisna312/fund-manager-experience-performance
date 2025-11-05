library(dplyr)
library(fixest)
library(lubridate) 
library(ggplot2)
install.packages("lmtest")



#Define path
paths <- list(
  crspm_join = "data/CRSPM_JOIN.csv",
  risk_free = "data/risk_free.csv",
  models = "output/models/",
  tables = "output/tables/",
  figures = "output/figures/"
)
# read data
df_join <- read_csv(paths$crspm_join)
rf_raw  <- read_csv(paths$risk_free)
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

df_analysis<-df_join  #for diagnostic checks

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


#  Build two analysis tables:
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



# Jensen's alpha (per fund) using fixest (1-factor and 4-factor) on table_experience dataset


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


#Jensens 4-factor alpha

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

#  Regressions (main analysis)

# Helper function to save model summaries
save_model_summary <- function(model, filename) {
  sink(file.path(paths$models, filename))
  print(summary(model))
  sink()
}


# A: Experience regressions 


# 1. Rate of return ~ experience (fund + year FE)
model_feols_basic <- feols(rate_of_return ~ experience + I(experience^2) + experience:crisis | CRSP_FUNDNO + year,
                           data = table_experience, cluster = "CRSP_FUNDNO")
save_model_summary(model_feols_basic, "experience_rate_basic.txt")

# 2. With controls
model_feols_ctrl <- feols(rate_of_return ~ experience + I(experience^2) + EXP_RATIO + TNA_LATEST + experience:crisis | CRSP_FUNDNO + year,
                          data = table_experience, cluster = "CRSP_FUNDNO")
save_model_summary(model_feols_ctrl, "experience_rate_controls.txt")

# 3. Sharpe ratio model
model_sharpe <- feols(sharpe_ratio ~ experience + I(experience^2) + EXP_RATIO + TNA_LATEST + experience:crisis | CRSP_FUNDNO + year,
                      data = table_experience, cluster = "CRSP_FUNDNO")
save_model_summary(model_sharpe, "experience_sharpe.txt")

# 4. Jensen alpha regressions (year FE)
model_jensen1 <- feols(jensen_alpha_one_factor ~ experience + I(experience^2) + EXP_RATIO + TNA_LATEST + experience:crisis | year,
                       data = table_experience, cluster = "CRSP_FUNDNO")
save_model_summary(model_jensen1, "experience_jensen1.txt")

model_jensen4 <- feols(jensen_alpha_four_factor ~ experience + I(experience^2) + EXP_RATIO + TNA_LATEST + experience:crisis | year,
                       data = table_experience, cluster = "CRSP_FUNDNO")
save_model_summary(model_jensen4, "experience_jensen4.txt")


# B: Tenure regressions 


# 1. Rate of return ~ tenure (fund + year FE)
model_feols_basic_tenure <- feols(rate_of_return ~ tenure + I(tenure^2) + tenure:crisis | CRSP_FUNDNO + year,
                                  data = table_tenure, cluster = "CRSP_FUNDNO")
save_model_summary(model_feols_basic_tenure, "tenure_rate_basic.txt")

# 2. With controls
model_feols_ctrl_tenure <- feols(rate_of_return ~ tenure + I(tenure^2) + EXP_RATIO + TNA_LATEST + tenure:crisis | CRSP_FUNDNO + year,
                                 data = table_tenure, cluster = "CRSP_FUNDNO")
save_model_summary(model_feols_ctrl_tenure, "tenure_rate_controls.txt")

# 3. Sharpe ratio model
model_sharpe_tenure <- feols(sharpe_ratio ~ tenure + I(tenure^2) + EXP_RATIO + TNA_LATEST + tenure:crisis | CRSP_FUNDNO + year,
                             data = table_tenure, cluster = "CRSP_FUNDNO")
save_model_summary(model_sharpe_tenure, "tenure_sharpe.txt")

# 4. Jensen alpha regressions (year FE)
model_jensen1_tenure <- feols(jensen_alpha_one_factor ~ tenure + I(tenure^2) + EXP_RATIO + TNA_LATEST + tenure:crisis | year,
                              data = table_tenure, cluster = "CRSP_FUNDNO")
save_model_summary(model_jensen1_tenure, "tenure_jensen1.txt")

model_jensen4_tenure <- feols(jensen_alpha_four_factor ~ tenure + I(tenure^2) + EXP_RATIO + TNA_LATEST + tenure:crisis | year,
                              data = table_tenure, cluster = "CRSP_FUNDNO")
save_model_summary(model_jensen4_tenure, "tenure_jensen4.txt")

cat("✅ All model summaries saved inside:", paths$models, "\n")


'


#  Diagnostics
# results of regression using experience variable are more stable, while creating experience variable, we didn't use mutual funds, which are managed by groups and flagged those using group variable 
# We are checking if observations which is removed due to management by groups are  different from the others using other variables of data
# percentage of cash holding by mutual fund(PER_CASH), df_analysis is dataset copy we made before filtering, ASSET_DT is the date at which PER_CASH is recorded
# 1. Crisis-interaction model
# -------------------------------
df_analysis <- df_analysis %>%
  mutate(
    year = year(ASSET_DT),
    crisis_A = ifelse(year %in% c(2001, 2008, 2009), 1, 0),
    year_A = year(ASSET_DT)
  )

model_per_cash <- feols(
  PER_CASH ~ experience + experience:crisis_A + GROUP | CRSP_FUNDNO + year_A,
  data = df_analysis,
  cluster = "CRSP_FUNDNO"
)

save_model_summary(model_per_cash, "per_cash_experience_crisis.txt")


# 2. Check if NAV and PER_CASH differ across GROUP and JOIN_NULL


# NAV ~ GROUP
model_NAV_group <- lm(NAV_LATEST ~ GROUP, data = df1)
save_model_summary(model_NAV_group, "nav_by_group.txt")

# NAV ~ JOIN_NULL
model_NAV_NA <- lm(NAV_LATEST ~ JOIN_NULL, data = df1)
save_model_summary(model_NAV_NA, "nav_by_joinnull.txt")

# PER_CASH ~ GROUP
model_cash_group <- lm(PER_CASH ~ GROUP, data = df1)
save_model_summary(model_cash_group, "percash_by_group.txt")

# PER_CASH ~ JOIN_NULL
model_cash_NA <- lm(PER_CASH ~ JOIN_NULL, data = df1)
save_model_summary(model_cash_NA, "percash_by_joinnull.txt")


# This analysis indicates sample selection bias(limitation due to data constraints)


# plot of experience
# Remove NA values and plot the histogram
p_exp <- df_analysis %>%
  filter(!is.na(experience)) %>%
  ggplot(aes(x = experience)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Manager Experience (Years)",
    x = "Experience (Years)",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave(filename = paste0(paths$figures, "experience_distribution.png"),
       plot = p_exp, width = 7, height = 5)



# Summary of Experience Variable
sink(file.path(paths$models, "summary_experience_variable.txt"))
cat("Summary of 'experience' variable in table_experience\n\n")
print(summary(table_experience$experience))
sink()
cat("✅ Summary of experience variable saved to:", file.path(paths$models, "summary_experience_variable.txt"), "\n")





#END
