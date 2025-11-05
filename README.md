# Fund Manager Experience and Mutual Fund Performance

This repository contains R code from my Master's thesis project at the Indian Statistical Institute, Delhi, titled  
**“The Role of Managerial Experience on Mutual Fund
 Performance”**, completed under the supervision of **Prof. Monishankar Bishnoi** and **Prof. Jaideep Choudhary**.

---

##  Objective
The project investigates whether the experience of fund managers affects mutual fund performance (measured via rate of return, Sharpe ratio, and Jensen’s alpha).  
It also examines the non-linear relationship between experience and performance and how it evolves during financial crises.

---

##  Methodology Overview
1. **Data Cleaning and Transformation**
   - CRSP Mutual Fund dataset (proprietary)
   - Risk-free rate and factor data merged from Fama–French datasets
   - Calculation of fund-level variables like tenure, experience, Sharpe ratio, and Jensen’s alpha

2. **Econometric Modeling**
   - Panel regressions with fund and year fixed effects using the `fixest` packages
   - Models estimated:
     - Return vs Experience/Tenure + controls
     - Sharpe Ratio models vs Experience/Tenure + controls
     - Jensen’s one-factor and four-factor alpha models vs Experience/Tenure + controls

3. **Key Metrics**
   - Rate of return (annualised)
   - Sharpe ratio
   - Jensen’s alpha (1-factor and 4-factor)
   - Fund size (AUM)
   - Expense ratio
   - Crisis interaction terms

---

## 📁 Folder Structure


fund-manager-experience-performance/
│
├── data/
│ ├── crspm.sas7bdat # Original CRSPM SAS dataset (raw, untouched)
│ ├── risk_free.csv # Factor / risk-free data
│ └── CRSPM_JOIN.csv # Output from Script 1 (cleaned, ready-to-analyze)
│
├── code/
│ ├── 1_data_cleaning.R # read raw data → clean → save CRSPM_JOIN.csv
│ └── 2_analysis.R # read CRSPM_JOIN.csv + risk_free.csv → regressions
│
├── output/
│ ├── models/ # Store model summaries or RDS outputs
│ ├── figures/ # Any charts you create later
│ └── tables/ # Regression result tables
│
└── README.md # Short description of the project
The data cleaning process is time-consuming.
During re-runs, recalculating the rate of return caused inconsistencies.
To improve  reproducibility:

The data cleaning part is executed once and saved as a clean file (CRSPM_Join.csv).

The analysis part can be rerun multiple times independently.

This modular structure reduces runtime and avoids redundant processing.


## Data 
The underlying mutual fund and CRSP datasets are proprietary and **not publicly shared** due to licensing restrictions.  
However, the full code pipeline is provided to illustrate the analytical and econometric approach.



---

##  R Packages
```r
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(fixest)
library(plm)
library(ggplot2)
library(haven)
