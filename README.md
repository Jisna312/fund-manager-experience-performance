# Fund Manager Experience and Mutual Fund Performance

This repository contains R code from my Master's thesis project at the Indian Statistical Institute, Delhi, titled  
**“The Role of Managerial Experience on Mutual Fund
 Performance”**, completed under the supervision of **Prof. Monishankar Bishnoi** and **Prof. Jaideep Choudhary**.

---

##  Objective
The project investigates whether the experience of fund managers affects mutual fund performance (measured via rate of return, Sharpe ratio, and Jensen’s alpha).  
It also examines the non-linear relationship between experience and performance and how it evolves during financial crises. Also, how this relationship changes during recessions.

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

## Folder structure

- fund-manager-experience-performance/
  - data/
    - readme.txt               # information about datasets
    - DATA_SUMMARY             # summary of variables in crspm dataset
  - coding/
    - 1_data_cleaning.R        # read raw data -> clean -> save CRSPM_JOIN.csv
    - 2_analysis.R             # read CRSPM_JOIN.csv + risk_free.csv -> regressions
  - output/
    - models/                  # store model summaries or RDS outputs
    - figures/                 # charts and plots
    - tables/                  # regression result tables
  - README.md                  # project description and instructions
  - Paper.pdf                  # written analysis and explanation of results

---

##  How to Run

Make sure your working directory is set to the project root (`fund-manager-experience-performance/`).

###  Data Cleaning
Run the following command in R or the terminal:
```r
source("coding/data_cleaning.R")
```
This script:
- Loads the raw CRSP SAS dataset from `data/`
- Cleans and preprocesses fund-level information
- Saves the cleaned output as `data/CRSPM_JOIN.csv`

###  Analysis and Regression
Once the cleaned file is generated, run:
```r
source("coding/analysis.R")
```
This script:
- Loads `CRSPM_JOIN.csv` and `risk_free.csv`
- Calculates returns, Jensen’s alpha, and related measures
- Performs regression analysis using `feols()` 
- Saves tables and figures into the `output/` folder

---

## 💡 Notes
- The workflow is split into two scripts because the data cleaning stage takes longer to execute.
- Separating the scripts improves **reproducibility** and allows quick reruns of analysis without reprocessing the raw data.
- Make sure all input files are placed correctly inside the `data/` folder before running the scripts.

## Data 
The underlying mutual fund and CRSP datasets are proprietary and **not publicly shared**
However, the full code pipeline is provided to illustrate the analytical and econometric approach.

## Limitation
Analysis shows sample selection bias, which is happening due to data constraints; I am working on solutions to remedy the sample selection bias.

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
library(lmtest)
