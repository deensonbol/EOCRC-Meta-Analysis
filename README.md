# Body Mass Index and Early-Onset Colorectal Cancer: A Meta-Analysis  

This repository contains R code and output for a meta-analysis examining the relationship between **obese BMI** and **early-onset colorectal cancer (EOCRC)**.  
The analysis uses pooled data from published studies and applies random-effects models to estimate combined risk ratios, hazard ratios, and odds ratios.


##  Overview
- Compiles data from **case-control** and **cohort** studies (2018–2024)
- Visualizes pooled estimates using **forest plots**
- Evaluates heterogeneity using τ², I², and Cochran’s Q


## Methods
- Conducted using R with the **metafor** package for random-effects modeling  
- Tables generated using **knitr** + **kableExtra**  
- Outputs exported to HTML and PNG formats for reproducible reporting


## Tools Used
- **R Packages:** metafor, knitr, kableExtra, webshot2, chromote  
- **Output:** HTML tables, PNG forest plots  
- **Platform:** RStudio / GitHub


## How to Run
1. Clone or download this repository.  
2. Open `eo_crc_meta.R` in RStudio.  
3. Run the script to reproduce the tables and forest plots.  
   > (Ensure Chrome/Chromium is installed for webshot2 image exports.)


## Example Output
- `table.html` – Formatted study characteristics  
- `table.png` – Exported table image  
- Forest plots – Combined RR/HR and OR estimates  
