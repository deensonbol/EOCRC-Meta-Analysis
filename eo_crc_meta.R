# --- Setup: install/load packages (idempotent) -------------------------------
pkgs <- c("knitr","kableExtra","webshot2","chromote","metafor")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install)

library(knitr)
library(kableExtra)
library(webshot2)   
library(metafor)

# --- Case-control & cohort study characteristics table ----------------------
# Build data frames
case_control <- data.frame(
  Author_Year = c("Li et al. (2022)", "Schumacher et al. (2021)", "Zheng et al. (2018)"),
  Location = c("Germany", "USA", "USA"),
  Sample_Size = c("747 Cases / 621 Controls", "1032 Cases / 5128 Controls", "2031 Cases"),
  Gender_Breakdown = c(
    "425M / 322F (Cases), 345M / 276F (Controls)",
    "517M / 515F (Cases), 2567M / 2561F (Controls)",
    "No gender breakdown"
  ),
  Age_Range = c("30+", "15-49", "20-50"),
  Early_Onset_Definition = rep("<50", 3),
  Obesity_Definition = rep("Normal (<25), Overweight (25-29.9), Obese (≥30)", 3),
  stringsAsFactors = FALSE
)

cohort <- data.frame(
  Author_Year = c("Himbert et al. (2024)", "Krigel et al. (2020)", "Liu et al. (2019)",
                  "Low et al. (2021)", "Pan et al. (2023)", "Song et al. (2023)",
                  "O’Sullivan et al. (2024)", "Syed et al. (2019)"),
  Location = c("USA & Germany", "USA", "USA", "USA", "China", "South Korea", "Canada", "USA"),
  Sample_Size = c("459 Cases", "363 Cases", "114 EOCRC Cases, 85,256 Total",
                  "651 Cases / 67,416 Controls", "222 Cases / 87,833 Controls",
                  "3,340,635 Total, 7,492 EOCRC Cases",
                  "98 Cases / 127,852 Total", "5,710 Cases / 11,800,420 Controls"),
  Gender_Breakdown = c(
    "252M / 207F", "186M / 177F", "All Women",
    "594M / 57F (Cases), 55,399M / 12,017F (Controls)",
    "~85M / ~137F (Cases)", "3,376F / 4,116M (Cases)",
    "No gender breakdown",
    "2,910F / 2,800M (Cases), 6,869,290F / 4,931,130M (Controls)"
  ),
  Age_Range = c("<50", "18-49", "25-42", "<50", "30-50", "20-49", "<50", "25-49"),
  Early_Onset_Definition = rep("<50", 8),
  Obesity_Definition = c(
    "Normal (<25), Overweight (25-29.9), Obese (≥30)",
    "Normal (<25), Overweight (25-29.9), Obese (≥30)",
    "Normal (<23), Overweight (23-27.4), Obese (≥27.5)",
    "Normal (<25), Overweight (25-29.9), Obese (≥30)",
    "Normal (<24), Overweight (24-27.9), Obese (≥28)",
    "Normal (<25), Overweight (25-29.9), Obese (≥30)",
    "Normal (<25), Overweight (25-29.9), Obese (≥30)",
    "Normal (<25), Overweight (25-29.9), Obese (≥30)"
  ),
  stringsAsFactors = FALSE
)

# Combine & tidy columns
ws_table <- rbind(case_control, cohort)
rownames(ws_table) <- NULL
names(ws_table)[names(ws_table) == "Obesity_Definition"] <- "BMI range definition (kg/m²)"
ws_table$`BMI range definition (kg/m²)` <- gsub(", ", "<br>", ws_table$`BMI range definition (kg/m²)`)
ws_table$Gender_Breakdown[1:nrow(case_control)] <- gsub(", ", "<br>", ws_table$Gender_Breakdown[1:nrow(case_control)])

# Render kable (HTML) and export to PNG via webshot2
cap_html <- "<strong>Table 1. Characteristics of All Studies Investigating Obese BMI for Early-Onset (&lt;50 Years of Age) Colorectal Cancer Risk</strong>"

tbl <- kable(
  ws_table,
  caption = cap_html,
  col.names = c("Author (Year)", "Location", "Sample Size", "Gender Breakdown",
                "Age Range", "Early-Onset Definition (Years)", "BMI range definition (kg/m²)"),
  format = "html", escape = FALSE
) |>
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped", "hover", "condensed", "responsive")) |>
  group_rows("Case-Control Studies", 1, nrow(case_control)) |>
  group_rows("Cohort Studies", nrow(case_control) + 1, nrow(ws_table)) |>
  footnote(
    general = "This table summarizes the characteristics of studies included in the meta-analysis, divided into case-control and cohort studies.",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )

save_kable(tbl, "table.html")
webshot("table.html", "table.png", zoom = 2)   

# --- Meta-analysis: RR/HR (random-effects) + custom forest plot -------------
meta_data_rr_hr <- data.frame(
  Study = c("Liu, P. H., et al. (2019)", "Pan et al., (2023)", "Song et al., (2023)",
            "O'Sullivan et al., (2024)", "Zheng et al., (2018)"),
  Effect_Estimate = c(1.93, 1.98, 1.09, 0.85, 1.27),
  Variance = c(0.286990, 0.156348, 0.001100, 0.101683, 0.027495),
  Type = c("RR", "RR", "HR", "HR", "HR")
)

# Keep row order stable so symbols align with rows
res_rr_hr <- rma(yi = Effect_Estimate, vi = Variance, data = meta_data_rr_hr, method = "REML")

# Map symbols/colors by type (RR: black square; HR: red circle)
pch_map <- ifelse(meta_data_rr_hr$Type == "RR", 15, 16)
col_map <- ifelse(meta_data_rr_hr$Type == "RR", "black", "red")

fp <- forest(
  res_rr_hr,
  slab    = meta_data_rr_hr$Study,
  xlab    = "Risk Ratio (RR) / Hazard Ratio (HR) [95% CI]",
  main    = "RR/HR of Obese BMI on Early-Onset Colorectal Cancer [95% CI]",
  atransf = identity,
  at      = seq(0.5, 3, 0.5),
  refline = 1,
  mlab    = "Random Effects Model",
  pch     = NA
)

points(x = meta_data_rr_hr$Effect_Estimate,
       y = fp$rows,
       pch = pch_map,
       col = col_map,
       cex = 1.2)

text(x = max(fp$xlim), y = fp$ylim[1] + 1.5,
     labels = "Pooled Effect Estimate", pos = 2, cex = 0.8)

legend("topright",
       legend = c("Relative Risk (RR)", "Hazard Ratio (HR)"),
       pch    = c(15, 16),
       col    = c("black", "red"),
       cex    = 0.85)

summary(res_rr_hr)

# --- Meta-analysis: OR (random-effects) + heterogeneity annotation ----------
meta_data_or <- data.frame(
  Study = c("Himbert et al. (2024)", "Krigel et al. (2020)",
            "Li et al. (2022) AT AGE 30", "Li et al. (2022) ~10 years before diagnosis",
            "Low et al. (2021)", "Schumacher et al. (2021)", "Syed et al. (2019)"),
  Effect_Estimate = c(0.56, 1.44, 2.06, 1.88, 0.69, 1.41, 2.88),
  Variance        = c(0.007, 0.060, 0.299, 0.132, 0.006, 0.022, 0.005)
)

res_or <- rma(yi = Effect_Estimate, vi = Variance, data = meta_data_or, method = "REML")

fp_or <- forest(res_or,
                slab    = meta_data_or$Study,
                xlab    = "Odds Ratio (OR) [95% CI]",
                main    = "OR of Obese BMI on Early-Onset Colorectal Cancer [95% CI]",
                atransf = identity,
                at      = seq(0.5, 3.5, 0.5),
                refline = 1,
                mlab    = "Random Effects Model")

text(x = max(fp_or$xlim), y = fp_or$ylim[1] + 0.2,
     labels = "Pooled Effect Estimate", pos = 2, cex = 0.7)

pval_str <- ifelse(res_or$QEp < 0.001, "<0.001", sprintf("%.3f", res_or$QEp))
het_label <- bquote("Heterogeneity:" ~ tau^2 == .(sprintf("%.2f", res_or$tau2)) *
                      "; " ~ I^2 == .(sprintf("%.1f", res_or$I2)) * "%" *
                      "; p = " ~ .(pval_str))

x_center <- mean(fp_or$xlim)
y_bottom <- fp_or$ylim[1] - 0.75

text(x = x_center, y = y_bottom, labels = het_label,
     cex = 0.65, adj = c(1.35, -0.45), xpd = TRUE)

summary(res_or)
