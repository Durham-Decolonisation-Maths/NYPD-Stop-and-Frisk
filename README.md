# NYPD Stop-and-Frisk Algorithmic Bias Audit Lab

This repository contains materials for a workshop auditing algorithmic bias using public NYPD Stop, Question, and Frisk (SQF) data. 

Students will audit predictive models for racial disparities, specifically evaluating **False Positive Rates (FPR)** across demographic groups—and test threshold adjustments as a policy intervention. See COMPAS repository.

---

## 🛠️ Data Preprocessing

The dataset used in this lab is extracted and cleaned from the [`shftan/auditblackbox`](https://github.com/shftan/auditblackbox) repository (based on Tan et al., AAAI/ACM AIES 2018 / Goel et al., 2016).

To generate the cleaned CSV file used in this lab:

1. Run the R preprocessing script:
   ```R
   source("process_nypd_stopandfrisk_weapon.R")
