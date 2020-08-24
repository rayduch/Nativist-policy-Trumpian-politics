# Replication Guide to Manuscript ID PSRM-OA-2019-0032 

## Requirements

**Hardware**: All code was run on a  MacBook Pro 13-inch (2017), 2.3 GHz Dual-Core Intel Core i5 processor, 8GB RAM, and macOS Catalina (v10.15.2).

**Data**: All data is included within the `data/` subdirectory of these replication files.

**Software**: *R (v3.6.0)*

**Package requirements**:
* openxlsx -- 4.1.0.1
* xtable -- 1.8-4
* gridExtra -- 2.3
* coefplot -- 1.2.6
* dummies -- 1.5.6
* nnet -- 7.3--12
* stargazer -- 5.2.2
* tidyverse -- 1.3.0
* lubridate -- 1.7.4
* ggrepel -- 0.8.1
* cregg -- 0.3.0

**Total replication time**: approx. 1 minute.

## Replication workflow

All figures and tables (except the non-analytic Table 1) are produced via three R scripts:
* `1_main_replication.R` produces Figures 1-4 and Tables 2 and 4
* `2_iet_analysis.R` produces Table 3
* `3_appendix_replication.R` produces all Figures and Tables in the Appendix

To execute all the replication files, we have also included a small bash file that can be executed at the command line:
```
bash main_replication.sh
```
Text output of the replication conducted on the author's computer is saved as `replication_log.txt`. 

**Note:** the main conjoint models are generated using `0_main_models.R` -- since these models are used for both the main and appendix figures/tables, this code is sourced into the necessary replication files automatically. Users do not need to run this file separately.

