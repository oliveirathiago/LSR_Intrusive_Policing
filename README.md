# LSR_Intrusive_Policing

This repository contains the data and code for the article:

> Oliveira, Thiago R. (2024). Legal cynicism, intrusive policing, and the dynamics of police legitimacy: Evidence from Brazil\'92s largest city. Currently available at https://osf.io/preprints/socarxiv/89jkv.

Please cite this repository as:

> Oliveira, Thiago. (2024). *Repository of R code and data for 'Legal cynicism, intrusive policing, and the dynamics of police legitimacy*. Accessed on XXXX.

### Structure

-   `data`
    -   contains the relevant data sets necessary to replicate all analyses
    -   `import` is an empty directory with the raw survey data, which is protected
    -   `export` is populated when `1_SVM_Text_classification_models.R` and `2_Cleaning_data.R` run.
-   `plots` populated when `3_main_analysis.R` runs.
-   `1_SVM_Text_classification_models.R` replicates the text classification models used to classify open-ended responses into four categories.
-   `2_Cleaning_data.R` draws on the raw (protected) survey data and generates the data set `data/export/dflong_new.RData`, which can be used to replicate all analyses included in Oliveira (2024).
-   `3_main_analysis.R` replicates the main analyses of the paper.
-   `4_Appendix.R` replicates the analyses included in the Appendix.
