{\rtf1\ansi\ansicpg1252\cocoartf2761
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww22820\viewh15000\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 # LSR_Intrusive_Policing\
\
This repository contains the data and code for the article:\
\
> Oliveira, Thiago R. (2024). Legal cynicism, intrusive policing, and the dynamics of police legitimacy: Evidence from Brazil\'92s largest city. Currently available at {\field{\*\fldinst{HYPERLINK "https://osf.io/preprints/socarxiv/89jkv"}}{\fldrslt https://osf.io/preprints/socarxiv/89jkv}}.\
\
Please cite this repository as:\
\
> Oliveira, Thiago. (2024). *Repository of R code and data for 'Legal cynicism, intrusive policing, and the dynamics of police legitimacy*. Accessed on XXXX.\
\
### Structure\
\
-   `data`\
    -   contains the relevant data sets necessary to replicate all analyses\
    -   `import` is an empty directory with the raw survey data, which is protected\
    -   `export` is populated when `1_SVM_Text_classification_models.R` and `2_Cleaning_data.R` run.\
-   `plots` populated when `3_main_analysis.R` runs.\
-   `1_SVM_Text_classification_models.R` replicates the text classification models used to classify open-ended responses into four categories.\
-   `2_Cleaning_data.R` draws on the raw (protected) survey data and generates the data set `data/export/dflong_new.RData`, which can be used to replicate all analyses included in Oliveira (2024).\
-   `3_main_analysis.R` replicates the main analyses of the paper.\
-   `4_Appendix.R` replicates the analyses included in the Appendix.}