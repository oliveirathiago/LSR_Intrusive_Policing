#### Dynamics and consequences of the overpolicing-underpolicing paradox
### APPENDIX

library(tidyverse)
library(haven)
library(lavaan)
options(scipen=999)

######################################################

# Read data
load('data/export/dflong_new.Rdata')

# Cleaning data
dflong <- 
  dflong %>%
  mutate(gunall = gun == T,
         toppol = factor(toppol, levels = c("Consent", "Protest", "Rejection", "Coercion")),
         newid = as.factor(row_number()),
         law1 = case_when(
           law1 == 1 ~ 5,
           law1 == 2 ~ 4,
           law1 == 3 ~ 3,
           law1 == 4 ~ 2,
           law1 == 5 ~ 1,
           TRUE ~ NA_real_
         ),
         area1 = area == 1,
         area2 = area == 2,
         area3 = area == 3,
         area4 = area == 4,
         area5 = area == 5,
         area6 = area == 6,
         area7 = area == 7,
         area8 = area == 8,
         classA = class == 1,
         classB1 = class == 2,
         classB2 = class == 3,
         classC1 = class == 4,
         classC2 = class == 5,
         classDE = class == 6,
  ) %>%
  mutate(gunall = replace_na(gunall, 0),
         across(c(und2, und3, law1, ovp1, ovp2, pj1:pj4), ordered))

####################################################

cfa.1factor <- cfa('police =~ ovp1 + ovp2 + und3 + und2 + law1 + pj1 + pj2 + pj3 + pj4',
                   data = dflong %>% filter(wave == 1))

cfa.2factor_ovp <- cfa('police =~ und3 + und2 + law1 + pj1 + pj2 + pj3 + pj4
                        ovp =~ ovp1 + ovp2',
                       data = dflong %>% filter(wave == 1))

cfa.2factor_cyn <- cfa('police =~ ovp1 + ovp2 + pj1 + pj2 + pj3 + pj4
                        lc =~ und3 + und2 + law1',
                       data = dflong %>% filter(wave == 1))

cfa.2factor_pj <- cfa('police =~ ovp1 + ovp2 + und3 + und2 + law1
                       pj =~ pj1 + pj2 + pj3 + pj4',
                      data = dflong %>% filter(wave == 1))

cfa.3factor <- cfa('ovp =~ ovp1 + ovp2
                    lc =~ und3 + und2 + law1 
                    pj =~ pj1 + pj2 + pj3 + pj4',
                   data = dflong %>% filter(wave == 1))

# Table A4
cbind(OneFactor = inspect(cfa.1factor, 'fit.measures'),
      TwoFactor_OVP = inspect(cfa.2factor_ovp, 'fit.measures'),
      TwoFactor_CYN = inspect(cfa.2factor_cyn, 'fit.measures'),
      TwoFactor_PJ = inspect(cfa.2factor_pj, 'fit.measures'),
      ThreeFactor = inspect(cfa.3factor, 'fit.measures')
)[c('chisq', 'df', 'pvalue', 'cfi', 'tli', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper'),]


####################################################
# Measuring latent variables

cfa.lc <- cfa('lc =~ und3 + und2 + law1',
              estimator = 'pml', missing = 'pairwise', dflong)

irt_ovp <- 
  dflong %>%
  dplyr::select(ovp1, ovp2) %>%
  grm() %>%
  factor.scores.grm(resp.patterns = dflong %>% dplyr::select(ovp1, ovp2))


cfa.pj <- cfa('pj =~ pj1 + pj2 + pj3 + pj4',
              estimator = 'pml', missing = 'pairwise', dflong)

dflong <-
  dflong %>%
  mutate(lc = lavPredict(cfa.lc)[ , 'lc'],
         ovp = irt_ovp$score.dat$z1,
         pj = lavPredict(cfa.pj)[, 'pj'])

dflong %>% dplyr::select(ovp, lc, pj) %>% cor(use = 'complete.obs')
