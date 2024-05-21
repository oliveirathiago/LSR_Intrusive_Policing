#### Legal cynicism, intrusive policing, and the dynamics of police legitimacy: Evidence from Brazil's largest city ###
### Thiago R. Oliveira ###
### Law & Society Review

#### cleaning data ###

library(tidyverse)
library(haven)

data <- read_dta("data/import/banco_painel_3ondas.dta")
painel <- read_spss('data/import/painelaber_1.sav')
load('data/export/painel_with_topics_jan.RData')


data <- arrange(data, quest)
painel <- arrange(painel, quest)
data$area <- painel$area_chave

data <- data %>%
  as.data.frame() %>%
  mutate(id_match = paste(quest, onda, sep = ".")) %>%
  left_join(painel_with_topics[, c('id_match', 'topic_law', 'final_topics')], by = 'id_match') %>%
  rename('panelid' = 'id_entrev',
         'class' = 'CBclassf',
         'white' = 'raca_ob',
         'male' = 'sexo',
         'age' = 'idade',
         'law1' = 'Ofreq_proteg',
         'law2' = 'Ofreq_interess',
         'law3' = 'Ofreq_certo',
         'law4' = 'Ofreq_risco',
         'law5' = 'Ofreq_crime',
         'napol1' = 'Pbairr_acao',
         'napol3' = 'Pbairr_valor',
         'vio1' = 'Mviol_agress',
         'vio2' = 'Mviol_ladrao',
         'vio3' = 'Mviol_confli',
         'vio4' = 'Mviol_puni',
         'vio5' = 'Mviol_ving',
         'vio6' = 'Mviol_protes',
         'fearpolgen' = 'Ppbairr_medo',
         'fearpolper' = 'Ppbairr_medovc',
         'fear1' = 'Pmedop',
         'stop1_carro' = 'Ppara_carro',
         'stop1_moto' = 'Ppara_moto',
         'stop1_rua' = 'Ppara_rua',
         'stop1_out' = 'Ppara_out1',
         'stop2' = 'Ppara',
         'stop3' = 'Ppara_quant',
         'gun' = 'Ppara_arma',
         'phyhar' = 'Vrecsitu_poli',
         'verhar' = 'Vrecsitu_verb',
         'dutylaw' = 'Oconc_leis',
         'dutylaw_text' = 'Oconc_leis_raz_t',
         'dutypol' = 'Pobedi',
         'dutypol_text' = 'Pobedi_raz1',
         'djpol' = 'Pbairr_trata',
         'pj1' = 'Pbairr_expli',
         'pj2' = 'Pbairr_decis',
         'pj3' = 'Pbairr_atenc',
         'pj4' = 'Pbairr_tratavc',
         'eff1' = 'Ptrab_droga',
         'eff2' = 'Ptrab_assalt',
         'eff3' = 'Ptrab_chamad',
         'eff4' = 'Ptrab_delec',
         'eff5' = 'Ptrab_invest',
         'eff6' = 'Ptrab_manif',
         'und1pre' = 'Bserv_policia',
         'und1' = 'Bserv_policia_qq', # P6A07
         'und2' = 'Ptrab_ruas',       # P4607
         'und3' = 'Pbairr_segur',     # P3308
         'ovp1' = 'Pouviu_lei',       # acima da lei
         'ovp2' = 'Pouviu_intim',     # intimidam
         'toplaw' = 'topic_law',
         'toppol' = 'final_topics',
         'pm' = 'Ppol_bairr_pol',
         'pc' = 'Ppol_bairr_regis',
         'napolpm' = 'Ppm_lei',        # P43A03
         'napolpc' = 'Ppc_lei',
         'napolp'  = 'Pestad_lei') %>%
  dplyr::select(quest, onda, class, white, male, area, age, law1:law5, napol1, napol3, vio1:vio6, fearpolgen, fearpolper, fear1, 
                stop1_carro, stop1_moto, stop1_rua, stop1_out, stop2, stop3, gun, phyhar, verhar, dutylaw, dutypol, djpol, pj1:pj3, pj4, eff1:und2, und1, und3, und1pre, ovp1, ovp2, 
                toppol, toplaw, pm, pc, napolpm, napolpc, napolp, panelid) %>%
  mutate_at(vars(quest, onda), as.factor) %>%
  mutate_at(vars(law1:law5, napol1, napol3, djpol, pj1:pj4, ovp1, ovp2, fearpolgen, fearpolper, fear1, napolpm:napolp, eff1:ovp2), as.numeric) %>%
  mutate_at(vars(law1:law5, napol1, napol3, djpol, pj1:pj4, ovp1, ovp2, fearpolgen, fearpolper, napolpm:napolp), list(~case_when(
    . == '1' ~ 5, 
    . == '2' ~ 4,
    . == '3' ~ 3,
    . == '4' ~ 2,
    . == '5' ~ 1,
    TRUE ~ as.numeric(NA)))) %>%
  mutate_at(vars(eff1:eff6), list(~case_when(
    . == '6' ~ 1,
    . == '4' ~ 2,
    . == '3' ~ 3, 
    . == '5' ~ 4,
    . == '2' ~ 5,
    . == '1' ~ 6,
    TRUE ~ as.numeric(NA)))) %>%
  mutate_at(vars(fear1), list(~case_when(
    . == '1' ~ 3,
    . == '2' ~ 2,
    . == '3' ~ 1,
    TRUE ~ as.numeric(NA)))) %>%
  mutate(und1 = ifelse(und1pre == 2, 0, und1),
         vio1 = vio1 == 1,
         vio2 = vio2 == 1,
         vio3 = vio3 == 1,
         vio4 = vio4 == 1,
         vio5 = vio5 == 1,
         vio6 = vio6 == 1,
         napolpre2 = NULL,
         pm = onda == 1 & pm == 1,
         pc = onda == 1 & pc == 1,
         lawcons = toplaw == 'Consent',
         polcons = toppol == 'Consent',
         polcoer = toppol == 'Coercion',
         polprot = toppol == 'Protest',
         polrej  = toppol == 'Rejection',
         stop1_moto = stop1_moto == 1,
         stop1_carro = stop1_carro == 1,
         stop1_rua = stop1_rua == 1,
         stop1_out = stop1_out == 1,
         stop2 = stop2 > 0 & stop2 < 90,
         stop3 = stop3 > 0 & stop3 < 90,
         gun = gun == 1,
         white = white == 1,
         male = male == 1,
         verhar = verhar == 1,
         phyhar = phyhar == 1,
         dutypol = dutypol == 1,
         dutylaw = dutylaw == 1) %>%
  mutate_at(vars(und1), list(~case_when(
    . == '0' ~ 5,
    . == '4' ~ 4,
    . == '3' ~ 3,
    . == '2' ~ 2,
    . == '1' ~ 1,
    TRUE ~ as.numeric(NA)))) %>%
  mutate_at(vars(und2, und3), list(~case_when(
    . == '1' ~ 1,
    . == '2' ~ 2,
    . == '5' ~ 3,
    . == '3' ~ 4,
    . == '4' ~ 5,
    . == '6' ~ 6,
    TRUE ~ as.numeric(NA)))) %>%
  mutate(stop1 = stop1_carro == T | stop1_moto == T |
           stop1_rua == T | stop1_out == T,
         napolpre2 = ifelse(onda == 1 & pm == T, napolpm, 
                            ifelse(onda == 1 & pm == F & pc == T, napolpc, 
                                   ifelse(onda == 1 & pm == F & pc == F, napolp, 0)))) %>%
  mutate(stop = ifelse(onda == 1, stop1,
                       ifelse(onda == 2, stop2, 
                              ifelse(onda == 3, stop3, F))),
         napol2 = ifelse(onda == 1, napolpre2, napol3)) %>%
  dplyr::select(quest:napol1, napol2, vio1:fear1, stop, gun:eff6, und1, und2, und3, ovp1:toplaw, lawcons:polrej, panelid) %>%
  arrange(onda) 

# area, race, gender, class: set as time-invariant
# select vars, save data.

dfwide <- reshape(data,
                  timevar = 'onda',
                  idvar = 'quest',
                  direction = 'wide',
                  sep = '_') %>%
  dplyr::select(-c('class_2', 'white_2', 'male_2', 'area_2',
                   'class_3', 'white_3', 'male_3', 'area_3')) %>%
  rename('class' = 'class_1',
         'white' = 'white_1',
         'male' = 'male_1',
         'area' = 'area_1')

dflong <- reshape(dfwide,
                  timevar = 'wave',
                  idvar = 'quest',
                  direction = 'long',
                  sep = '_',
                  varying = c(6:146)) %>%
  filter(!is.na(panelid)) %>%
  arrange(wave)

save(dflong, file = 'data/export/dflong_new.Rdata')
