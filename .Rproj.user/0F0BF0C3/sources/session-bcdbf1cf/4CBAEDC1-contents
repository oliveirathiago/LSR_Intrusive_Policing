#### Legal cynicism, intrusive policing, and the dynamics of police legitimacy: Evidence from Brazil's largest city ###
### Thiago R. Oliveira ###
### Law & Society Review

library(tidyverse)
library(haven)
library(lavaan)
library(ltm)
library(MplusAutomation)
library(lme4)
library(panelr)
library(dpm)
library(texreg)
options(scipen=999)


######################################################

# Read data
load('data/export/dflong_new.Rdata')

# Cleaning data
dflong <- 
  dflong %>%
  mutate(#newid = as.factor(row.names(dflong)),
         newid = as_factor(row_number()),
         gunall = gun == T,,
         toppol = factor(toppol, levels = c("Consent", "Protest", "Rejection", "Coercion")),
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

##############################



### Measuring police legitimacy

# 1) Export data to Mplus
dflong %>%
  dplyr::select(newid, napol1, napol2, fearpolgen, fearpolper, toppol) %>%
  prepareMplusData(filename = 'data/export/continuum.dat')

# 2) Run IRT model on Mplus

# 3) Save IRT scores from Mplus
dflong <-
  dflong %>%
  left_join(read.table('data/export/continuum_long.txt') %>%
              rename('newid' = 'V4',
                     'legitimacy' = 'V5') %>%
              dplyr::select(newid, legitimacy) %>%
              mutate(newid = as_factor(newid)),
            by = 'newid')


# Correlation matrices between latent constructs by wave
cor_constucts_1 <- dflong %>% filter(wave == 1) %>% dplyr::select(ovp, lc, pj, legitimacy) %>% cor(use = 'complete.obs')
cor_constucts_2 <- dflong %>% filter(wave == 2) %>% dplyr::select(ovp, lc, pj, legitimacy) %>% cor(use = 'complete.obs')
cor_constucts_3 <- dflong %>% filter(wave == 3) %>% dplyr::select(ovp, lc, pj, legitimacy) %>% cor(use = 'complete.obs')


###################################################

# Table 1
dflong %>%
  mutate(across(c(ovp1, ovp2, und2, und3, law1, vio1:vio6, pj1:pj4), as.numeric)) %>%
  mutate(across(c(vio1:vio6), ~case_when(
    . == 1 ~ 0L,
    . == 2 ~ 1L
  ))) %>%
  group_by(wave) %>%
  summarise(ovp_abovelaw = mean(ovp1, na.rm = T),
            ovp_harass = mean(ovp2, na.rm = T),
            und_law = mean(law1, na.rm = T),
            und_mysafety = mean(und3, na.rm = T),
            und_neighsafe = mean(und2, na.rm = T),
            napol = mean(napol1, na.rm = T),
            fear = mean(fearpolgen, na.rm = T),
            duty = mean(dutypol, na.rm = T),
            norm = mean(polcons, na.rm = T),
            coer = mean(polcoer, na.rm = T),
            disob = mean(polprot, na.rm = T),
            rej = mean(polrej, na.rm = T),
            vio1 = mean(vio1, na.rm = T),
            vio2 = mean(vio2, na.rm = T),
            vio3 = mean(vio3, na.rm = T),
            vio4 = mean(vio4, na.rm = T),
            vio5 = mean(vio5, na.rm = T),
            pj1 = mean(pj1, na.rm = T),
            pj2 = mean(pj2, na.rm = T),
            pj3 = mean(pj3, na.rm = T),
            pj4 = mean(pj4, na.rm = T),
            stop = mean(stop, na.rm = T),
            gun = mean(gunall, na.rm = T),
            white = mean(white, na.rm = T),
            male = mean(male, na.rm = T),
            class = mean(class, na.rm = T)
  )


# Table 2
dflong %>%
  dplyr::select(quest, wave,
                white, class, male, area,
                ovp, lc, stop, gun, age) %>%
  pivot_wider(id_cols = c('quest', 'white', 'class', 'male', 'area'),
              names_from = wave,
              values_from = c('ovp', 'lc', 'stop', 'gun', 'age')) %>%
  mutate(youth = age_1 < 25,
         youth2 = age_1 < 30,
         youngmale = age_1 < 25 & male == T) %>%
  group_by(area) %>%
  summarise(
    white = mean(white, na.rm = T),
    class = mean(class, na.rm = T),
    ovp = mean(ovp_1, na.rm = T),
    lc = mean(lc_1, na.rm = T),
    stop = mean(stop_1, na.rm = T),
    gun = mean(gun_1, na.rm = T),
    young = mean(youth, na.rm = T),
    youngadults = mean(youth2, na.rm = T),
    youngmale = mean(youngmale, na.rm = T)
  )

# area2 = Area 1: reference group
# area8 = Area 2
# area4 = Area 3
# area3 = Area 4
# area7 = Area 5
# area5 = Area 6
# area6 = Area 7
# area1 = Area 8


###################################################

## FIRST PART: DYNAMICS OF POLICE INTRUSION AND POLICE CYNICISM
# Dynamic panel models

df_dpm <-
  dflong %>%
  panel_data(id = quest,
             wave = wave)

dpm_ovp <- dpm(ovp ~ lc + gun | white + male + classA + classB1 + classB2 + classC2 + classDE + area1 + area3 + area4 + area5 + area6 + area7 + area8,
               data = df_dpm, error.inv = T, information = "observed", missing = "fiml", x.free = F)
dpm_lc <- dpm(lc ~ ovp + + gun | white + male + classA + classB1 + classB2 + classC2 + classDE + area1 + area3 + area4 + area5 + area6 + area7 + area8,
              data = df_dpm, error.inv = T, information = "observed", missing = "fiml", x.free = F)

dpm.plot_data <-
  summary(dpm_ovp, standardized = T, ci = T)$coefficients %>% 
  mutate(model = 'M2: Perceptions of \n police intrusion') %>%
  full_join(summary(dpm_lc, standardized = T, ci = T)$coefficients %>%
              mutate(model = 'M1: Cynicism about \n police protection')) %>%
  tibble() %>%
  add_row(t = '2', coef = 'area2TRUE', model = 'M2: Perceptions of \n police intrusion') %>%
  add_row(t = '2', coef = 'area2TRUE', model = 'M1: Cynicism about \n police protection') %>%
  add_row(t = '2', coef = 'lc (t - 1)', model = 'M2: Perceptions of \n police intrusion') %>%
  add_row(t = '2', coef = 'ovp (t - 1)', model = 'M1: Cynicism about \n police protection') %>%
  add_row(t = '2', coef = 'classC1', model = 'M2: Perceptions of \n police intrusion') %>%
  add_row(t = '2', coef = 'classC1', model = 'M1: Cynicism about \n police protection')


plot.dpm_reciprocal <- ggplot(dpm.plot_data %>% 
                                filter(coef %in% c('ovp', 'lc', 'ovp (t - 1)', 'lc (t - 1)')) %>% 
                                mutate(coef = case_when(
                                  coef == 'ovp (t - 1)' ~ 'ovp', 
                                  coef == 'lc (t - 1)' ~ 'lc', 
                                  TRUE ~ coef)) %>%
                                drop_na, aes(y = Est., x = coef)) +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 0.2, width = .1, position = position_dodge(), show.legend = T) + 
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  facet_grid(. ~ model, switch = "y")+#, scales = "free") +
  ylim(-.4,.4) +
  coord_flip() + 
  ylab("") + xlab("") + 
  labs(caption = 'Two ML-SEMs estimating the effects of perceptions of police intrusion on cynicism about police protection (M1)
                  and the effects of cynicism about police protection on perceptions of police intrusion (M2). \n
                  Fully standardized coefficients and 95% confidence intervals reported. n = 1200 for both models (using FIML).') + 
  guides(colour = guide_legend(title = "")) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8),
        legend.position="bottom") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 12),
        legend.text = element_text(colour = "#3C3C3C", size = 12),
        strip.text.x = element_text(size = 12),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 2) + 
  scale_x_discrete(limits = c('lc', 'ovp'),
                   breaks = c('lc', 'ovp'),
                   labels = c('Cynicism about police protection', 'Perceptions of police intrusion')) 

plot.dpm_correlates <- ggplot(dpm.plot_data %>% 
                                filter(!coef %in% c('ovp (t - 1)', 'ovp', 'lc (t - 1)', 'lc')),
                              aes(y = Est., x = coef)) + 
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 0.2, width = .25, position = position_dodge(), show.legend = T) + 
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  facet_grid(. ~ model, switch = "y") +
  ylim(-.7,.7) +
  coord_flip() + 
  ylab("") + xlab("") + 
  labs(caption = 'Two ML-SEMs estimating the effects of perceptions of police intrusion on cynicism about police protection (M1)
                  and the effects of cynicism about police protection on perceptions of police intrusion (M2). \n
                  Fully standardized coefficients and 95% confidence intervals reported. n = 1200 for both models (using FIML).') + 
  guides(colour = guide_legend(title = "")) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8),
        legend.position="bottom") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 12),
        legend.text = element_text(colour = "#3C3C3C", size = 12),
        strip.text.x = element_text(size = 12),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 2.5) + 
  scale_x_discrete(limits = c('classATRUE', 'classB1TRUE', 'classB2TRUE', 'classC2TRUE', 'classDETRUE', 'classC1', 
                              'maleTRUE', 'whiteTRUE', 'area1TRUE', 'area6TRUE', 'area5TRUE', 'area7TRUE', 
                              'area3TRUE', 'area4TRUE', 'area8TRUE', 'area2TRUE', 'gunTRUE'),
                   breaks = c('classATRUE', 'classB1TRUE', 'classB2TRUE', 'classC2TRUE', 'classDETRUE', 'classC1', 
                              'maleTRUE', 'whiteTRUE', 'area1TRUE', 'area6TRUE', 'area5TRUE', 'area7TRUE', 
                              'area3TRUE', 'area4TRUE', 'area8TRUE', 'area2TRUE', 'gunTRUE'),
                   labels = c('Social class: A', 'Social class: B1', 'Social class: B2', 'Social class: C2', 'Social class: D/E', 'Social class (reference group: C1)',
                              'Gender (1 = male)', 'Race (1 = White)',
                              'Area 8 (White, wealthy gated communities)', 'Area 7 (affluent, mostly White)',
                              'Area 6 (middle class, mostly White)', 'Area 5 (middle class, racially diverse)',
                              'Area 4 (poor and mostly non-White)', 'Area 3 (concentrated disadvantage, racially diverse)',
                              'Area 2 (peri-urban and racially diverse)', 'Area 1 (reference group: economically deprived)',
                              'Recent police stop at gunpoint'))

pdf('plots/dpm_reciprocal.pdf')
plot.dpm_reciprocal
dev.off()

pdf('plots/dpm_correlates.pdf')
plot.dpm_correlates
dev.off()

# area2 = Area 1: reference group
# area8 = Area 2
# area4 = Area 3
# area3 = Area 4
# area7 = Area 5
# area5 = Area 6
# area6 = Area 7
# area1 = Area 8

####################################################
## SECOND PART: REGRESSING LEGITIMACY ON POLICE INTRUSION AND POLICE CYNICISM

## Association between overpolicing and underpolicing and police legitimacy
m1 <- wbm(legitimacy ~ ovp + lc | class + white + male + area1 + area3 + area4 + area5 + area6 + area7 + area8 | (1| quest), data = df_dpm)
m2 <- wbm(legitimacy ~ ovp + lc + pj + gunall | class + white + male + area1 + area3 + area4 + area5 + area6 + area7 + area8 | (1| quest), data = df_dpm)

wbm.plot_data <-
  tibble(
    model = c('m1', 'm2') %>% rep(each = 4) %>% rep(each = 2),
    est = c('Within effects', 'Between effects') %>% rep(each = 4) %>% rep(2) %>% factor(levels = c('Within effects', 'Between effects')),
    variables = c('ovp', 'lc', 'pj', 'gunall') %>% rep(2) %>% rep(2),
    coef = c(
      fixef(m1)['ovp'], fixef(m1)['lc'], NA, NA,
      fixef(m1)['`imean(ovp)`'], fixef(m1)['`imean(lc)`'], NA, NA,
      fixef(m2)['ovp'], fixef(m2)['lc'], fixef(m2)['pj'], fixef(m2)['gunallTRUE'], 
      fixef(m2)['`imean(ovp)`'], fixef(m2)['`imean(lc)`'], fixef(m2)['pj'], fixef(m2)['`imean(gunallTRUE)`']
    ),
    cilow = c(
      confint(m1)['ovp', 1], confint(m1)['lc', 1], NA, NA,
      confint(m1)['`imean(ovp)`', 1], confint(m1)['`imean(lc)`', 1], NA, NA,
      confint(m2)['ovp', 1], confint(m2)['lc', 1], confint(m2)['pj', 1], confint(m2)['gunallTRUE', 1],
      confint(m2)['`imean(ovp)`', 1], confint(m2)['`imean(lc)`', 1], confint(m2)['`imean(pj)`', 1], confint(m2)['`imean(gunallTRUE)`', 1]
    ),
    ciupp = c(
      confint(m1)['ovp', 2], confint(m1)['lc', 2], NA, NA,
      confint(m1)['`imean(ovp)`', 2], confint(m1)['`imean(lc)`', 2], NA, NA,
      confint(m2)['ovp', 2], confint(m2)['lc', 2], confint(m2)['pj', 2], confint(m2)['gunallTRUE', 2],
      confint(m2)['`imean(ovp)`', 2], confint(m2)['`imean(lc)`', 2], confint(m2)['`imean(pj)`', 2], confint(m2)['`imean(gunallTRUE)`', 2]
    )
  )

plot.wbm <- ggplot(wbm.plot_data, aes(y = coef, x = variables, colour = model, group = model)) +
  geom_errorbar(aes(ymin = cilow, ymax = ciupp), size = 0.5, width = .25, position = position_dodge(), show.legend = T) + 
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  facet_grid(. ~ est, switch = "y")+#, scales = "free") +
  ylim(-.8,.8) +
  coord_flip() + 
  ylab("") + xlab("") + 
  labs(caption = 'Within-between (hybrid) regression models estimated using R\'s package panelr.
                  Estimated coefficients and 95% confidence intervals reported.
                  Within-individual and between-individual effects reported. \n
       Both models control for race, sex, social class, and neighborhood of residence.
       Model 4 includes perceptions of procedural fairness and a recent police stop as control variables. \n
       N = 2929 and n = 975 for Model 2A.
       N = 2923 and n = 969 for Model 2B.') + 
  guides(colour = guide_legend(title = "")) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 10)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 10),
        legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 10),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(0, 0, 0, 0, "cm")) + 
  theme(aspect.ratio = 1.75) + 
  scale_x_discrete(limits = c('gunall', 'pj', 'lc', 'ovp'),
                   breaks = c('gunall', 'pj', 'lc', 'ovp'),
                   labels = c('Police stop at gunpoint', 'Perceived procedural fairness',
                              'Cynicism about police protection', 'Perceived police intrusion')) + 
  scale_colour_brewer(palette = "Set1", 
                      breaks = c('m1', 'm2'),
                      labels = c('M3', 'M4'))

pdf('plots/wbm_legitimacy.pdf')
plot.wbm
dev.off()


