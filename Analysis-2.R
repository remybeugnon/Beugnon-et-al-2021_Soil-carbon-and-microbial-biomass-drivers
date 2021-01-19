#### START ####
rm(list = ls())
setwd("C:/Users/rb44vuni/Nextcloud/Remy's Thesis/Beugnon et al. 2020")

#### ~~~~~~~~~~~~~ ####
#### > 0. Packages ####
#### ~~~~~~~~~~~~~ ####
libs <- c(
  # Data
  'dplyr','stringr','arm','lavaan','readxl',
  # Plot 
  'ggpubr', 'ggplot2', 'ggnewscale', 
  'extrafont', 'cowplot'
)
invisible(lapply(libs, library, character.only = T))
select = dplyr::select
#### ~~~~~~~~~~~~~~~~~~ ####
#### > 1. Data handling ####
#### ~~~~~~~~~~~~~~~~~~ ####
# Loading data

df = read.csv('data-analyses.csv')
df$CN.litterfall = df$C.litterfall / df$N.litterfall
#### ~~~~~~~~~~~~~~~~~~~~~~~~~ ####
#### > 2. Statistical analyses ####
#### ~~~~~~~~~~~~~~~~~~~~~~~~~ ####
#### >> 2.1 Diversity effect ####

r.1 = 'mic.bio'
e.1 = c('Sp.rich', 'homo.hetero')
df.1 = df %>% select(all_of(c(r.1,e.1))) %>% apply(., 2,rescale) %>% data.frame()

form.1 =  as.formula('
      # Response
      mic.bio ~ 
      ## Diversity treatment
      Sp.rich + homo.hetero')

# Fit model
model.1 = lm(data = df.1, formula = form.1)
# Model selection based on AIC
model.step.1 = step(model.1, direction = 'both', trace = F)
# Summary
summary(model.step.1)

#### >> 2.2 Trait-based model ####
# Data
r.2 = 'mic.bio'
e.2 = c('C.litterfall', 'CN.litterfall', 
        'neigh.biomass', 'TSP.biomass', 
        'ENL',  
        'TSP.FRic.SRL', 'TSP.FRic.RD','TSP.FRic.AM.ECM',
        'TSP.SRL', 'TSP.RD', 'TSP.AM.ECM',
        'FDis.SRL', 'FDis.RD', 'FDis.AM.ECM',
        'SRL', 'RD', 'AM.ECM')
df.2 = df %>% select(all_of(c(r.2,e.2))) %>% apply(., 2,rescale) %>% data.frame()

# Formula
form.2 = as.formula('
      # Response
      mic.bio ~
      
      # Explanatory
      ## Litterfall of leaf C and N
      C.litterfall + CN.litterfall +
      
      ## Tree biomass
      neigh.biomass + TSP.biomass + 
      
      ## Crown structure
      ENL +
      
      ## Trait at TSP level 
      TSP.SRL + TSP.RD + TSP.AM.ECM + 
      TSP.FRic.AM.ECM + TSP.FRic.SRL + TSP.FRic.RD +
      
      ## Traits at neighborhood level
      FDis.SRL + FDis.RD + FDis.AM.ECM +
      SRL + RD + AM.ECM')

# Fit model
model.2 = lm(data = df.2, formula = form.2)
# Model selection based on AIC
model.step.2 = step(model.2, direction = 'both', trace = F)
# Summary
summary(model.step.2)

#### >> 2.3 Link soil microbial biomass ~ soil C concentration ####
mod.C = lm(data = df %>% select(mic.bio, Soil.C.2018) %>% apply(.,2,rescale) %>% data.frame(), 
             formula = Soil.C.2018 ~ mic.bio) %>% summary


#### >> 2.4 SEM Model ####
v.4 = c('CURV_PR', 'CURV_PL', 'SLOPE', 'ALTITUDE', 
        'Soil.C.2010', 'Soil.C.2018', 
        'mic.bio',
        'Sp.rich', 'homo.hetero',
        'C.litterfall', 'CN.litterfall', 
        'neigh.biomass', 'TSP.biomass', 
        'ENL', 
        'TSP.SRL', 'TSP.RD', 'TSP.AM.ECM', 
        'TSP.FRic.AM.ECM', 'TSP.FRic.SRL', 'TSP.FRic.RD',
        'FDis.SRL', 'FDis.RD', 'FDis.AM.ECM',
        'SRL', 'RD', 'AM.ECM')
df.4 = df %>% select(all_of(v.4)) %>% apply(., 2, rescale) %>% data.frame()

#### >>> 2.4.1 Microbial biomass -> Soil C ####
mod.mic.C = '
Soil.C.2018 ~ Soil.C.2010 + 
              CURV_PL +
              CN.litterfall + ENL + 
              TSP.RD + RD + 
              mic.bio
Soil.C.2010 ~ SLOPE + CURV_PL
ENL ~ Sp.rich + SLOPE + CURV_PL + CURV_PR
mic.bio ~ ENL + TSP.SRL + TSP.RD + FDis.SRL + RD
'

# Fitting
fit.mic.C = sem(mod.mic.C,df.4)

#### >>> 2.4.2 Soil C -> Microbial biomass ####
mod.C.mic = '
Soil.C.2018 ~ Soil.C.2010 + 
              CURV_PL +
              CN.litterfall + ENL + 
              TSP.RD + RD
Soil.C.2010 ~ SLOPE + CURV_PL
ENL ~ Sp.rich + SLOPE + CURV_PL + CURV_PR
mic.bio ~ ENL + TSP.SRL + TSP.RD + FDis.SRL + RD + Soil.C.2018
'

# Fitting
fit.C.mic = sem(mod.C.mic,df.4)

#### >>> 2.4.3 Soil C <-> Microbial biomass ####
mod.C..mic = '
Soil.C.2018 ~ Soil.C.2010 + 
              CURV_PL +
              CN.litterfall + ENL + 
              TSP.RD + RD
Soil.C.2010 ~ SLOPE + CURV_PL
ENL ~ Sp.rich + SLOPE + CURV_PL + CURV_PR
mic.bio ~ ENL + TSP.SRL + TSP.RD + FDis.SRL + RD
mic.bio ~~ Soil.C.2018
'

# Fitting
fit.C..mic = sem(mod.C..mic,df.4)

#### >>> 2.4.4 Soil C -> Microbial biomass & Microbial biomass -> Soil C####
mod.C.mic.C = '
Soil.C.2018 ~ Soil.C.2010 + 
              CURV_PL +
              CN.litterfall + ENL + 
              TSP.RD + RD + 
              mic.bio
Soil.C.2010 ~ SLOPE + CURV_PL
ENL ~ Sp.rich + SLOPE + CURV_PL + CURV_PR
mic.bio ~ ENL + TSP.SRL + TSP.RD + FDis.SRL + RD + Soil.C.2018
'

# Fitting
fit.C.mic.C = sem(mod.C.mic.C,df.4)
summary(fit.C.mic.C)
inspect(fit.C.mic.C, 'R2')

summary(fit.C.mic)
inspect(fit.C.mic, 'R2')
# Model comparison 
fitMeasures(fit.C..mic)['aic']
fitMeasures(fit.C.mic.C)['aic']
fitMeasures(fit.C.mic)['aic']
fitMeasures(fit.mic.C)['aic']

fitMeasures(fit.C..mic)['cfi']
fitMeasures(fit.C.mic.C)['cfi']
fitMeasures(fit.C.mic)['cfi']
fitMeasures(fit.mic.C)['cfi']

#### ~~~~~~~~~~~~~~~~~ ####
#### > 3. Plot results ####
#### ~~~~~~~~~~~~~~~~~ ####
text.size = 13
#### >> 3.1 Microbial biomass vs. species richness ####
sum.mod.1 = data.frame(summary(model.step.1)$coefficients)
sum.mod.1$vars = 
  sum.mod.1 %>%
  rownames %>%
  as.factor

# Built table to plot
df.plot.1 = 
  sum.mod.1 %>%  
  filter(rownames(.) != "(Intercept)") %>%                  # Remove Intercept row
  dplyr::select(-t.value) %>%                                       # Remove t.value
  set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%   # Rename
  right_join(x = . , y = data.frame(vars = e.1), by = c('vars')) # Add levels and labels

# Set names as factors
df.plot.1$name = df.plot.1$vars %>%
  str_replace_all('Sp.rich', 'Neigh. sp. rich.') %>%
  str_replace_all('homo.hetero', 'TSP sp. rich.') %>%
  factor

# Calculate confidence interval 
df.plot.1$ymax = df.plot.1$Estimate + 1.96 * df.plot.1$SE
df.plot.1$ymin = df.plot.1$Estimate - 1.96 * df.plot.1$SE
# Identify significant explanatory variables
df.plot.1$lty = 3
df.plot.1$lty[df.plot.1$`p-value` < 0.05] = 1

# Identify significant explanatory variables

p.mic.sp = 
  ggplot(data = df.plot.1, 
         aes(y = Estimate, 
             x = name)) +
  geom_hline(aes(yintercept = 0), 
             color = 'gray') +
  geom_point() + 
  geom_errorbar(aes(ymax = ymax, 
                    ymin = ymin), 
                width = 0, 
                size = 1) +
  ylim(-1,1) +
  labs(x = '', 
       y = '') +
  ggtitle(" ") +
  coord_flip() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = text.size),
        axis.text.x = element_blank(),
        axis.title = element_text(size = text.size + 2),
        panel.background = element_rect(fill = NA, color = 'gray'),
        plot.margin = margin(t = 1, r = .1, b = .5, l = .2, "cm"))

# p.mic.sp

#### >> 3.2. Based model ####
sum.mod.2 = data.frame(summary(model.step.2)$coefficients)
sum.mod.2$vars = 
  sum.mod.2 %>%
  rownames %>%
  as.factor

# Built table to plot
df.plot.2 = 
  sum.mod.2 %>%  
  filter(rownames(.) != "(Intercept)") %>%                  # Remove Intercept row
  dplyr::select(-t.value) %>%                                       # Remove t.value
  set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%   # Rename
  right_join(x = . , y = data.frame(vars = e.2), by = c('vars')) # Add levels and labels

# Set names as factors
df.plot.2$name = df.plot.2$vars %>%
  str_replace_all('\\.', ' ')
df.plot.2$name = df.plot.2$name %>%
  str_replace_all('FDis AM ECM', 'FDis AM/EM') %>%
  str_replace_all('TSP AM ECM', 'TSP AM/EM') %>%
  str_replace_all('TSP FRic AM ECM', 'TSP FRic AM/EM') %>% 
  str_replace_all('AM ECM', 'AM/EM') 
df.plot.2$name = factor(df.plot.2$name, levels = df.plot.2$name)

# Calculate confidence interval 
df.plot.2$ymax = df.plot.2$Estimate + 1.96 * df.plot.2$SE
df.plot.2$ymin = df.plot.2$Estimate - 1.96 * df.plot.2$SE

# Identify significant explanatory variables
df.plot.2$lty = 3
df.plot.2$lty[df.plot.2$`p-value` < 0.05] = 1

# Plot
p.mic.trait = 
  ggplot(data = df.plot.2, 
         aes(y = Estimate, 
             x = name)) +
  geom_hline(aes(yintercept = 0), 
             color = 'gray') +
  geom_point() + 
  geom_errorbar(aes(ymax = ymax, 
                    ymin = ymin), 
                width = 0, 
                size = 1, 
                lty = df.plot.2$lty) +
  ylim(-1,1) +
  labs(x = '', 
       y = '') +
  ggtitle(" ") +
  coord_flip() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = text.size),
        axis.text.x = element_text(size = text.size - 4),
        axis.title = element_text(size = text.size + 2),
        panel.background = element_rect(fill = NA, color = 'gray'),
        plot.margin = margin(t = 1, r = .1, b = .5, l = .2, "cm"))

# p.mic.trait

#### >> 3.3. Microbial biomass on soil carbon C ####
p.mic.C = 
  ggplot(data = df, aes(x = mic.bio, y = Soil.C.2018)) + 
  geom_point() + geom_smooth(method = 'lm', color = 'gray') +
  labs(x = 'Microbial biomass (nmol/g)', 
       y = 'Soil carbon\nconcentration\n2018 (g/kg)') +
  theme_bw() + 
  theme(axis.title = element_text(size = 12))

#### >> 3.4 SEM directionality ####
dir = c('Soil C \u2192 Mic. bio.', 'Soil C \u2190 Mic. bio.',
        "Soil C \u2192 Mic. bio. &\nSoil C \u2190 Mic. bio.",'Soil C \u2194 Mic. bio.')
df.plot.4 = data.frame(
  dir =  factor(dir, levels = dir),
  AIC = c(fitMeasures(fit.C.mic)['aic'],fitMeasures(fit.mic.C)['aic'],
          fitMeasures(fit.C.mic.C)['aic'],fitMeasures(fit.C..mic)['aic']),
  CFI = c(fitMeasures(fit.C.mic)['cfi'],fitMeasures(fit.mic.C)['cfi'],
          fitMeasures(fit.C.mic.C)['cfi'],fitMeasures(fit.C..mic)['cfi']),
  RMSEA = c(fitMeasures(fit.C.mic)['rmsea'],fitMeasures(fit.mic.C)['rmsea'],
          fitMeasures(fit.C.mic.C)['rmsea'],fitMeasures(fit.C..mic)['rmsea']),
  SRMR = c(fitMeasures(fit.C.mic)['srmr'],fitMeasures(fit.mic.C)['srmr'],
          fitMeasures(fit.C.mic.C)['srmr'],fitMeasures(fit.C..mic)['srmr'])
)

df.plot.4$AIC = df.plot.4$AIC %>% round(0)
df.plot.4$CFI = df.plot.4$CFI %>% round(3)
df.plot.4$RMSEA = df.plot.4$RMSEA %>% round(3)
df.plot.4$SRMR = df.plot.4$SRMR %>% round(3)
p.dir =
  ggplot(data = df.plot.4, aes(x = 1, y = dir, label = write)) + 
  geom_text(data = df.plot.4, aes(x = '1', y = dir, label = AIC), size = (text.size - 8)) +
  geom_text(data = df.plot.4, aes(x = '2', y = dir, label = CFI), size = (text.size - 8)) +
  geom_text(data = df.plot.4, aes(x = '3', y = dir, label = RMSEA), size = (text.size - 8)) +
  geom_text(data = df.plot.4, aes(x = '4', y = dir, label = SRMR), size = (text.size - 8)) +
  labs(x = '', y = '') +
  scale_x_discrete(breaks = as.character(c(1,2,3,4)),labels = c('1' = 'AIC','2' = 'CFI', '3'='RMSEA', '4'='SRMR'))+
  ggtitle(" ") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = text.size),
        plot.margin = margin(t = .5, r = .5, b = .1, l = .5, "cm"))
p.dir
#### >> 3.5. Together ####

p.l = ggarrange(p.mic.sp, p.mic.trait , nrow = 2, heights = c(.25,.75), align = 'v') 
p.t = ggarrange(p.mic.C ,p.dir , ncol = 2, heights = c(.6,.3), align = 'h')

ggsave(p.l, filename = 'Fig3-plot-left.png', width = 10, height = 30, unit = 'cm')
ggsave(p.t, filename = 'Fig3-plot-top.png', width = 30, height = 6, unit = 'cm')

parameterEstimates(fit.C.mic.C, standardized = T) %>% 
  select(lhs, op, rhs, std.all, pvalue)
inspect(fit.C.mic.C, 'R2')

save.image("C:/Users/rb44vuni/Nextcloud/Remy's Thesis/Beugnon et al. 2020/Git_repository/Beugnon-et-al-2020_Abiotic-biotic-mediations-of-scale-dependent-tree-trait-effects-on-soil-carbon/H2-mod.RData")
