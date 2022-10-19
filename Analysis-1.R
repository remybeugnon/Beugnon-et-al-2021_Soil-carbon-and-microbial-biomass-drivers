# R version 4.1.3 (2022-03-10)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Monterey 12.6
# Packages: 
# - dplyr_1.0.9
# - stringr_1.4.0
# - arm_1.13-1
# - lavaan_0.6-11
# - readxl_1.4.0
# - ggpubr_0.4.0
# - ggplot2_3.3.5
# - ggnewscale_0.4.7
# - extrafont_0.18
# - cowplot_1.1.1

#### START ####
rm(list = ls())

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
#### >> 2.1 Topography effect on historical soil C concentration ####
r.1 = 'Soil.C.2010'
e.1 = c('CURV_PR', 'CURV_PL', 'SLOPE', 'ALTITUDE')
df.1 = df %>% select(all_of(c(r.1,e.1))) %>% apply(., 2,rescale) %>% data.frame()
form.1 =  as.formula('
      # Response
      Soil.C.2010 ~
      # Explanatory
      CURV_PR + CURV_PL + SLOPE + ALTITUDE')

# Fit model
model.1 = lm(data = df.1, formula = form.1)
# Model selection based on AIC
model.step.1 = step(model.1, direction = 'both', trace = F)
# Summary
summary(model.step.1)

#### >> 2.2 Soil history, topography and tree diversity treatment effects on soil carbon concentration ####

r.2 = 'Soil.C.2018'
e.2 = c('CURV_PR', 'CURV_PL', 'SLOPE', 'ALTITUDE', 'Soil.C.2010', 'Sp.rich')
df.2 = df %>% select(all_of(c(r.2,e.2))) %>% apply(., 2,rescale) %>% data.frame()

form.2 =  as.formula('
      # Response
      Soil.C.2018 ~
      # Explanatory
      ## Topography
      CURV_PR + CURV_PL + SLOPE + 
      ## Diversity treatment
      Sp.rich + 
      ## Soil cencentration in 2010
      Soil.C.2010')

# Fit model
model.2 = lm(data = df.2, formula = form.2)
# Model selection based on AIC
model.step.2 = step(model.2, direction = 'both', trace = F)
# Summary
summary(model.step.2)

#### >> 2.3 Trait based model ####
# Data
r.3 = 'Soil.C.2018'
e.3 = c('ALTITUDE','CURV_PL', 'CURV_PR',  'SLOPE', 
        'Soil.C.2010', 
        'C.litterfall', 'CN.litterfall', 
        'neigh.biomass',
        'ENL', 
        'FDis.SRL', 'FDis.RD', 'FDis.AM.ECM',
        'SRL', 'RD', 'AM.ECM')
df.3 = df %>% 
  select(all_of(c(r.3,e.3))) %>% 
  apply(., 2,rescale) %>% data.frame()

# Formula
form.3 = as.formula('
      # Response
      Soil.C.2018 ~
      
      # Explanatory
      ## Topography
      CURV_PR + CURV_PL + SLOPE + 
      
      ## Litterfall of leaf C and N
      C.litterfall + CN.litterfall +
      
      ## Tree biomass
      neigh.biomass + 
      
      ## Crown structure
      ENL +

      ## Traits at neighborhood level
      FDis.SRL + FDis.RD + FDis.AM.ECM +
      SRL + RD + AM.ECM +
      
      ## Soil content in 2010
      Soil.C.2010')

# Fit model
model.3 = lm(data = df.3, 
             formula = form.3)

# Model selection based on AIC
model.step.3 = step(model.3, 
                    direction = 'both', 
                    trace = T)

# Summary
summary(model.step.3)

#### >> 2.4 Tree species richness effects on forest productivity ####
mod.neigh.biomass = lm(data = df %>% 
                         select(Sp.rich, neigh.biomass) %>% 
                         apply(.,2,rescale) %>% data.frame(), 
                     formula = neigh.biomass ~ Sp.rich) %>% 
  summary

mod.lit = lm(data = df %>% select(Sp.rich, C.litterfall) %>% 
               apply(.,2,rescale) %>% 
               data.frame(), 
             formula = C.litterfall ~ Sp.rich) %>% 
  summary

mod.lit.CN = lm(data = df %>% 
                  select(Sp.rich, CN.litterfall) %>% 
                  apply(.,2,rescale) %>% 
                  data.frame(), 
             formula = CN.litterfall ~ Sp.rich) %>% 
  summary

mod.ENL = lm(data = df %>% select(Sp.rich, ENL, SLOPE, CURV_PL, CURV_PR) %>% 
               apply(.,2,rescale) %>% data.frame(), 
             formula = ENL ~ Sp.rich + SLOPE + CURV_PL + CURV_PR) %>% 
  step(direction = 'both', trace = F) %>% 
  summary

#### >> 2.5 SEM Model ####
v.4 = c('CURV_PR', 'CURV_PL', 'SLOPE', 'ALTITUDE', 
        'Soil.C.2010', 'Soil.C.2018', 
        'Sp.rich',
        'C.litterfall', 'CN.litterfall', 
        'neigh.biomass', 
        'ENL', 
        'FDis', 'FDis.SRL', 'FDis.RD', 'FDis.AM.ECM',
        'SRL', 'RD', 'AM.ECM')

df.4 = df %>% 
  select(all_of(v.4)) %>% 
  apply(., 2, rescale) %>% 
  data.frame()

mod = '
Soil.C.2018 ~ Soil.C.2010 + 
              CURV_PR +
              CN.litterfall + ENL + 
              RD
Soil.C.2010 ~ SLOPE + CURV_PL
ENL ~ Sp.rich + SLOPE + CURV_PL + CURV_PR
'

# Fitting
fit = sem(mod,df.4)

# Model summary
summary(fit, standardized = T)
inspect(fit, 'R2')
fitMeasures(fit)

#### ~~~~~~~~~~~~~~~~~ ####
#### > 3. Plot results ####
#### ~~~~~~~~~~~~~~~~~ ####
text.size = 13
#### >> 3.1 Carbon balance ####
p.C.balance = 
  ggplot(data = df, 
         aes(x = 1, 
             y = (Soil.C.2018 - Soil.C.2010)/8)) + # 8 years 
  geom_violin(width = 1, 
              fill = 'gray90', 
              colour = 'gray60') +
  ggforce::geom_sina() +
  geom_boxplot(width = 0.5) + 
  geom_hline(yintercept = 0, size = 1, lty = 3, color = 'black') + 
  theme(panel.background = element_rect(fill = 'white', 
                                        color = 'gray'), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.text.x =  element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(t = 1, r = .5, b = 1, l = .5, "cm")) + 
  coord_cartesian(xlim = c(.5,1.5), ylim = c(-3,2), clip = 'off') + 
  annotate(geom = 'text', x = -1, y = -.5, label = 'C concentration variation (g/kg/year)', angle = 90)  
  
# p.C.balance

#### >> 3.2 Soil carbon stock drivers 2010 ####
# Extract summary
sum.mod.1 = data.frame(summary(model.step.1)$coefficients)
sum.mod.1$vars = 
  sum.mod.1 %>%
  rownames %>%
  as.factor

# Built table to plot
df.plot.1 = 
  sum.mod.1 %>%  
  filter(rownames(.) != "(Intercept)") %>%                  # Remove Intercept row
  dplyr::select(Estimate, 'SE' = Std..Error, 'p-value'=Pr...t..,vars) %>%   # Rename
  right_join(x = . , y = data.frame(vars = e.1), by = c('vars')) # Add levels and labels

# Set names as factors
df.plot.1$name = df.plot.1$vars %>%
  str_replace_all('SLOPE', 'Slope') %>%
  str_replace_all('CURV_PR', 'Curvature PR') %>%
  str_replace_all('CURV_PL', 'Curvature PL') %>%
  str_replace_all('ALTITUDE', 'Altitude') %>%
  factor

# Calculate confidence interval 
df.plot.1$ymax = df.plot.1$Estimate + 1.96 * df.plot.1$SE
df.plot.1$ymin = df.plot.1$Estimate - 1.96 * df.plot.1$SE

# Identify significant explanatory variables
df.plot.1$alpha = NA
df.plot.1$alpha[(df.plot.1$ymax * df.plot.1$ymin) > 0] = 's'
df.plot.1$alpha[(df.plot.1$ymax * df.plot.1$ymin) < 0] = 'n.s'

p.soil.2010.drivers = 
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
       y = 'Estimates') +
  coord_flip() +
  ggtitle("  ") +
  theme(legend.position = 'none', 
        axis.text = element_text(size = text.size),
        axis.text.x = element_text(size = text.size - 4),
        axis.title = element_text(size = text.size + 2),
        panel.background = element_rect(fill = NA, color = 'gray'),
        plot.margin = margin(t = 1, r = .5, b = .5, l = .5, "cm")
  )

# p.soil.2010.drivers

#### >> 3.3. Diversity effect on soil carbon C ####
sum.mod.2 = data.frame(summary(model.step.2)$coefficients)
sum.mod.2$vars = 
  sum.mod.2 %>%
  rownames %>%
  as.factor

# Built table to plot
df.plot.2 = 
  sum.mod.2 %>%  
  filter(rownames(.) != "(Intercept)") %>%                  # Remove Intercept row
  dplyr::select(Estimate, 'SE'=Std..Error, 'p-value'=Pr...t..,vars) %>%   # Rename
  right_join(x = . , y = data.frame(vars = e.2), by = c('vars')) # Add levels and labels

# Set names as factors
df.plot.2$name = df.plot.2$vars %>%
  str_replace_all('SLOPE', 'Slope') %>%
  str_replace_all('CURV_PR', 'Curvature PR') %>%
  str_replace_all('CURV_PL', 'Curvature PL') %>%
  str_replace_all('ALTITUDE', 'Altitude') %>%
  str_replace_all('Sp.rich', 'Sp. rich.') %>%
  str_replace_all('Soil.C.2010', '[C] 2010') %>%
  factor()

df.plot.2$color = c(rep('black', 4), 'brown', 'green', 'green')
# Calculate confidence interval 
df.plot.2$ymax = df.plot.2$Estimate + 1.96 * df.plot.2$SE
df.plot.2$ymin = df.plot.2$Estimate - 1.96 * df.plot.2$SE

# Identify significant explanatory variables
df.plot.2$lty = 3
df.plot.2$lty[df.plot.2$`p-value` < 0.05] = 1

# Plot
p.soil.2018.drivers.2 = 
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
        axis.text.x = element_blank(),
        axis.title = element_text(size = text.size + 2),
        panel.background = element_rect(fill = NA, color = 'gray'),
        plot.margin = margin(t = 1, r = .1, b = .5, l = .2, "cm"))

# p.soil.2018.drivers.2

#### >> 3.4. Traits effect on soil carbon C ####
sum.mod.3 = data.frame(summary(model.step.3)$coefficients)
sum.mod.3$vars = 
  sum.mod.3 %>%
  rownames %>%
  as.factor

# Built table to plot
df.plot.3 = 
  sum.mod.3 %>%  
  filter(rownames(.) != "(Intercept)") %>%                  # Remove Intercept row
  dplyr::select(Estimate, 'SE'=Std..Error, 'p-value'=Pr...t..,vars) %>%   # Rename
  right_join(x = . , y = data.frame(vars = e.3), by = c('vars')) # Add levels and labels

# Set names as factors
df.plot.3$name = df.plot.3$vars %>%
  str_replace_all('SLOPE', 'Slope') %>%
  str_replace_all('CURV_PR', 'Curvature PR') %>%
  str_replace_all('CURV_PL', 'Curvature PL') %>%
  str_replace_all('ALTITUDE', 'Altitude') %>%
  str_replace_all('\\.', ' ') %>% 
  str_replace_all('AM ECM', 'AM/EM') %>%
  str_replace_all('FDis AM ECM', 'FDis AM/EM') %>%
  str_replace_all('Soil C 2010', '[C] 2010')

e.3 = e.3 %>%
  str_replace_all('SLOPE', 'Slope') %>%
  str_replace_all('CURV_PR', 'Curvature PR') %>%
  str_replace_all('CURV_PL', 'Curvature PL') %>%
  str_replace_all('ALTITUDE', 'Altitude') %>%
  str_replace_all('\\.', ' ') %>% 
  str_replace_all('AM ECM', 'AM/EM') %>%
  str_replace_all('FDis AM ECM', 'FDis AM/EM') %>%
  str_replace_all('Soil C 2010', '[C] 2010')

df.plot.3$name = df.plot.3$name %>%
  factor(levels = e.3)

# df.plot.3$color = c(rep('black', 4), 'brown', 'green', 'green')
# Calculate confidence interval 
df.plot.3$ymax = df.plot.3$Estimate + 1.96 * df.plot.3$SE
df.plot.3$ymin = df.plot.3$Estimate - 1.96 * df.plot.3$SE

# Identify significant explanatory variables
df.plot.3$lty = 3
df.plot.3$lty[df.plot.3$`p-value` < 0.05] = 1

# Plot
p.soil.2018.drivers.3 = 
  ggplot(data = df.plot.3, 
         aes(y = Estimate, 
             x = name)) +
  geom_hline(aes(yintercept = 0), 
             color = 'gray') +
  geom_point() + 
  geom_errorbar(aes(ymax = ymax, 
                    ymin = ymin), 
                width = 0, 
                size = 1, 
                lty = df.plot.3$lty) +
  ylim(-1,1) +
  labs(x = '', 
       y = 'Estimates') +
  ggtitle(" ") +
  coord_flip() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = text.size),
        axis.text.x = element_text(size = text.size - 4),
        axis.title = element_text(size = text.size + 2),
        panel.background = element_rect(fill = NA, color = 'gray'),
        plot.margin = margin(t = 1.5, r = .1, b = 1, l = .2, "cm"))

# p.soil.2018.drivers.3

#### >> 3.5. Diversity effect on productivity ####
df.plot.4 = data.frame(
  resp = c('neigh. biomass', 'C litterfall', bquote('ENL')),
  div.effec = c(mod.neigh.biomass$coefficients['Sp.rich', 'Estimate'], 
                mod.lit$coefficients['Sp.rich', 'Estimate'],
                mod.ENL$coefficients['Sp.rich', 'Estimate']),
  pval = c(mod.neigh.biomass$coefficients['Sp.rich', 'Pr(>|t|)'], 
           mod.lit$coefficients['Sp.rich', 'Pr(>|t|)'], 
           mod.ENL$coefficients['Sp.rich', 'Pr(>|t|)'])
)

df.plot.4$div.effec = df.plot.4$div.effec %>% round(.,3) 
df.plot.4$pval = df.plot.4$pval %>% round(.,4) 
df.plot.4$sig = NA
df.plot.4$sig[df.plot.4$pval<0.05] = '*'
df.plot.4$sig[df.plot.4$pval<0.01] = '**'
df.plot.4$sig[df.plot.4$pval<0.001] = '***'
df.plot.4$write = paste0(df.plot.4$div.effec, df.plot.4$sig)
df.plot.4$write[df.plot.4$pval>0.05] = 'n.s.'

p.sp.rich =
  ggplot(df.plot.4, aes(x = 1, y = resp, label = write)) + 
  geom_text(size = (text.size - 8)) + 
  labs(x = '', y = '') + 
  ggtitle(" ") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = text.size),
        plot.margin = margin(t = 1.5, r = .5, b = .1, l = .5, "cm"))

# p.sp.rich

#### >> 3.6. Together ####

p.l = ggarrange(p.C.balance, p.soil.2010.drivers, p.sp.rich, nrow = 3, heights = c(.3,.4, .3), align = 'v') 
p.r = ggarrange(p.soil.2018.drivers.2, p.soil.2018.drivers.3, nrow = 2, heights = c(.25,.75), align = 'v')
p = ggarrange(p.l,p.r, ncol = 2)

ggsave(p, filename = 'Fig2-plots.png', width = 17, height = 30, unit = 'cm')

parameterEstimates(fit, standardized = T) %>% 
  select(lhs, op, rhs, std.all, pvalue)
inspect(fit, 'R2')

save.image("H1-mod.RData")