#### START ####
rm(list = ls())
# setwd("C:/Users/rb44vuni/Nextcloud/Remy's Thesis/Beugnon et al. 2020")

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

#### ~~~~~~~~~~~~~~~~~~~~~~~~~ ####
#### > 2. Statistical analyses ####
#### ~~~~~~~~~~~~~~~~~~~~~~~~~ ####
#### >> 2.1 Drivers of environmental conditions ####
e.1 = c('Sp.rich', 'homo.hetero')
e.2 = c('C.litterfall', 'CN.litterfall', 
        'neigh.biomass', 'TSP.biomass', 
        'ENL', 
        'TSP.FRic.SRL', 'TSP.FRic.RD','TSP.FRic.AM.ECM', 
        'TSP.SRL', 'TSP.RD', 'TSP.AM.ECM', 
        'FDis.SRL', 'FDis.RD', 'FDis.AM.ECM',
        'SRL', 'RD', 'AM.ECM')
e.2.2 = c('C.litterfall', 'CN.litterfall', 
          'neigh.biomass', 'TSP.biomass', 
          'ENL')
#### >>> 2.1.1 Drivers of temperature ####
r.1 = 'temperature' 
df.T.1 = df %>% select(all_of(c(r.1,e.1))) %>% apply(., 2,rescale) %>% data.frame()

form.T.1 =  as.formula('
      # Response
      temperature ~ 
      ## Diversity treatment
      Sp.rich + homo.hetero')

# Fit model
model.T.1 = lm(data = df.T.1, formula = form.T.1)
# Model selection based on AIC
model.step.T.1 = step(model.T.1, direction = 'both', trace = F)
# Summary
summary(model.step.T.1)

# Trait based model 
df.T.2 = df %>% select(all_of(c(r.1,e.2.2))) %>% apply(., 2,rescale) %>% data.frame()

form.T.2 =  as.formula('
      # Response
      temperature ~ 
      # Explanatory
      ## Litterfall of leaf C and N
      C.litterfall + CN.litterfall +
      
      ## Tree biomass
      neigh.biomass + TSP.biomass + 
      
      ## Crown structure
      ENL')

# Fit model
model.T.2 = lm(data = df.T.2, formula = form.T.2)
# Model selection based on AIC
model.step.T.2 = step(model.T.2, direction = 'both', trace = F)
# Summary
summary(model.step.T.2)

#### >>> 2.1.2 Drivers of soil humidity ####
r.1 = 'Soil.humidity' 
df.RH.1 = df %>% select(all_of(c(r.1,e.1))) %>% apply(., 2,rescale) %>% data.frame()

form.RH.1 =  as.formula('
      # Response
      Soil.humidity ~ 
      ## Diversity treatment
      Sp.rich + homo.hetero')

# Fit model
model.RH.1 = lm(data = df.RH.1, formula = form.RH.1)
# Model selection based on AIC
model.step.RH.1 = step(model.RH.1, direction = 'both', trace = F)
# Summary
summary(model.step.RH.1)

# Trait based model 
df.RH.2 = df %>% select(all_of(c(r.1,e.2))) %>% apply(., 2,rescale) %>% data.frame()

form.RH.2 =  as.formula('
      # Response
      Soil.humidity ~
      
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
model.RH.2 = lm(data = df.RH.2, formula = form.RH.2)
# Model selection based on AIC
model.step.RH.2 = step(model.RH.2, direction = 'both', trace = F)
# Summary
summary(model.step.RH.2)


#### >>> 2.1.3 Soil N ####
r.1 = 'Soil.N.2018' 
df.N.1 = df %>% select(all_of(c(r.1,e.1))) %>% apply(., 2,rescale) %>% data.frame()

form.N.1 =  as.formula('
      # Response
      Soil.N.2018 ~ 
      ## Diversity treatment
      Sp.rich + homo.hetero')

# Fit model
model.N.1 = lm(data = df.N.1, formula = form.N.1)
# Model selection based on AIC
model.step.N.1 = step(model.N.1, direction = 'both', trace = F)
# Summary
summary(model.step.N.1)

# Trait based model 
df.N.2 = df %>% select(all_of(c(r.1,e.2))) %>% apply(., 2,rescale) %>% data.frame()

form.N.2 =  as.formula('
      # Response
      Soil.N.2018 ~
      
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
      
      ## Traits at neighboNood level
      FDis.SRL + FDis.RD + FDis.AM.ECM +
      SRL + RD + AM.ECM')

# Fit model
model.N.2 = lm(data = df.N.2, formula = form.N.2)
# Model selection based on AIC
model.step.N.2 = step(model.N.2, direction = 'both', trace = F)
# Summary
summary(model.step.N.2)


#### >>> 2.1.4 Soil P ####
r.1 = 'Soil.P.2018' 
df.P.1 = df %>% select(all_of(c(r.1,e.1))) %>% apply(., 2,rescale) %>% data.frame()

form.P.1 =  as.formula('
      # Response
      Soil.P.2018 ~ 
      ## Diversity treatment
      Sp.rich + homo.hetero')

# Fit model
model.P.1 = lm(data = df.P.1, formula = form.P.1)
# Model selection based on AIC
model.step.P.1 = step(model.P.1, direction = 'both', trace = F)
# Summary
summary(model.step.P.1)

# Trait based model 
df.P.2 = df %>% select(all_of(c(r.1,e.2))) %>% apply(., 2,rescale) %>% data.frame()

form.P.2 =  as.formula('
      # Response
      Soil.P.2018 ~
      
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
      
      ## Traits at neighboNood level
      FDis.SRL + FDis.RD + FDis.AM.ECM +
      SRL + RD + AM.ECM')

# Fit model
model.P.2 = lm(data = df.P.2, formula = form.P.2)
# Model selection based on AIC
model.step.P.2 = step(model.P.2, direction = 'both', trace = F)
# Summary
summary(model.step.P.2)

#### >>> 2.1.5  Plant abundance ####
r.1 = 'Plant.ab' 
df.pl.ab.1 = df %>% select(all_of(c(r.1,e.1))) %>% apply(., 2,rescale) %>% data.frame()

form.pl.ab.1 =  as.formula('
      # Response
      Plant.ab ~ 
      ## Diversity treatment
      Sp.rich + homo.hetero')

# Fit model
model.pl.ab.1 = lm(data = df.pl.ab.1, formula = form.pl.ab.1)
# Model selection based on AIC
model.step.pl.ab.1 = step(model.pl.ab.1, direction = 'both', trace = F)
# Summary
summary(model.step.pl.ab.1)

# Trait based model 
df.pl.ab.2 = df %>% select(all_of(c(r.1,e.2))) %>% apply(., 2,rescale) %>% data.frame()

form.pl.ab.2 =  as.formula('
      # Response
      Plant.ab ~
      
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
      
      ## Traits at neighboNood level
      FDis.SRL + FDis.RD + FDis.AM.ECM +
      SRL + RD + AM.ECM')

# Fit model
model.pl.ab.2 = lm(data = df.pl.ab.2, formula = form.pl.ab.2)
# Model selection based on AIC
model.step.pl.ab.2 = step(model.pl.ab.2, direction = 'both', trace = F)
# Summary
summary(model.step.pl.ab.2)

#### >>> 2.1.6  Root abundance ####
r.1 = 'root.ab' 
df.rt.ab.1 = df %>% select(all_of(c(r.1,e.1))) %>% apply(., 2,rescale) %>% data.frame()

form.rt.ab.1 =  as.formula('
      # Response
      root.ab ~ 
      ## Diversity treatment
      Sp.rich + homo.hetero')

# Fit model
model.rt.ab.1 = lm(data = df.rt.ab.1, formula = form.rt.ab.1)
# Model selection based on AIC
model.step.rt.ab.1 = step(model.rt.ab.1, direction = 'both', trace = F)
# Summary
summary(model.step.rt.ab.1)

# Trait based model 
df.rt.ab.2 = df %>% select(all_of(c(r.1,e.2))) %>% apply(., 2,rescale) %>% data.frame()

form.rt.ab.2 =  as.formula('
      # Response
      root.ab ~
      
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
      
      ## Traits at neighboNood level
      FDis.SRL + FDis.RD + FDis.AM.ECM +
      SRL + RD + AM.ECM')

# Fit model
model.rt.ab.2 = lm(data = df.rt.ab.2, formula = form.rt.ab.2)
# Model selection based on AIC
model.step.rt.ab.2 = step(model.rt.ab.2, direction = 'both', trace = F)
# Summary
summary(model.step.rt.ab.2)

#### >>> 2.1.7  Litter abundance ####
r.1 = 'litter.ab' 
df.lit.ab.1 = df %>% select(all_of(c(r.1,e.1))) %>% apply(., 2,rescale) %>% data.frame()

form.lit.ab.1 =  as.formula('
      # Response
      litter.ab ~ 
      ## Diversity treatment
      Sp.rich + homo.hetero')

# Fit model
model.lit.ab.1 = lm(data = df.lit.ab.1, formula = form.lit.ab.1)
# Model selection based on AIC
model.step.lit.ab.1 = step(model.lit.ab.1, direction = 'both', trace = F)
# Summary
summary(model.step.lit.ab.1)

# Trait based model 
df.lit.ab.2 = df %>% select(all_of(c(r.1,e.2))) %>% apply(., 2,rescale) %>% data.frame()

form.lit.ab.2 =  as.formula('
      # Response
      litter.ab ~
      
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
      
      ## Traits at neighboNood level
      FDis.SRL + FDis.RD + FDis.AM.ECM +
      SRL + RD + AM.ECM')

# Fit model
model.lit.ab.2 = lm(data = df.lit.ab.2, formula = form.lit.ab.2)
# Model selection based on AIC
model.step.lit.ab.2 = step(model.lit.ab.2, direction = 'both', trace = F)
# Summary
summary(model.step.lit.ab.2)

#### >>> 2.1.8  Litter CN ####
r.1 = 'litter.CN' 
df.lit.CN.1 = df %>% select(all_of(c(r.1,e.1))) %>% apply(., 2,rescale) %>% data.frame()

form.lit.CN.1 =  as.formula('
      # Response
      litter.CN ~ 
      ## Diversity treatment
      Sp.rich + homo.hetero')

# Fit model
model.lit.CN.1 = lm(data = df.lit.CN.1, formula = form.lit.CN.1)
# Model selection based on AIC
model.step.lit.CN.1 = step(model.lit.CN.1, direction = 'both', trace = F)
# Summary
summary(model.step.lit.CN.1)

# Trait based model 
df.lit.CN.2 = df %>% select(all_of(c(r.1,e.2))) %>% apply(., 2,rescale) %>% data.frame()

form.lit.CN.2 =  as.formula('
      # Response
      litter.CN ~
      
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
      
      ## Traits at neighboNood level
      FDis.SRL + FDis.RD + FDis.AM.ECM +
      SRL + RD + AM.ECM')

# Fit model
model.lit.CN.2 = lm(data = df.lit.CN.2, formula = form.lit.CN.2)
# Model selection based on AIC
model.step.lit.CN.2 = step(model.lit.CN.2, direction = 'both', trace = F)
# Summary
summary(model.step.lit.CN.2)


#### >> 2.2 Environment effect on microbial community ####
r = 'mic.bio'
e = c('Soil.humidity','temperature',
      'Soil.N.2018','Soil.P.2018',
      'Plant.ab','root.ab','litter.CN','litter.ab')
df.rs = df %>% select(all_of(c(r,e))) %>% apply(., 2,rescale) %>% data.frame()
form = as.formula('mic.bio ~

      # Microclimate variables
      Soil.humidity + temperature +

      # Soil quality
      Soil.N.2018 + Soil.P.2018 + 

      # Biotic environment
      Plant.ab + root.ab + litter.CN + litter.ab')

mod = lm(form, data = df.rs)
mod.step = step(mod)
summary(mod.step)

#### >> 2.3 SEM Model ####
v.4 = c('CURV_PR', 'CURV_PL', 'SLOPE', 'ALTITUDE', 
        'Soil.C.2010', 'Soil.C.2018', 
        'mic.bio',
        'Soil.humidity','temperature',
        'Soil.N.2018','Soil.P.2018',
        'Plant.ab','root.ab','litter.CN','litter.ab',
        'Sp.rich', 'homo.hetero',
        'C.litterfall', 'CN.litterfall', 
        'neigh.biomass', 'TSP.biomass', 
        'ENL', 
        'TSP.SRL', 'TSP.RD', 'TSP.AM.ECM', 
        'TSP.FRic.AM.ECM', 'TSP.FRic.SRL', 'TSP.FRic.RD',
        'FDis', 'FDis.SRL', 'FDis.RD', 'FDis.AM.ECM',
        'SRL', 'RD', 'AM.ECM')
df.4 = df %>% select(all_of(v.4)) %>% apply(., 2, rescale) %>% data.frame()

mod = '
Soil.C.2018 ~ Soil.C.2010 + 
              CURV_PL +
              CN.litterfall + ENL + 
              TSP.RD + RD
Soil.C.2010 ~ SLOPE + CURV_PL
ENL ~ Sp.rich + SLOPE + CURV_PL + CURV_PR
mic.bio ~ ENL + TSP.SRL + TSP.RD + FDis.SRL + RD + 
                Soil.C.2018 + 
                temperature + Soil.humidity + 
                Soil.N.2018 + 
                litter.CN
            
temperature ~ ENL 
Soil.humidity ~ CN.litterfall + TSP.SRL
Soil.N.2018 ~ CN.litterfall + FDis.SRL + SRL
litter.CN ~ CN.litterfall + ENL + TSP.FRic.RD + AM.ECM

'
# Fitting
fit = sem(mod,df.4)
summary(fit)
inspect(fit, 'R2')
fitMeasures(fit)

#### ~~~~~~~~~~~~~~~~~ ####
#### > 3. Plot results ####
#### ~~~~~~~~~~~~~~~~~ ####
text.size = 13
#### >> 3.1 Micro-environmental conditions and tree species richness ####
#### >> 3.1.1 Data ####
df.plot.1 = data.frame(label = c('Sp. rich','TSP. sp. rich') %>% as.factor(),
                       vars = e.1 %>% as.factor()) %>%
  left_join(., 
            summary(model.step.T.1)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('T_', colnames(.))),
            by = c('vars' = 'T_vars')
            ) %>%
  left_join(., 
            summary(model.step.RH.1)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('RH_', colnames(.))),
            by = c('vars' = 'RH_vars')
  ) %>%
  left_join(., 
            summary(model.step.N.1)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('N_', colnames(.))),
            by = c('vars' = 'N_vars')
  ) %>%
  left_join(., 
            summary(model.step.P.1)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('P_', colnames(.))),
            by = c('vars' = 'P_vars')
  ) %>%
  left_join(., 
            summary(model.step.pl.ab.1)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('pl.ab_', colnames(.))),
            by = c('vars' = 'pl.ab_vars')
  ) %>%
  left_join(., 
            summary(model.step.rt.ab.1)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('rt.ab_', colnames(.))),
            by = c('vars' = 'rt.ab_vars')
  ) %>%
  left_join(., 
            summary(model.step.lit.ab.1)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('lit.ab_', colnames(.))),
            by = c('vars' = 'lit.ab_vars')
  ) %>%
  left_join(., 
            summary(model.step.lit.CN.1)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('lit.CN_', colnames(.))),
            by = c('vars' = 'lit.CN_vars')
  )

#### >> 3.1.2 Plot ####
r.T = (summary(model.T.1)$adj.r.squared %>% round(2) * 100)
r.RH = 0 
r.N = (summary(model.N.1)$adj.r.squared %>% round(2) * 100)
r.P = 0
r.pl.ab = (summary(model.pl.ab.1)$adj.r.squared %>% round(2) * 100)
r.rt.ab = 0
r.lit.ab = (summary(model.lit.ab.1)$adj.r.squared %>% round(2) * 100)
r.lit.CN = 0

p.sp.T ={ 
  ggplot(data = df.plot.1, 
         aes(y = T_Estimate, 
             x = label)) +
  geom_hline(aes(yintercept = 0), 
             color = 'gray') +
  geom_point() + 
  geom_errorbar(aes(ymax = T_ymax, 
                    ymin = T_ymin), 
                width = 0, 
                size = 1,
                lty = df.plot.1$T_lty) +
  ylim(-1,1) +
  labs(y = bquote(R^2~" = " * .(r.T) * ' %'), 
       x = '') +
  ggtitle("Temperature") +
  coord_flip() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = text.size),
        axis.text.x = element_blank(),
        axis.title = element_text(size = text.size + 2),
        panel.background = element_rect(fill = NA, color = 'gray'),
        plot.margin = margin(t = 1, r = .1, b = .5, l = 1.1, "cm"))}
p.sp.T
p.sp.RH = {
  ggplot(data = df.plot.1, 
         aes(y = RH_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = RH_ymax, 
                      ymin = RH_ymin), 
                  width = 0, 
                  size = 1,
                  lty = df.plot.1$RH_lty) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.RH) * ' %'), 
         x = '') +
    ggtitle("Soil humidity") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text = element_blank(),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.N = {
  ggplot(data = df.plot.1, 
         aes(y = N_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = N_ymax, 
                      ymin = N_ymin), 
                  width = 0, 
                  size = 1,
                  lty = df.plot.1$N_lty) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.N) * ' %'), 
         x = '') +
    ggtitle("Soil N 2018") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text = element_blank(),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.P = {
  ggplot(data = df.plot.1, 
         aes(y = P_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = P_ymax, 
                      ymin = P_ymin), 
                  width = 0, 
                  size = 1,
                  lty = df.plot.1$P_lty) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.P) * ' %'), 
         x = '') +
    ggtitle("Soil P 2018") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text = element_blank(),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.pl.ab = {
  ggplot(data = df.plot.1, 
         aes(y = pl.ab_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = pl.ab_ymax, 
                      ymin = pl.ab_ymin), 
                  width = 0, 
                  size = 1,
                  lty = df.plot.1$pl.ab_lty) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.pl.ab) * ' %'), 
         x = '') +
    ggtitle("Plant abundance") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text = element_blank(),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.rt.ab = {
  ggplot(data = df.plot.1, 
         aes(y = rt.ab_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = rt.ab_ymax, 
                      ymin = rt.ab_ymin), 
                  width = 0, 
                  size = 1,
                  lty = df.plot.1$rt.ab_lty) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.rt.ab) * ' %'), 
         x = '') +
    ggtitle("Root biomass") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text = element_blank(),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.lit.ab = {
  ggplot(data = df.plot.1, 
         aes(y = lit.ab_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = lit.ab_ymax, 
                      ymin = lit.ab_ymin), 
                  width = 0, 
                  size = 1,
                  lty = df.plot.1$lit.ab_lty) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.lit.ab) * ' %'), 
         x = '') +
    ggtitle("Litter abundance") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text = element_blank(),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.lit.CN = {
  ggplot(data = df.plot.1, 
         aes(y = lit.CN_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = lit.CN_ymax, 
                      ymin = lit.CN_ymin), 
                  width = 0, 
                  size = 1,
                  lty = df.plot.1$lit.CN_lty) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.lit.CN) * ' %'), 
         x = '') +
    ggtitle("Litter CN") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text = element_blank(),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}

#### >> 3.2 Environmental condition and tree traits ####
#### >> 3.2.1 Data ####
r.T.2 = (summary(model.T.2)$adj.r.squared %>% round(2) * 100)
r.RH.2 = (summary(model.RH.2)$adj.r.squared %>% round(2) * 100)
r.N.2 = (summary(model.N.2)$adj.r.squared %>% round(2) * 100)
r.P.2 = (summary(model.P.2)$adj.r.squared %>% round(2) * 100)
r.pl.ab.2 = (summary(model.pl.ab.2)$adj.r.squared %>% round(2) * 100)
r.rt.ab.2 = (summary(model.rt.ab.2)$adj.r.squared %>% round(2) * 100)
r.lit.ab.2 = (summary(model.lit.ab.2)$adj.r.squared %>% round(2) * 100)
r.lit.CN.2 = (summary(model.lit.ab.2)$adj.r.squared %>% round(2) * 100)

df.plot.2 = data.frame(label = e.2 %>% as.factor(),
                       vars = e.2 %>% as.factor()) %>%
  left_join(., 
            summary(model.step.T.2)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('T_', colnames(.))),
            by = c('vars' = 'T_vars')
  ) %>%
  left_join(., 
            summary(model.step.RH.2)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('RH_', colnames(.))),
            by = c('vars' = 'RH_vars')
  ) %>%
  left_join(., 
            summary(model.step.N.2)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('N_', colnames(.))),
            by = c('vars' = 'N_vars')
  ) %>%
  left_join(., 
            summary(model.step.P.2)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('P_', colnames(.))),
            by = c('vars' = 'P_vars')
  ) %>%
  left_join(., 
            summary(model.step.pl.ab.2)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('pl.ab_', colnames(.))),
            by = c('vars' = 'pl.ab_vars')
  ) %>%
  left_join(., 
            summary(model.step.rt.ab.2)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('rt.ab_', colnames(.))),
            by = c('vars' = 'rt.ab_vars')
  ) %>%
  left_join(., 
            summary(model.step.lit.ab.2)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('lit.ab_', colnames(.))),
            by = c('vars' = 'lit.ab_vars')
  ) %>%
  left_join(., 
            summary(model.step.lit.CN.2)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)) %>%
              set_colnames(paste0('lit.CN_', colnames(.))),
            by = c('vars' = 'lit.CN_vars')
  )

df.plot.2$label =
  df.plot.2$label %>%
  str_replace_all('\\.', ' ') %>%
  str_replace_all('FDis AM ECM', 'FDis AM/EM') %>%
  str_replace_all('TSP AM ECM', 'TSP AM/EM') %>%
  str_replace_all('TSP FRic AM ECM', 'TSP FRic AM/EM') %>% 
  str_replace_all('AM ECM', 'AM/EM') %>%
  factor(., levels = .)

p.sp.T.2 = {
  ggplot(data = df.plot.2, 
         aes(y = T_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = T_ymax, 
                      ymin = T_ymin, 
                      lty = T_lty %>% as.factor()), 
                  width = 0, 
                  size = 1) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.T.2) * ' %'), 
         x = '') +
    ggtitle("") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text = element_text(size = text.size),
          axis.text.x = element_text(size = text.size - 4),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .2, "cm"))}
p.sp.RH.2 = {
  ggplot(data = df.plot.2, 
         aes(y = RH_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = RH_ymax, 
                      ymin = RH_ymin, 
                      lty = RH_lty %>% as.factor()), 
                  width = 0, 
                  size = 1) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.RH.2) * ' %'), 
         x = '') +
    ggtitle("") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = text.size - 4),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.N.2 = {
  ggplot(data = df.plot.2, 
         aes(y = N_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = N_ymax, 
                      ymin = N_ymin, 
                      lty = N_lty %>% as.factor()), 
                  width = 0, 
                  size = 1) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.N.2) * ' %'), 
         x = '') +
    ggtitle("") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = text.size - 4),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.P.2 = {
  ggplot(data = df.plot.2, 
         aes(y = P_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = P_ymax, 
                      ymin = P_ymin, 
                      lty = P_lty %>% as.factor()), 
                  width = 0, 
                  size = 1) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.P.2) * ' %'), 
         x = '') +
    ggtitle("") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = text.size - 4),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.pl.ab.2 = {
  ggplot(data = df.plot.2, 
         aes(y = pl.ab_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = pl.ab_ymax, 
                      ymin = pl.ab_ymin, 
                      lty = pl.ab_lty %>% as.factor()), 
                  width = 0, 
                  size = 1) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.pl.ab.2) * ' %'), 
         x = '') +
    ggtitle("") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = text.size - 4),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.rt.ab.2 = {
  ggplot(data = df.plot.2, 
         aes(y = rt.ab_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = rt.ab_ymax, 
                      ymin = rt.ab_ymin, 
                      lty = rt.ab_lty %>% as.factor()), 
                  width = 0, 
                  size = 1) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.rt.ab.2) * ' %'), 
         x = '') +
    ggtitle("") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = text.size - 4),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.lit.ab.2 = {
  ggplot(data = df.plot.2, 
         aes(y = lit.ab_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = lit.ab_ymax, 
                      ymin = lit.ab_ymin, 
                      lty = lit.ab_lty %>% as.factor()), 
                  width = 0, 
                  size = 1) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.lit.ab.2) * ' %'), 
         x = '') +
    ggtitle("") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = text.size - 4),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}
p.sp.lit.CN.2 = {
  ggplot(data = df.plot.2, 
         aes(y = lit.CN_Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = lit.CN_ymax, 
                      ymin = lit.CN_ymin, 
                      lty = lit.CN_lty %>% as.factor()), 
                  width = 0, 
                  size = 1) +
    ylim(-1,1) +
    labs(y = bquote(R^2~" = " * .(r.lit.CN.2) * ' %'), 
         x = '') +
    ggtitle("") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = text.size - 4),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .1, "cm"))}

p = ggarrange(p.sp.T, p.sp.RH, p.sp.N, p.sp.P, 
              p.sp.pl.ab, p.sp.rt.ab, p.sp.lit.ab, p.sp.lit.CN,
              p.sp.T.2, p.sp.RH.2, p.sp.N.2, p.sp.P.2, 
              p.sp.pl.ab.2, p.sp.rt.ab.2, p.sp.lit.ab.2, p.sp.lit.CN.2,
              ncol = 8, nrow = 2, widths = rep(c(.19,rep(.1,7)),2), heights = rep(c(2,8),8)
              )
p
#### >> 3.3. Environment effect on microbial biomass ####
df.plot.3 = data.frame(label = e %>% as.factor(),
                       vars = e %>% as.factor()) %>%
  left_join(., 
            summary(mod.step)$coefficients %>%
              data.frame() %>%
              mutate(vars = rownames(.)) %>%
              filter(vars != "(Intercept)") %>%                  # Remove Intercept row
              select(-t.value) %>%                                       
              set_colnames(c('Estimate', 'SE', 'p-value','vars')) %>%
              mutate(ymax = Estimate + 1.96 * SE, 
                     ymin = Estimate - 1.96 * SE,
                     lty = ifelse(`p-value` < 0.05, 1, 3)),
              by = 'vars')

df.plot.3$label =
  df.plot.3$label %>%
  str_replace_all('\\.', ' ') %>%
  str_replace_all('root ab', 'Root biomass') %>%
  str_replace_all('litter ab', 'Litter ab') %>%
  str_replace_all('litter CN', 'Litter CN') %>%
  str_replace_all('temperature', 'Temperature') %>%
  factor(., levels = rev(.))

p.env.mic = {
  ggplot(data = df.plot.3, 
         aes(y = Estimate, 
             x = label)) +
    geom_hline(aes(yintercept = 0), 
               color = 'gray') +
    geom_point() + 
    geom_errorbar(aes(ymax = ymax, 
                      ymin = ymin, 
                      lty = lty %>% as.factor()), 
                  width = 0, 
                  size = 1) +
    ylim(-1,1) +
    labs(x = '', 
         y = 'Esimate') +
    ggtitle("") +
    coord_flip() +
    theme(legend.position = 'none', 
          axis.text = element_text(size = text.size),
          axis.text.x = element_text(size = text.size - 4),
          axis.title = element_text(size = text.size + 2),
          panel.background = element_rect(fill = NA, color = 'gray'),
          plot.margin = margin(t = 1, r = .1, b = .5, l = .2, "cm"))}

ggsave(p, filename = 'Fig4-plot-top.png', width = 40, height = 25, unit = 'cm')
ggsave(p.env.mic, filename = 'Fig4-plot-left.png', width = 7, height = 21, unit = 'cm')

PARM = parameterEstimates(fit, standardized = T) %>%
  select(lhs, op, rhs, std.all, pvalue)
inspect(fit, 'R2')
fitMeasures(fit)

save.image("C:/Users/rb44vuni/Nextcloud/Remy's Thesis/Beugnon et al. 2020/Git_repository/Beugnon-et-al-2020_Abiotic-biotic-mediations-of-scale-dependent-tree-trait-effects-on-soil-carbon/H3-mod.RData")