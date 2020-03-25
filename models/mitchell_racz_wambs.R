# This is the model fitting procedure for the Mitchell paper.
# First, we load in data and fix factor type and factor levels.
# Then, we fit that one model with an interaction and without it.
# We go through the wambs checklist. More or less.
# Once they LGTM, we compare the two variants using waic and loo. We reloo when needed.
# We keep the model with a better overall elpd. If there is no robust difference in elpd we keep the model with less stuff in it.

## --------- header --------- ##

setwd('~/Github/MitchellRacz2020/')

options(mc.cores=parallel::detectCores())
library(tidyverse)
library(broom)
library(brms)
library(ggthemes)
library(bayesplot)
library(tidybayes)
library(patchwork)

## --------- data --------- ##

d = read_tsv('data/mitchell_racz_data.tsv')

d2 = d[!is.na(d$correct),]
d2$resp = as.numeric(d2$correct)
d2$education = factor(d2$education, levels = c('none', 'pre-school', 'class 1', 'class 2', 'class 5'))
d2$education.ordered = ordered(d2$education)

## --------- functions --------- ##

comparePosteriors = function(model1, model2, id1, id2){
  posts = posterior_samples(model1) %>% 
    rownames_to_column() %>% 
    pivot_longer(- rowname, names_to = 'predictor', values_to = 'draw') %>% 
    filter(str_detect(predictor, 'b_')) %>% 
    mutate(model = id1)
  posts2 = posterior_samples(model2) %>% 
    rownames_to_column() %>% 
    pivot_longer(- rowname, names_to = 'predictor', values_to = 'draw') %>% 
    filter(str_detect(predictor, 'b_')) %>% 
    mutate(model = id2) %>% 
    bind_rows(posts)
  
  p1 = posts2 %>% 
    ggplot(aes(x = draw, y = predictor, fill = model)) +
    geom_halfeyeh(alpha = .5) +
    scale_fill_brewer(palette = 'Dark2')
  
  return(p1)
}

wambsCheck = function(my_fit, my_data){
  
  my_formula = my_fit$formula
  my_data_r = sample_n(my_data, nrow(my_data))
  
  fit_unif = brm(formula = my_formula, data = my_data, family = bernoulli, save_all_pars = T, control=list(adapt_delta=0.99, stepsize = 0.01, max_treedepth =15))
  fit_long = brm(formula = my_formula, data = my_data, family = bernoulli, prior = gelman_hill_binom_priors, save_all_pars = T, iter = 5000, control=list(adapt_delta=0.99, stepsize = 0.01, max_treedepth =15))
  fit_rand = brm(formula = my_formula, data = my_data_r, family = bernoulli, prior = gelman_hill_binom_priors, save_all_pars = T, control=list(adapt_delta=0.99, stepsize = 0.01, max_treedepth =15))
  
  my_text = paste0(
    paste0('Rhat below 1.1 for fit: ', all(rhat(my_fit) < 1.1), '\n'),  
    paste0('Rhat below 1.1 with noninf prior: ', all(rhat(fit_unif) < 1.1), '\n'), 
    paste0('Rhat below 1.1 with 5000 iters: ', all(rhat(fit_long) < 1.1), '\n'), 
    paste0('Rhat below 1.1 with rand ord: ', all(rhat(fit_rand) < 1.1)) 
  )
  
  p1 = comparePosteriors(my_fit, fit_unif, 'weak prior', 'noninf prior') + ggtitle('non-informative prior')
  p2 = comparePosteriors(my_fit, fit_long, '2000 iter', '5000 iter') + ggtitle('more iterations')
  p3 = comparePosteriors(my_fit, fit_rand, 'orig', 'reshuffled') + ggtitle('reshuffled data')
  
  p4 = wrap_elements((grid::textGrob(my_text))) + p1 + p2 + p3
  
  return(p4)
}

## --------- weakly informative priors for a bernoulli model --------- ##

gelman_hill_binom_priors = c(
  prior(student_t(1, 0, 2.5), class = "Intercept"),
  prior(student_t(1, 0, 2.5), class = "b")
)

## --------- fit 1 --------- ##

fit1 = brm(resp ~ gender + task + c.age + location + education.ordered + (1|ID) + (1|word), data = d2, family = bernoulli, prior = gelman_hill_binom_priors, save_all_pars = T)
fit2 = brm(resp ~ gender * task + c.age + location + education.ordered + (1|ID) + (1|word), data = d2, family = bernoulli, prior = gelman_hill_binom_priors, control=list(adapt_delta=0.99, stepsize = 0.01, max_treedepth =15))
fit3 = brm(resp ~ gender + task + c.age + location + education.ordered + (1|word), data = d2, family = bernoulli, prior = gelman_hill_binom_priors, control=list(adapt_delta=0.99, stepsize = 0.01, max_treedepth =15))

crit1 = add_criterion(fit1, c('waic', 'loo'), reloo = T)
crit2 = add_criterion(fit2, c('waic', 'loo'), reloo = T)
crit3 = add_criterion(fit3, c('waic', 'loo'), reloo = T)

p1 = wambsCheck(fit1, d2)
p2 = wambsCheck(fit2, d2)
p3 = wambsCheck(fit3, d2)

ggsave(plot = p1, filename = 'models/figs/p1.pdf', width = 20, height = 10)
ggsave(plot = p2, filename = 'models/figs/p2.pdf', width = 20, height = 10)
ggsave(plot = p3, filename = 'models/figs/p3.pdf', width = 20, height = 10)

loo_compare(crit1, crit2)
loo_compare(crit1, crit2, crit3)
summary(fit1)
