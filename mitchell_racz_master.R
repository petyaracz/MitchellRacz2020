# code for the paper Mitchell and Rácz 2020

# --- setup --- #

setwd('~/Github/MitchellRacz2020/')
library(tidyverse)
library(knitr)
library(brms)
library(patchwork)
options(mc.cores=parallel::detectCores())
# options(mc.cores=1) # aaah make it stop
Sys.setenv(TZ="Europe/Rome")

# --- data --- #

d = read_tsv('data/mitchell_racz_data.tsv')

d2 = d[!is.na(d$correct),]
d2$resp = as.numeric(d2$correct)
d2$education = factor(d2$education, levels = c('none', 'pre-school', 'class 1', 'class 2', 'class 5'))
d2$education.ordered = ordered(d2$education)

# --- modelling --- #

## -- priors -- ##

gelman_hill_binom_priors = c(
  prior(student_t(1, 0, 2.5), class = "Intercept"),
  prior(student_t(1, 0, 2.5), class = "b")
)

## -- fit -- ##

fit1 = brm(resp ~ gender + task + c.age + location + education.ordered + (1|ID) + (1|word), data = d2, family = bernoulli, prior = gelman_hill_binom_priors, save_all_pars = T)

summary(fit1)$fixed[,c(1,2,3,4)] %>% kable(digits = 2)
summary(fit1)$random$ID[,c(1,2,3,4)] %>% kable(digits = 2)
summary(fit1)$random$word[,c(1,2,3,4)] %>% kable(digits = 2)

# for details, see models/mitchell_racz_wambs.r

# --- viz --- #

p1 = d2 %>% 
  group_by(ID,gender) %>% 
  summarise(mean.accuracy = mean(na.omit(correct))) %>% 
  ggplot(aes(x = gender, y = mean.accuracy, fill = gender)) +
    geom_violin() +
    geom_segment(aes(
		x=match(gender,levels(gender))-0.1,
			xend=match(gender,levels(gender))+0.1,
		y=mean.accuracy,yend=mean.accuracy),
		col='black'
	  ) +
    stat_summary(fun.y=mean, geom="point", shape=15, size=4) +
    scale_fill_brewer(palette = 'Set2') +
    theme(legend.position = 'none') + #, axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab('mean accuracy') +
    ylim(0,0.75) +
    ggtitle('(i) Accuracy and gender')


p2 = d2 %>% 
  group_by(ID,task) %>% 
  summarise(mean.accuracy = mean(na.omit(correct))) %>% 
  ggplot(aes(x = task, y = mean.accuracy, fill = task)) +
    geom_violin() +
    geom_segment(aes(
		x=match(task,levels(task))-0.1,
			xend=match(task,levels(task))+0.1,
		y=mean.accuracy,yend=mean.accuracy),
		col='black'
	  ) +
    stat_summary(fun.y=mean, geom="point", shape=15, size=4) +
    scale_fill_brewer(palette = 'Set3') +
    theme(legend.position = 'none') + #, axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab('') +
    ylim(0,0.75) +
    ggtitle('(ii) Accuracy and task type')


p3 = d2 %>% 
  filter(!location %in% c('Maguugu', 'Nyeamuusta')) %>% 
  group_by(ID,location) %>% 
  summarise(mean.accuracy = mean(na.omit(correct))) %>% 
  ggplot(aes(x = location, y = mean.accuracy, fill = location)) +
    geom_violin() +
    geom_segment(aes(
		x=match(location,levels(location))-0.1,
			xend=match(location,levels(location))+0.1,
		y=mean.accuracy,yend=mean.accuracy),
		col='black'
	  ) +
    stat_summary(fun.y=mean, geom="point", shape=15, size=4) +
    scale_fill_brewer(palette = 'Pastel2') +
    theme(legend.position = 'none') +#, axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab('') +
    ylim(0,0.75) +
    ggtitle('Accuracy and location')

p4 = d2 %>% 
  group_by(ID,age,gender) %>% 
  summarise(mean.accuracy = mean(na.omit(correct))) %>% 
    ggplot(aes(x = age, y = mean.accuracy)) +
    geom_point(size = 4, aes(colour = gender)) +
    scale_colour_brewer(palette = 'Set2') +
    theme(legend.position = 'none') +
    stat_smooth(formula = y ~ x, method = 'lm', se = F, fullrange = T, colour = 'black') +
    ylab('') +
    ylim(0,0.75) +
    ggtitle('(iv) Accuracy and age')

( p1 + p2 ) / ( p3 + p4 ) 
ggsave("figs/Figure1.tiff", width = 12, height = 12, dpi=400, compression = "lzw")

