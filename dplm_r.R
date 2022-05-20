rm(list=ls())

library(tidyverse)
library(ggplot2)
library(dplyr)
library(car)
install.packages("lsr")
library(lsr)
library(modelr)
install.packages('lmtest')
library(lmtest)
library(effects)
library(lme4)

#Preparing data
wd <- 'C:/Users/Alexandra Morozova/Desktop/ВШЭ/4 курс/диплом';
setwd(wd);

data <- read.csv('model.csv', dec=".", sep=",", header=T, encoding = 'UTF-8'); #na.strings='NA', fill = TRUE
data1 <- data %>% 
  gather(key = 'trial', value = 'score', A5, A7, A8, B, C) %>% 
  convert_as_factor(ID, trial, Version)

#Visualization data
ggplot(data1, aes(x=trial, y=score, color = Version)) +
  geom_boxplot()
ggplot(data1, aes(x=trial, y=score, color = Gender)) +
  geom_boxplot()
ggplot(data1, aes(x=Gender, y=score, color = trial)) +
  geom_boxplot()
  #facet_wrap(~trial, nrow=5)
ggplot(data1, aes(x=Gender, y=score)) +
  geom_boxplot()+
  facet_wrap(~Age, nrow=5)

ggplot(data1, aes(x=Age, y=score, color = trial)) +
  geom_boxplot()+
  facet_wrap(~trial, nrow=5)#factorisation is needed


ggboxplot(data1, x = "Age", y = "score", color = "Gender", palette = "jco")
ggboxplot(data1, x = "Gender", y = "score", color = "Gender", palette = "jco")
ggboxplot(data1, x = "trial", y = "score", color = "Gender", palette = "jco")
ggboxplot(data1, x = "Age", y = "score", palette = "jco")


bxp0
bxp1
bxp2
bxp3
bxp4

ggplot(data1)+
  geom_boxplot(aes(Gender, score))
ggplot(data1, aes(x=trial, y=score, color = Version)) +
  geom_boxplot()+
  facet_wrap(~ Gender, nrow = 2)
ggplot(data1, aes(x=Age, y=score, color = trial)) +
  geom_boxplot()+
  facet_wrap(~ trial, nrow = 5)
ggplot(data1, aes(x=Gender, y=score, color = trial)) +
  geom_boxplot()+
  facet_wrap(~ Age, nrow = 5)
ggplot(data1, aes(x=Gender, y=score)) +
  geom_boxplot()+
  facet_wrap(~ Age, nrow = 5)

data1$Age <- factor(data1$Age);
ggplot(data1, aes(x=Gender, y=score, color = trial)) +
  geom_boxplot()+
  facet_wrap(~ Age, nrow = 5)
ggplot(data1, aes(x=Age, y=score, color = trial)) +
  geom_boxplot()+
  facet_wrap(~ trial, nrow = 5)
boxplot(score ~ Age*Gender,
        col=c("white","lightgray"), data1)
boxplot(score ~ Age,
        col=c("white","lightgray"), data1)

#Normal dist check 
data1 %>%
  group_by(Version, trial) %>%
  shapiro_test(score) #not normal

data1 %>%
  group_by(Age, trial) %>%
  shapiro_test(score)#not normal

data1 %>%
  group_by(Gender, trial) %>%
  shapiro_test(score)#not normal

shapiro_test(data1, score)
hist(data1$score)
qqPlot(data1$score)#not normal



#boxplots with p-values
stat.test <- df %>%
  group_by(dose) %>%
  t_test(len ~ supp) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test

stat.test <- stat.test %>% add_xy_position(x = "supp")
bxp <- ggboxplot(df, x = "supp", y = "len", fill = "#00AFBB",
                 facet.by = "dose")
bxp + 
  stat_pvalue_manual(stat.test, label = "p.adj", hide.ns = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10)))


model5 = lmer(score ~ Age*trial + (1+trial|Age)+(1+trial|ID), data=data1, REML=FALSE)

qqnorm(resid(model5))
qqline(resid(model5))

hist(log(residuals(model5)))
hist(residuals(model5))



effects_mod <- effect(term= "Age*trial", mod= model5)
summary(effects_mod) #output of what the values are

emd_df <- as.data.frame(effects_mod)

mod_plot <- ggplot() + 
  geom_point(data=subset(data1), aes(Age, score)) + 
  geom_point(data=emd_df, aes(x=Age, y=fit), color="blue") +
  geom_line(data=emd_df, aes(x=Age, y=fit), color="blue") +
  geom_ribbon(data= emd_df, aes(x=Age, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  facet_wrap(~trial) +
  labs(x="Age and trial", y="Score")

mod_plot

tab_model(model5, show.re.var= TRUE,  dv.labels= "Effects of Age and Trial on Score")
print(dotplot(ranef(model5, condVar=TRUE)))
summary(model5)

install.packages('glmmTMB')
library(glmmTMB)
plot_model(model5,"eff",axis.title=c("Age","score"))
plot_model(model5,"est",axis.title=c("Trial","score"))
plot_model(model5,"int",axis.title=c("Age","score"))
plot_model(model5,"re")
plot_model(model5,"pred",axis.title=c("Trial","score"))
plot_model(model5,
           show.values=TRUE, show.p=TRUE,
           title="Effect of Age on Score")
