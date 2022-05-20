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


data1$Age <- factor(data1$Age);
data1$Age <- as.numeric(data1$Age);

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

ggqqplot(data1, "score", ggtheme = theme_bw()) +
  facet_grid(score ~ Age, labeller = "label_both")

#anova

anova_g <- aov(score ~ Gender, data1)
summary( anova_g )

anova_g.residuals <- residuals (object = anova_g) # extract the residuals
hist( x = anova_g.residuals )
qqnorm( y = anova_g.residuals )
shapiro.test( x = anova_g.residuals) #not normal

kruskal.test(score ~ Gender, data = data1)#ns p-value = 0.3054
leveneTest(y = data1$score, group = data1$Gender)#ok

data1 %>%
  pairwise_t_test(
    score ~ Gender, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

anova_a<- aov(score ~ Age, data1)
summary( anova_a )

anova_a.residuals <- residuals (object = anova_a) # extract the residuals
hist( x = anova_a.residuals )
qqnorm( y = anova_a.residuals )
shapiro.test( x = anova_g.residuals) #not normal

kruskal.test(score ~ Age, data = data1)#s p-value < 2.2e-16
leveneTest(y = data1$score, group = data1$Age)#not equal

oneway.test(score ~ Age, data = data1) #s p-value < 2.2e-16



#data format
data2 <- filter(data1, Age == 5 | Age == 6 | Age == 7 | Age == 8)
data3 <- filter(data1, Age == 9 | Age == 10 | Age == 11 | Age == 12)
data4 <- filter(data1, Age == 13 | Age == 14 | Age == 15)
data5 <- filter(data1, Age == 16 | Age == 17 | Age == 18)
#boxplots with p-values

#pwc
pwc <- data1 %>%
  group_by(trial) %>%
  pairwise_t_test(
    score ~ Gender, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc #gender by trial

pwc2 <- data1 %>%
  pairwise_t_test(
    score ~ Gender, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2 #gender

pwc1 <- data1 %>%
  pairwise_t_test(
    score ~ Age, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1 #age

pwc3 <- data1 %>%
  group_by(trial) %>%
  pairwise_t_test(
    score ~ Age, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc3 #Age by trial

#stat test
stat.test0 <- data1 %>%
  t_test(score ~ Age) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test0

stat.test0 <- stat.test0 %>% add_xy_position(x = "Age")
bxp_1 <- ggboxplot(data1, x = "Age", y = "score", fill = "#00AFBB")
bxp_1 + 
  stat_pvalue_manual(stat.test0, label = "p.adj", hide.ns = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10)))

#Model
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
