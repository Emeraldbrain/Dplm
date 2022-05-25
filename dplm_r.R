rm(list=ls())


library(tidyverse)
library(ggplot2)
library(dplyr)
library(car)
install.packages("lsr")
install.packages("lintr")
library(lsr)
library(modelr)
install.packages('lmtest')
library(lmtest)
library(effects)
library(lme4)

install.packages("devtools")
devtools::install_github("kassambara/pubr")

#Preparing data
wd <- 'C:/Users/Alexandra Morozova/Desktop/ВШЭ/4 курс/диплом'
setwd(wd)

data <- read.csv("C:/Users/Alexandra Morozova/Desktop/hse/4/dplm/model.csv", dec=".", sep=",", header=T, encoding = 'UTF-8') #na.strings='NA', fill = TRUE
data1 <- data %>% 
  gather(key = 'trial', value = 'score', A5, A7, A8, B, C) %>% 
  convert_as_factor(ID, Gender, trial, Version)

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


data1$Age <- factor(data1$Age)
data1$Age <- as.numeric(data1$Age)

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

ggqqplot(data1, "score", ggtheme = theme_bw(), group = 1 ) +
  facet_grid(score ~ Age, labeller = "label_both")

#anova

my.anova <- aov(score~Gender*trial, data1)
summary(my.anova)

anova_g <- aov(score ~ Gender, data1)
summary( anova_g )

anova_g.residuals <- residuals (object = anova_g) # extract the residuals
hist( x = anova_g.residuals )
qqnorm( y = anova_g.residuals )
shapiro.test( x = anova_g.residuals) #not normal

kg <- kruskal_test(score ~ Gender, data = data1)#ns p-value = 0.3054
leveneTest(y = data1$score, group = data1$Gender)#ok

data1 %>%
  pairwise_wilcox_test(
    score ~ Gender, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

anova_a<- aov(score ~ Age, data1)
summary( anova_a )

anova_a.residuals <- residuals (object = anova_a) # extract the residuals
hist( x = anova_a.residuals )
qqnorm( y = anova_a.residuals )
shapiro.test( x = anova_g.residuals) #not normal

ka <- kruskal_test(score ~ Age, data = data1)#s p-value < 2.2e-16
leveneTest(y = data1$score, group = data1$Age)#not equal

oneway.test(score ~ Age, data = data1) #s p-value < 2.2e-16

oa <- welch_anova_test(score ~ Age, data = data1)
oa

#data format
data2 <- filter(data1, Age == 5 | Age == 6 | Age == 7 | Age == 8)
data3 <- filter(data1, Age == 8 | Age == 9 | Age == 10 | Age == 11)
data4 <- filter(data1, Age == 11 | Age == 12 | Age == 13 | Age == 14)
data5 <- filter(data1, Age == 14 | Age == 15 | Age == 16)
data6 <- filter(data1, Age == 16 | Age == 17 | Age == 18)
#boxplots with p-values

#pwc

#WORKS!!!! -------------------------------------------------------------------------
res.kruskal <- data1 %>% kruskal_test(score ~ Gender)
res.kruskal

res.kruskal_a <- data1 %>% kruskal_test(score ~ Age)
res.kruskal_a

stat.test <- data1 %>%  pairwise_wilcox_test(
  score ~ Gender, paired = TRUE,
  p.adjust.method = "holm"
)
stat.test
stat.test <- stat.test %>% add_xy_position(x = "Gender")

ggboxplot(data1, x = "Gender", y = "score", color = "Gender", palette = "jco") +
  stat_pvalue_manual(stat.test, hide.ns = FALSE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(stat.test)
  )



stat.test_gt <- data1 %>%
  group_by(trial) %>%
  wilcox_test(score ~ Gender) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test_gt

stat.test_gt <- stat.test_gt %>% add_xy_position(x = "Gender")
bxp <- ggboxplot(data1, x = "Gender", y = "score", color = "Gender", palette = "jco",
                 facet.by = "trial")
bxp + 
  stat_pvalue_manual(stat.test_gt, label = "p.adj", hide.ns = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  labs(
  subtitle = get_test_label(res.kruskal, detailed = TRUE),
  caption = get_pwc_label(stat.test_gt)
)



stat.test_a2 <- data2 %>%
  group_by(trial) %>%
  wilcox_test(score ~ Age) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test_a2

stat.test_a2 <- stat.test_a2 %>% add_xy_position(x = "Age")
bxp <- ggboxplot(data2, x = "Age", y = "score", color = "Age", facet.by = "trial")
bxp + 
  stat_pvalue_manual(stat.test_a2, label = "p.adj", hide.ns = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.19))) +
  labs(
    subtitle = get_test_label(oa, detailed = TRUE),
    caption = get_pwc_label(stat.test_a2)
  )

stat.test_a3 <- data3 %>%
  group_by(trial) %>%
  wilcox_test(score ~ Age) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test_a3

stat.test_a3 <- stat.test_a3 %>% add_xy_position(x = "Age")
bxp <- ggboxplot(data3, x = "Age", y = "score", color = "Age", facet.by = "trial")
bxp + 
  stat_pvalue_manual(stat.test_a3, label = "p.adj", hide.ns = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    subtitle = get_test_label(oa, detailed = TRUE),
    caption = get_pwc_label(stat.test_a3))

stat.test_a4 <- data4 %>%
  group_by(trial) %>%
  wilcox_test(score ~ Age) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test_a4

stat.test_a4 <- stat.test_a4 %>% add_xy_position(x = "Age")
bxp <- ggboxplot(data4, x = "Age", y = "score", color = "Age", facet.by = "trial")
bxp + 
  stat_pvalue_manual(stat.test_a4, label = "p.adj", hide.ns = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  labs(
    subtitle = get_test_label(oa, detailed = TRUE),
    caption = get_pwc_label(stat.test_a4))

stat.test_a5 <- data5 %>%
  group_by(trial) %>%
  wilcox_test(score ~ Age) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test_a5

stat.test_a5 <- stat.test_a5 %>% add_xy_position(x = "Age")
bxp <- ggboxplot(data5, x = "Age", y = "score", color = "Age", facet.by = "trial")
bxp + 
  stat_pvalue_manual(stat.test_a5, label = "p.adj", hide.ns = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  labs(
    subtitle = get_test_label(oa, detailed = TRUE),
    caption = get_pwc_label(stat.test_a5))

stat.test_a6 <- data6 %>%
  group_by(trial) %>%
  wilcox_test(score ~ Age) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test_a6

stat.test_a6 <- stat.test_a6 %>% add_xy_position(x = "Age")
bxp <- ggboxplot(data6, x = "Age", y = "score", color = "Age", facet.by = "trial")
bxp + 
  stat_pvalue_manual(stat.test_a6, label = "p.adj", hide.ns = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    subtitle = get_test_label(oa, detailed = TRUE),
    caption = get_pwc_label(stat.test_a6))
#----------------------------------------------
stat.test_b <- data1 %>%
  group_by(trial) %>%
  wilcox_test(score ~ Age) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test_b

stat.test_b <- stat.test_b %>% add_xy_position(x = "Age")
stat.test_b$custom.label <- ifelse(stat.test_b$p.adj > 0.0001, '', NA)
bxp <- ggboxplot(data1, x = "Age", y = "score", color = "Age", facet.by = "trial")
bxp + 
  stat_pvalue_manual(stat.test_b, label = "custom.label", hide.ns = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.45)))
  

stat.test_b$custom.label <- ifelse(stat.test_b$p.adj <= 0.001, stat.test_b$p.adj, "ns")

# Visualization
bxp + 
  stat_pvalue_manual(stat.test_b, label = "custom.label") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10)))

stat.test_b <- stat.test_b[,-2]
stat.test_b <- stat.test_b[,-5] #4 times

stat.test_b <- subset(stat.test_b, p.adj <  0.05)
stat.test_5 <- subset(stat.test_b, trial == 'A5')
#dm1 <- filter(dm, p.adj.signif == '**' | p.adj.signif == '***' | p.adj.signif == '**' )

#stat.test_b <- pivot_wider(stat.test_b, names_from = c(p.adj.signif), values_from = p.adj)
stat.test_b <- pivot_wider(stat.test_b, names_from = c(group1), values_from = p.adj)
colnames(stat.test_b)[1] <- 'Попытка'
colnames(stat.test_b)[2] <- 'Возраст'

df1 <- stat.test_b[,-2]
#------------------------
install.packages("xlsx")
library(xlsx)
write.xlsx(stat.test_b, "table.xlsx")
write.csv2(stat.test_b, "table.csv")

df1 <- subset(dt, p >  0.05)
df1 <- df1[,-4]

df1 <- pivot_wider(df1, names_from = c(group1), values_from = p)
colnames(df1)[1] <- 'Возраст'

write.csv2(df1, "table1.csv")
write.csv2(stat.test_b, "table2.csv")
#-----------------------------------------------------------------------------------------

pwc <- data1 %>%
  group_by(trial) %>%
  dunn_test(score ~ Gender, 
            p.adjust.method = "hochberg"
            )
pwc

pwc <- pwc %>% add_xy_position(x = "Gender")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


pg <- data1 %>%
  group_by(trial) %>%
  pairwise_wilcox_test(
    score ~ Gender, paired = TRUE,
    p.adjust.method = "none"
  )
pg #gender by trial


pg <- pg %>% add_xy_position(x = "Gender")
bxp+ 
  stat_pvalue_manual(pg, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(kg, detailed = TRUE),
    caption = get_pwc_label(pg)
  )
bpg

bxp
bxp0
bxp1 #age trial
bxp2
bxp3
bxp4

pwc2 <- data1 %>%
  pairwise_wilcox_test(
    score ~ Gender, paired = TRUE,
    p.adjust.method = "none"
  )
pwc2 #gender

pwc2 <- pwc2 %>% add_xy_position(x = "Gender")
bxp + 
  stat_pvalue_manual(pwc2, tip.length = 0, hide.ns = FALSE) +
  labs(
    subtitle = get_test_label(kg, detailed = TRUE),
    caption = get_pwc_label(pwc2)
  )

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
  kruskal_test(score ~ Gender) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test0

stat.test0 <- stat.test0 %>% add_xy_position(x = "Gender")
bxp_1 <- ggboxplot(data1, x = "Gender", y = "score", fill = "#00AFBB")
bxp_1 + 
  stat_pvalue_manual(stat.test0, label = "p.adj", hide.ns = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10)))

#Model
model5 <- lmer(score ~ Age*trial + (1+trial|ID) + (1+trial|Age), data=data1, REML=FALSE)

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

coef_items <- coef(model5) 
coef_items

#Better model
m1 <- lm(score ~ Age*trial-1, data=data1) 
m1
summary(m1)

m2 <- lm(score ~ Age, data=data1) 
tab_model(m2, show.re.var= TRUE,  dv.labels= "Влияние возраста участника на результат теста Рея")

m3 <- lm(score ~ trial, data=data1) 
tab_model(m3, show.re.var= TRUE,  dv.labels= "Влияние возраста участника на результат теста Рея")

qqnorm(resid(m1))
qqline(resid(m1))

hist(log(residuals(m1)))
hist(residuals(m1))

m_1 <- aov(score ~ Age*trial-1, data=data1)

resid <- residuals(m1)
shapiro.test( resid )
leveneTest(data1$score ~ data1$Age)

leveneTest(score ~ Age * trial, data1)

anova(m1)
#
effects_mod1 <- effect(term= "Age*trial", mod= m1)
summary(effects_mod1) #output of what the values are

emd_df1 <- as.data.frame(effects_mod1)

mod_plot1 <- ggplot() + 
  geom_point(data=subset(data1), aes(Age, score)) + 
  geom_point(data=emd_df1, aes(x=Age, y=fit), color="blue") +
  geom_line(data=emd_df1, aes(x=Age, y=fit), color="blue") +
  geom_ribbon(data= emd_df1, aes(x=Age, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  facet_wrap(~trial) +
  labs(x="Age and trial", y="Score")

mod_plot1

plot_model(m1,"eff",axis.title=c("Age","score"))
plot_model(m1,"est",axis.title=c("Trial","score"))
plot_model(m1,"int",axis.title=c("Возраст","Результаты"), title = "Предсказанный результат по возрасту и попытке")
plot_model(m1,"pred",axis.title=c("Попытка","Результаты"), title = "Предсказанный результат по каждой попытке")
plot_model(m1,
           show.values=TRUE, show.p=TRUE,
           title="Effect of Age on Score")

tm <- tab_model(m1, show.re.var= TRUE,  dv.labels= "Влияние возраста участника на результат теста Рея")


#Estimates
#Положительный коэффициент указывает на то, что 
#по мере увеличения значения независимой переменной 
#среднее значение зависимой переменной также имеет тенденцию к увеличению. 
#Отрицательный коэффициент предполагает, что 
#по мере увеличения независимой переменной зависимая переменная имеет тенденцию к уменьшению.

#
coef_items1 <- coef(m1) 
coef_items1

ci_items <- predict(m1, 
                    newdata = data1,
                    interval = 'confidence',
                    level = 0.95) %>%
  as.data.frame() %>%
  rename(lci = lwr, uci = upr)


data1 %>% 
  add_predictions(m1) %>%
  ggplot() +
  geom_point(aes(Age, score)) +
  geom_line(aes(Age, pred), color = 'red') +
  facet_wrap(~trial) +
  labs(title='Регрессионная прямая')

data1 %>% 
  add_predictions(m1) %>%
  ggplot(aes(Age, score)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = 'red') +
  geom_smooth(method = 'lm',
              level = .8,
              color = 'red') +
  facet_wrap(~trial) +
  labs(title = '80% и 90% доверительные интервалы')


# 95% доверительный интервал для среднего
ci_Age <- predict(m1, 
                     newdata = data1,
                     interval = 'confidence',
                     level = 0.95) %>%
  as.data.frame() %>%
  rename(lci = lwr, uci = upr)

# 95% доверительный интервал для отдельных наблюдений
pi_Age <- predict(m1, 
                     newdata = data1,
                     interval = 'prediction',
                     level = 0.95) %>%
  as.data.frame() %>%
  select(-fit) %>% # точечный прогноз уже есть в наборе, удаляем этот столбец
  rename(lpi = lwr, upi = upr)

# Склеиваем все в одну таблицу
pred_amount <- bind_cols(data1, ci_Age, pi_Age)


m1_plot <- ggplot(data = pred_amount) +
  geom_ribbon(aes(Age, ymin = lpi, ymax = upi), 
              fill = 'lightskyblue', alpha = 0.5) + 
  geom_ribbon(aes(Age, ymin = lci, ymax = uci), 
              fill = 'green', alpha = 0.5) +
  geom_line(aes(Age, fit), color = 'red') +
  geom_point(aes(Age, score)) +
  facet_wrap(~trial) +
  labs(title = paste('Интервальный прогноз (95% интервалы) результатов теста Рея в зависимости от возраста', 
                     sep = '\n'),
       y = 'Результаты', x = 'Возраст' )

m1_plot

#Можно с 95% уверенностью утверждать, что регрессионная прямая совокупности находится внутри доверительного интервала для среднего (серая область на графике)

#С 95% вероятностью фактические значения будут находиться внутри доверительного интервала для отдельных наблюдений (синяя область на графике)

#Ширина доверительного интервала для отдельных наблюдений существенно больше, чем для среднего.
