## social self regulation paper code
## study 2
## Chelsea Helion, created: 01/12/2022; updated 02/02/2022
rm(list = ls())

library(tidyverse)
library(skimr)
library(reghelper)
library(lme4)
library(lmerTest)
library(sjPlot)
setwd("/Users/tua37526/Dropbox/SocialSelfReg")

## load in data
regData_s2 <- read_csv("Study2/AllBehaviorData.csv")
glimpse(regData_s2)

## look at demographics
singTrial_s2 <-
  regData_s2 %>%
  filter(Trial == 1)
mean(singTrial_s2$Age)
sd(singTrial_s2$Age)
table(singTrial_s2$Gender)

## check average number of trials completed
trials <- regData_s2 %>%
  select(SANPID) %>%
  group_by(SANPID) %>%
  mutate(id = seq_len(n())) %>%
  summarise(meanID = length(id))
mean(trials$meanID)
sd(trials$meanID)

## check manipulation
t.test(
  singTrial_s2$VideogameWant,
  singTrial_s2$FinanceWant,
  var.equal = T,
  paired = T
)
singTrial_s2 %>%
  summarise(mean(VideogameWant),
            mean(FinanceWant),
            sd(VideogameWant),
            sd(FinanceWant))

## look at average trial completion
## check average number of trials completed
trials <- regData_s2 %>%
  select(SANPID, ResponseFavorCoded) %>%
  group_by(SANPID) %>%
  filter(!is.na(ResponseFavorCoded)) %>%
  mutate(id = seq_len(n())) %>%
  summarise(meanID = length(id))
mean(trials$meanID)
sd(trials$meanID)

## Choice behavior; 0 = favored game, 1 = favored questions
regData_s2 <- regData_s2 %>%
  filter(!is.na(ResponseFavorCoded))

## Choice behavior analyses
## binary choice
## fit mixed effects interaction model -- Social Motivation and Condition
regData_s2$SelfOther_GoalMotivation_c <-
  scale(regData_s2$SelfOther_GoalMotivation,
        center = T,
        scale = F)
glm1 <-
  glmer(
    formula = ResponseFavorCoded ~ SelfOther_GoalMotivation_c * Group + (1 |
                                                                           SANPID),
    data = regData_s2,
    family = "binomial"
  )
summary(glm1)

glm2 <-
  glmer(
    formula = ResponseFavorCoded ~ SelfOther_GoalMotivation_c + Group + (1 |
                                                                           SANPID),
    data = regData_s2,
    family = "binomial"
  )
summary(glm2)
anova(glm1, glm2)
plot(effect("SelfOther_GoalMotivation_c:Group", glm1), grid = TRUE)

confint(glm1, parm = "beta_", method = "Wald")
simple_slopes(
  glm1,
  levels = NULL,
  confint = FALSE,
  ci.width = .95,
  confint.method = c("Wald")
)

glm1_plot <-
  glmer(
    formula = ResponseFavorCoded ~ SelfOther_GoalMotivation * Group + (1 |
                                                                         SANPID),
    data = regData_s2,
    family = "binomial"
  )
## plot glmer model
plot_model(glm1_plot,
           type = "pred",
           terms = c("SelfOther_GoalMotivation", "Group"))

## run moral obligation
regData_s2$MoralObl_c <-
  scale(regData_s2$MoralObl,
        center = T,
        scale = F)

## binary choice
glm1_mo <-
  glmer(
    formula = ResponseFavorCoded ~ MoralObl_c * Group + (1 |
                                                           SANPID),
    data = regData_s2,
    family = "binomial"
  )
summary(glm1_mo)

glm2_mo <-
  glmer(
    formula = ResponseFavorCoded ~ MoralObl_c + Group + (1 |
                                                           SANPID),
    data = regData_s2,
    family = "binomial"
  )
summary(glm2_mo)
anova(glm2_mo, glm1_mo)
simple_slopes(glm1_mo)

## time allocation
## fit mixed effects interaction model -- Social Motivation and Condition

mlm1 <-
  lmer(DiffTime ~ SelfOther_GoalMotivation_c * Group + (1 |
                                                          SANPID), data = regData_s2)
summary(mlm1)

mlm2 <-
  lmer(DiffTime ~ SelfOther_GoalMotivation_c + Group + (1 |
                                                          SANPID), data = regData_s2)
summary(mlm2)

anova(mlm2, mlm1)
confint(mlm1, parm = "beta_", method = "Wald")
simple_slopes(
  mlm1,
  levels = NULL,
  confint = FALSE,
  ci.width = .95,
  confint.method = c("Wald")
)

mlm_plot <- lmer(DiffTime ~ SelfOther_GoalMotivation * Group + (1 |
                                                                  SANPID), data = regData_s2)
plot_model(mlm_plot,
           type = "pred",
           terms = c("SelfOther_GoalMotivation", "Group"))


## time allocation
mlm1_mo <-
  lmer(DiffTime ~ MoralObl_c * Group + (1 |
                                          SANPID), data = regData_s2)
summary(mlm1_mo)

mlm2_mo <-
  lmer(DiffTime ~ MoralObl_c + Group + (1 |
                                          SANPID), data = regData_s2)
summary(mlm2_mo)

anova(mlm1_mo, mlm2_mo)
confint(mlm1_mo, parm = "beta_", method = "Wald")
confint(mlm2_mo, parm = "beta_", method = "Wald")

## check strength of effects when social desirability is included in the model
glmSD <-
  glmer(
    formula = ResponseFavorCoded ~ SocialDesirability_Total + (1 |
                                                                 SANPID),
    data = regData_s2,
    family = "binomial"
  )
summary(glmSD)
confint(glmSD, parm = "beta_", method = "Wald")
cc <- confint(glmSD, parm = "beta_", method = "Wald")
ctab <- cbind(est=fixef(glmSD),cc)
rtab <- exp(ctab)
print(rtab,digits=3)

mlmSD <-
  lmer(formula = DiffTime ~ SocialDesirability_Total + (1 |
                                                          SANPID),
       data = regData_s2)
summary(mlmSD)
confint(mlmSD, parm = "beta_", method = "Wald")

glm1_SD <-
  glmer(
    formula = ResponseFavorCoded ~ SelfOther_GoalMotivation_c * Group + SocialDesirability_Total + (1 |
                                                                                                      SANPID),
    data = regData_s2,
    family = "binomial"
  )
summary(glm1_SD)
confint(glm1_SD, parm = "beta_", method = "Wald")
cc <- confint(glm1_SD, parm = "beta_", method = "Wald")
ctab <- cbind(est=fixef(glm1_SD),cc)
rtab <- exp(ctab)
print(rtab,digits=3)

mlm1_SD <-
  lmer(
    formula = DiffTime ~ relational_Obl_c * Group + SocialDesirability_Total + (1 |
                                                                                  SANPID),
    data = regData_s2
  )
summary(mlm1_SD)
confint(mlm1_SD, parm = "beta_", method = "Wald")

## Items for Goal Assessment table, per condition
t.test(VideogameWant ~ Group, data = singTrial_s2)
t.test(FinanceWant ~ Group, data = singTrial_s2)
t.test(FinanceImportance ~ Group, data = singTrial_s2)
t.test(MoralObl ~ Group, data = singTrial_s2)
t.test(SelfOther_GoalMotivation ~ Group, data = singTrial_s2)

singTrial_s2 %>%
  filter(Group == "Exp") %>%
  summarise(
    sd(VideogameWant),
    sd(FinanceWant),
    sd(FinanceImportance),
    sd(MoralObl),
    sd(SelfOther_GoalMotivation)
  )
singTrial_s2 %>%
  filter(Group == "Control") %>%
  summarise(
    sd(VideogameWant),
    sd(FinanceWant),
    sd(FinanceImportance),
    sd(MoralObl),
    sd(SelfOther_GoalMotivation)
  )
