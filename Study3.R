## social self regulation paper code
## study 3
## Chelsea Helion, created: 01/15/2022; updated 02/02/2022
rm(list = ls())

library(tidyverse)
library(skimr)
library(reghelper)
library(lme4)
library(lmerTest)
library(sjPlot)
setwd("/Users/tua37526/Dropbox/SocialSelfReg")

## load in data
regData_s3 <- read_csv("Study3/BehavioralData_na.csv")
skim(regData_s3)
questDat_s3 <- read_csv("Study3/ScoredQualtrics.csv")
skim(questDat_s3)
regData_s3 <- inner_join(regData_s3, questDat_s3)

## look at demographics
## study 3
regData_s3a <- regData_s3 %>%
  select(
    SANPID,
    Age,
    Gender,
    FinanceBenefit_FinanceGoalAssociation,
    FinanceBenefit_SocialGoalAssociation,
    SocialBenefit_FinanceGoalAssociation,
    SocialBenefit_SocialGoalAssociation,
    Closeness,
    Duration,
    MoralObl,
    LetDownConcern
  ) %>%
  group_by(SANPID) %>% mutate(id = seq_len(n()))

regData_s3a$id <- as.numeric(regData_s3a$id)

singTrial_s3 <- regData_s3a %>%
  filter(id == 1)

rm(list = c("regData_s3a", "questDat_s3"))

mean(singTrial_s3$Age)
sd(singTrial_s3$Age)
table(singTrial_s3$Gender)

## create relational obligation variable
regData_s3$RelationalObl <-
  (regData_s3$MoralObl + regData_s3$LetDownConcern) / 2
regData_s3$RelationalObl_c <-
  scale(regData_s3$RelationalObl,
        center = T,
        scale = F)

## Choice behavior analyses
## binary choice
## fit mixed effects model (mean centered)
regData_s3$Condition <-
  as.factor(regData_s3$TypeNameShown) #(1 = FinanceBenefit, 2 = FriendBenefit)
glm1 <-
  glmer(
    formula = ResponseFavorCode ~ Condition * RelationalObl_c + (1 |
                                                                   SANPID),
    data = regData_s3,
    family = "binomial"
  )
summary(glm1)

glm2 <- glmer(
  formula = ResponseFavorCode ~ Condition + (1 | SANPID),
  data = regData_s3,
  family = "binomial"
)
summary(glm2)
anova(glm1, glm2)
confint(glm1, parm = "beta_", method = "Wald")
simple_slopes(glm1)

## fit mixed effects model (for plotting)
glm_plot <-
  glmer(
    formula = ResponseFavorCode ~ Condition * RelationalObl + (1 |
                                                                 SANPID),
    data = regData_s3,
    family = "binomial"
  )

## plot glmer model
plot_model(glm_plot,
           type = "pred",
           terms = c("RelationalObl", "Condition"))

## time allocation
## fit mixed effects model (mean centered)
mlm1 <-
  lmer(formula = DiffTime ~ Condition * RelationalObl_c + (1 | PID),
       data = regData_s3)
summary(mlm1)

mlm2 <-
  lmer(formula = DifferenceTime ~ Condition + RelationalObl + (1 |
                                                                 PID),
       data = regData_s3)
summary(mlm2)
anova(mlm1, mlm42)
confint(mlm1, parm = "beta_", method = "Wald")
simple_slopes(mlm1)

## fit mixed effects model (for plotting)
mlm_plot <-
  lmer(formula = DiffTime ~ Condition * RelationalObl + (1 | SANPID),
       data = regData_s3)

## plot glmer model
plot_model(mlm_plot,
           type = "pred",
           terms = c("RelationalObl", "Condition"))

## check strength of effects when social desirability is included in the model
glm1_SD <-
  glmer(
    formula = ResponseFavorCode ~ Condition * RelationalObl_c + SocialDesirability + (1 |
                                                                   SANPID),
    data = regData_s3,
    family = "binomial"
  )
summary(glm1_SD)

mlm1_SD <-
  lmer(
    formula = DiffTime ~ Condition * RelationalObl_c + SocialDesirability + (1 |
                                                                                        SANPID),
    data = regData_s3
  )
summary(mlm1_SD)
## plot glmer model
plot_model(mlm1_SD,
           type = "pred",
           terms = c("RelationalObl_c", "Condition"))


## mean differences for table
relGoals <- singTrial_s3 %>%
  summarise(
    meanFB_Finance = mean(FinanceBenefit_FinanceGoalAssociation),
    meanFB_Social = mean(FinanceBenefit_SocialGoalAssociation),
    meanSB_Finance = mean(SocialBenefit_FinanceGoalAssociation),
    meanSB_Social = mean(SocialBenefit_SocialGoalAssociation),
  )
t.test(relGoals$meanFB_Finance, relGoals$meanSB_Finance, paired = T)
t.test(relGoals$meanFB_Social, relGoals$meanSB_Social, paired = T)

