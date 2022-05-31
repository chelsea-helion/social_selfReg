## social self regulation paper code
## study 1
## Chelsea Helion, created: 01/10/2022; updated 02/02/2022
rm(list = ls())

library(tidyverse)
library(skimr)
library(reghelper)
library(lme4)
library(lmerTest)
library(sjPlot)
library(corrplot)
library(ggeffects)
setwd("/Users/tua37526/Dropbox/SocialSelfReg")

## load in data
regData_s1 <- read_csv("Study1/BehavioralData_narm.csv")
glimpse(regData_s1)

## look at demographics
singTrial_s1 <-
  regData_s1 %>%
  filter(Trial == 10)
mean(singTrial_s1$Age)
sd(singTrial_s1$Age)
table(singTrial_s1$GenderCode)

## check average number of trials completed
trials <- regData_s1 %>% 
  select(SANPID) %>% 
  group_by(SANPID) %>% 
  mutate(id = seq_len(n())) %>% 
  summarise(meanID = length(id))
mean(trials$meanID)
sd(trials$meanID)

## check manipulation
t.test(
  singTrial_s1$VideogameWant,
  singTrial_s1$FinancialWant,
  var.equal = T,
  paired = T
)
singTrial_s1 %>%
  summarise(mean(VideogameWant),
            mean(FinancialWant),
            sd(VideogameWant),
            sd(FinancialWant))

## relaional assessment measures
# look at correlation of relational goals
relDat_s1 <- singTrial_s1 %>%
  select(
    Financial_Closeness,
    Financial_Duration,
    Financial_MoralObl,
    Financial_letDown,
    SocialFriend_Closeness,
    SocialFriend_Duration,
    SocialFriend_MoralObl,
    SocialFriend_LetDown,
    Celeb_Closeness,
    Celeb_Duration,
    Celeb_MoralObl,
    Celeb_LetDown
  )
relDat_s1 <- relDat_s1 %>%
  filter(!is.na(Financial_Closeness))
m <- cor(relDat_s1)
m_mat <- corr.test(relDat_s1)$p
corrplot(m)

## create relational obligation variable
regData_s1$relational_Obl <- (regData_s1$Moral_Obl + regData_s1$Let_Down) / 2
regData_s1$relational_Obl_c <- scale(regData_s1$relational_Obl, center = TRUE, scale = FALSE)

## Choice behavior analyses
## binary choice
regData_s1$TypeNameShown <- as.factor(regData_s1$TypeNameShown)
## 1 = Finance, 2 = Friend, 3 = Celebrity; Videogame = 1, Finance Task = 0
glm1 <-
  glmer(
    formula = ResponseFavorCoded ~ TypeNameShown + (1 | SANPID),
    data = regData_s1,
    family = "binomial"
  )
summary(glm1)
regData_s1$TypeNameShown <- relevel(regData_s1$TypeNameShown, ref = "3") ## repeat with new reference

glm2 <- glmer(formula = ResponseFavorCoded ~ (1 | SANPID),
              data = regData_s1,
              family = "binomial")
summary(glm2)

## model comparison
anova(glm1, glm2)
cc <- confint(glm1, parm = "beta_", method = "Wald")
ctab <- cbind(est=fixef(glm1),cc)
rtab <- exp(ctab)
print(rtab,digits=3)

## plot effects for binary choice
glm_plot <- glmer(
  formula = ResponseFavorCoded ~ TypeNameShown + (1 | SANPID),
  data = regData_s1,
  family = "binomial"
)
plot_model(glm_plot, type = "pred", terms = c("TypeNameShown"))

## time allocation
## fit mixed effects model
regData_s1$TypeNameShown <- relevel(regData_s1$TypeNameShown, ref = "1") 
mlm1 <- lmer(formula = DifferenceTime ~ TypeNameShown + (1 | SANPID),
             data = regData_s1)
summary(mlm1)
regData_s1$TypeNameShown <- relevel(regData_s1$TypeNameShown, ref = "2") ## repeat with new reference


mlm2 <- lmer(formula = DifferenceTime ~ (1 | SANPID),
             data = regData_s1)
summary(mlm2)

anova(mlm1, mlm2)
confint(mlm1, parm = "beta_", method = "Wald")

## relational assessment measures for summary table
relDat_s1 %>%
  summarise(
    meanFC = mean(Financial_Closeness, na.rm = T),
    meanSC = mean(SocialFriend_Closeness),
    meanCC = mean(Celeb_Closeness),
    sdFC = sd(Financial_Closeness, na.rm = T),
    sdSC = sd(SocialFriend_Closeness),
    sdCC = sd(Celeb_Closeness)
  )
relDat_s1 %>%
  summarise(
    meanFD = mean(Financial_Duration, na.rm = T),
    meanSD = mean(SocialFriend_Duration),
    meanCD = mean(Celeb_Duration),
    sdFD = sd(Financial_Duration, na.rm = T),
    sdSD = sd(SocialFriend_Duration),
    sdCD = sd(Celeb_Duration)
  )

relDat_s1$F_RO <-
  (relDat_s1$Financial_MoralObl + relDat_s1$Financial_letDown) / 2
relDat_s1$S_RO <-
  (relDat_s1$SocialFriend_MoralObl + relDat_s1$SocialFriend_LetDown) / 2
relDat_s1$C_RO <- (relDat_s1$Celeb_MoralObl + relDat_s1$Celeb_LetDown) / 2

relDat_s1 %>%
  summarise(
    meanFR = mean(F_RO, na.rm = T),
    meanSR = mean(S_RO),
    meanCR = mean(C_RO),
    sdFR = sd(F_RO, na.rm = T),
    sdSR = sd(S_RO),
    sdCR = sd(C_RO)
  )


## binary choice x relational obligation
## fit mixed effects model
## 1 = Finance, 2 = Friend, 3 = Celebrity; Videogame = 1, Finance Task = 0
regData_s1$TypeNameShown <- relevel(regData_s1$TypeNameShown, ref = "1")
regData_s1$type_name_shown_int <- recode(regData_s1$TypeNameShown, "1" = -1, "2" = 0, "3" = 1)
regData_s1$inter <- regData_s1$type_name_shown_int * regData_s1$relational_Obl_c
glm3 <-
  glmer(
    formula = ResponseFavorCoded ~ TypeNameShown * relational_Obl_c + (1 | SANPID),
    data = regData_s1,
    family = "binomial"
  )
summary(glm3)
regData_s1$TypeNameShown <- relevel(regData_s1$TypeNameShown, ref = "2") ## repeat with new reference

glm4 <- glmer(formula = ResponseFavorCoded ~ TypeNameShown + (1 | SANPID),
              data = regData_s1,
              family = "binomial")
summary(glm4)

## model comparison
anova(glm3, glm4)
confint(glm3, parm = "beta_", method = "Wald")
simple_slopes(
  glm3,
  levels = NULL,
  confint = FALSE,
  ci.width = .95,
  confint.method = c("Wald")
)

cc <- confint(glm3, parm = "beta_", method = "Wald")
ctab <- cbind(est=fixef(glm3),cc)
rtab <- exp(ctab)
print(rtab,digits=3)

## plot data
glm3_plot <-
  glmer(
    formula = ResponseFavorCoded ~ TypeNameShown * relational_Obl + (1 | SANPID),
    data = regData_s1,
    family = "binomial"
  )
df <- ggpredict(glm3_plot, terms = c("relational_Obl", "TypeNameShown"))
theme_set(theme_bw())
ggplot(data = as.data.frame(df),
       aes(
         y = predicted,
         x = x,
         color = group,
         fill = group
       )) +
  geom_line() +
  geom_ribbon(alpha = 0.1,
              aes(ymin = conf.low, ymax = conf.high),
              linetype = .5) +
  scale_color_manual(values = c("#F79082", "#2B7DCC", "#F67D1A")) +
  scale_fill_manual(values = c("#F79082", "#2B7DCC", "#F67D1A")) +
  ylab("Probability of selecting the Financial Literacy Task") +
  xlab("Reported feelings of relationship obligation") +
  theme(legend.position = "none")

## time allocation x relational obligation
## fit mixed effects model
mlm3 <- lmer(formula = DifferenceTime ~ TypeNameShown * relational_Obl_c + (1 | SANPID),
             data = regData_s1)
summary(mlm3)
regData_s1$TypeNameShown <- relevel(regData_s1$TypeNameShown, ref = "1") ## repeat with new reference


mlm4 <- lmer(formula = DifferenceTime ~ TypeNameShown + (1 | SANPID),
             data = regData_s1)
summary(mlm4)

anova(mlm3, mlm4)
confint(mlm3, parm = "beta_", method = "Wald")
simple_slopes(
  mlm3,
  levels = NULL,
  confint = FALSE,
  ci.width = .95,
  confint.method = c("Wald")
)

## plot effects for time allocation
mlm3_plot <- lmer(formula = DifferenceTime ~ TypeNameShown * relational_Obl + (1 | SANPID),
                  data = regData_s1)
df2 <- ggpredict(mlm3_plot, terms = c("relational_Obl", "TypeNameShown"))
theme_set(theme_bw())

ggplot(data = as.data.frame(df2),
       aes(
         y = predicted,
         x = x,
         color = group,
         fill = group
       )) +
  geom_line() +
  geom_ribbon(alpha = 0.1,
              aes(ymin = conf.low, ymax = conf.high),
              linetype = .5) +
  scale_color_manual(values = c("#0000CC", "#660099", "#CC0000")) +
  scale_fill_manual(values = c("#0000CC", "#660099", "#CC0000")) +
  ylab("Minutes allocated to Financial Literacy Task relative to Videogame") +
  xlab("Reported feelings of relationship obligation") +
  theme(legend.position = "none")