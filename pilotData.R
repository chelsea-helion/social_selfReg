## social self regulation
## grant analyses for pilot data
## Chelsea Helion
rm(list = ls())

library(tidyverse)
library(skimr)
library(reghelper)
library(lme4)
library(lmerTest)
library(sjPlot)
library(corrplot)
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

## subset social and financial conditions
regData_s1 <-
  regData_s1 %>% 
  filter(TypeNameShownStr != "Celebrity")

## create relational obligation variable
regData_s1$relational_Obl <- (regData_s1$Moral_Obl + regData_s1$Let_Down) / 2
regData_s1$relational_Obl_c <- scale(regData_s1$relational_Obl, center = TRUE, scale = FALSE)

## Choice behavior analyses
## binary choice
regData_s1$TypeNameShown <- as.factor(regData_s1$TypeNameShown)
## 1 = Finance, 2 = Friend; Videogame = 1, Finance Task = 0
glm1 <-
  glmer(
    formula = ResponseFavorCoded ~ TypeNameShown + (1 | SANPID),
    data = regData_s1,
    family = "binomial"
  )
summary(glm1)

glm3 <-
  glmer(
    formula = ResponseFavorCoded ~ TypeNameShown * relational_Obl_c + (1 | SANPID),
    data = regData_s1,
    family = "binomial"
  )
summary(glm3)
confint(glm3, parm = "beta_", method = "Wald")


simple_slopes(
  glm3,
  levels = NULL,
  confint = FALSE,
  ci.width = .95,
  confint.method = c("Wald")
)

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
  scale_color_manual(values = c("#0000CC", "#CC0000")) +
  scale_fill_manual(values = c("#0000CC", "#CC0000")) +
  ylab("Probability of selecting the Financial Literacy Task") +
  xlab("Reported feelings of relationship obligation") +
  theme(legend.position = "none")