## Author: Nutchanon Yongsatianchot
## Code for Data Analysis of Hurricane One Shot Experiment

library(ggplot2)
library(readr)
library(brms)
library(mosaic)
library(dplyr)
library(ggpubr)

setwd("~/Hurricane Experiments All Materials")
dat <- read_csv("Hurricane Game One Shot - Prolific_February 9, 2022.csv")
dat <- dat[-c(1:2),] ##removing names and questions

## Cleaning data ##
dat$age <- as.numeric(dat$age)

dat$B1_life_1   <- as.numeric(dat$B1_life_1)
dat$B1_flood_1  <- as.numeric(dat$B1_flood_1)
dat$B1_outage_1 <- as.numeric(dat$B1_outage_1)
dat$B1_wind_1   <- as.numeric(dat$B1_wind_1)

dat$B2_life_1   <- as.numeric(dat$B2_life_1)
dat$B2_flood_1  <- as.numeric(dat$B2_flood_1)
dat$B2_outage_1 <- as.numeric(dat$B2_outage_1)
dat$B2_wind_1   <- as.numeric(dat$B2_wind_1)


dat$d2 <- dat$Decision2
dat$d2[dat$d2 == "Evacuate to a hotel up north paying at least $150 per night."] <- "Evac"
dat$d2[dat$d2 == "Stay in your place, a single-detached house, and ride out the storm."] <- "Stay"

dat$cond <- dat$Condition
dat$cond[dat$cond == "Decision2"] <- dat$d2[dat$cond=="Decision2"]
dat$cond <- factor(dat$cond, levels=c("Stay","Evac","No Decision"))

dat$diff_life   <- dat$B2_life_1   - dat$B1_life_1
dat$diff_flood  <- dat$B2_flood_1  - dat$B1_flood_1
dat$diff_outage <- dat$B2_outage_1 - dat$B1_outage_1
dat$diff_wind   <- dat$B2_wind_1   - dat$B1_wind_1

## Data Summary ## 

summary(dat$age)
table(dat$gender)
summary(as.numeric(dat$`FI1_B_Time_Page Submit`))
summary(as.numeric(dat$`SI2_B_Time_Page Submit`))


## Exclusion Criteria ##

### finish reading too fast
id_too_fast <- which(as.numeric(dat$`FI1_B_Time_Page Submit`) < 30 | 
                       as.numeric(dat$`SI2_B_Time_Page Submit`) < 15)

### extreme outlier above 4 s.d. 
sd <- 3
lb_life <- mean(dat$diff_life) - sd(dat$diff_life)*sd
ub_life <- mean(dat$diff_life) + sd(dat$diff_life)*sd
lb_wind <- mean(dat$diff_wind) - sd(dat$diff_wind)*sd
ub_wind <- mean(dat$diff_wind) + sd(dat$diff_wind)*sd
lb_flood <- mean(dat$diff_flood) - sd(dat$diff_flood)*sd
ub_flood <- mean(dat$diff_flood) + sd(dat$diff_flood)*sd
lb_outage <- mean(dat$diff_outage) - sd(dat$diff_outage)*sd
ub_outage <- mean(dat$diff_outage) + sd(dat$diff_outage)*sd

id_extreme <- which((dat$diff_life < lb_life) | (dat$diff_life > ub_life) | 
                      (dat$diff_wind < lb_wind) | (dat$diff_wind > ub_wind) |
                      (dat$diff_flood < lb_flood) | (dat$diff_flood > ub_flood) |
                      (dat$diff_outage < lb_outage) | (dat$diff_outage > ub_outage))

dat_exc_fast <- dat[-id_too_fast,]
dat_exc_ext <- dat[-id_extreme,]
dat_exc_all <- dat[-union(id_extreme,id_too_fast),]

dim(dat_exc_all)

## Fitting Regression ## 
### Post Bel ~ Cond + Pre Bel << Adjusted for pre Bel 
seed <- 77

m_life  <- brm(B2_life_1 ~ 0 + cond + B1_life_1, 
                data = dat_exc_all, family= gaussian(), 
                prior = c(prior(normal(0,10), class = b)),
                iter = 4000, seed = seed)
m_flood <- brm(B2_flood_1 ~ 0 + cond + B1_flood_1, 
                data = dat_exc_all, family= gaussian(),
                prior = c(prior(normal(0,5), class = b)),
                iter = 4000, seed = seed)
m_outage <- brm(B2_outage_1 ~ 0 + cond + B1_outage_1, 
                 data = dat_exc_all, family= gaussian(),
                 prior = c(prior(normal(0,5), class = b)),
                 iter = 4000, seed = seed)
m_wind  <- brm(B2_wind_1 ~ 0 + cond + B1_wind_1, 
                data = dat_exc_all, family= gaussian(),
                prior = c(prior(normal(0,10), class = b)),
                iter = 4000, seed = seed)

m_list <- list(m_life, m_flood, m_outage, m_wind)


m_life_fast  <- brm(B2_life_1 ~ 0 + cond + B1_life_1, 
               data = dat_exc_fast, family= gaussian(), 
               prior = c(prior(normal(0,10), class = b)),
               iter = 4000, seed = seed)
m_flood_fast <- brm(B2_flood_1 ~ 0 + cond + B1_flood_1, 
               data = dat_exc_fast, family= gaussian(),
               prior = c(prior(normal(0,5), class = b)),
               iter = 4000, seed = seed)
m_outage_fast <- brm(B2_outage_1 ~ 0 + cond + B1_outage_1, 
                data = dat_exc_fast, family= gaussian(),
                prior = c(prior(normal(0,5), class = b)),
                iter = 4000, seed = seed)
m_wind_fast  <- brm(B2_wind_1 ~ 0 + cond + B1_wind_1, 
               data = dat_exc_fast, family= gaussian(),
               prior = c(prior(normal(0,10), class = b)),
               iter = 4000, seed = seed)

m_fast_list <- list(m_life_fast, m_flood_fast, m_outage_fast, m_wind_fast)


m_life_ext  <- brm(B2_life_1 ~ 0 + cond + B1_life_1, 
                    data = dat_exc_ext, family= gaussian(), 
                    prior = c(prior(normal(0,10), class = b)),
                    iter = 4000, seed = seed)
m_flood_ext <- brm(B2_flood_1 ~ 0 + cond + B1_flood_1, 
                    data = dat_exc_ext, family= gaussian(),
                    prior = c(prior(normal(0,5), class = b)),
                    iter = 4000, seed = seed)
m_outage_ext <- brm(B2_outage_1 ~ 0 + cond + B1_outage_1, 
                     data = dat_exc_ext, family= gaussian(),
                     prior = c(prior(normal(0,5), class = b)),
                     iter = 4000, seed = seed)
m_wind_ext  <- brm(B2_wind_1 ~ 0 + cond + B1_wind_1, 
                    data = dat_exc_ext, family= gaussian(),
                    prior = c(prior(normal(0,10), class = b)),
                    iter = 4000, seed = seed)

m_ext_list <- list(m_life_ext, m_flood_ext, m_outage_ext, m_wind_ext)


## The diff model is the same as the post model 

m_diff_life <- brm(diff_life ~ 0 + cond + B1_life_1, data = dat_exc_all, 
                   family= gaussian(),
                   prior = c(prior(normal(0,10), class = b)),
                   iter = 4000, seed = seed)
m_diff_flood <- brm(diff_flood ~ 0 + cond + B1_flood_1, data = dat_exc_all, 
                    family= gaussian(),
                    prior = c(prior(normal(0,5), class = b)),
                    iter = 4000, seed = seed)
m_diff_outage <- brm(diff_outage ~ 0 + cond + B1_outage_1, data = dat_exc_all, 
                     family= gaussian(),
                     prior = c(prior(normal(0,5), class = b)),
                     iter = 4000, seed = seed)
m_diff_wind <- brm(diff_wind ~ 0 + cond + B1_wind_1, data = dat_exc_all, 
                   family= gaussian(),
                   prior = c(prior(normal(0,10), class = b)),
                   iter = 4000, seed = seed)

m_diff_list <- list(m_diff_life, m_diff_flood, m_diff_outage, m_diff_wind)


## Visualize the results ##
### There are two types: 1) Conditional_effects and 2) Hypothesis (testing diff)

### Conditional effects
#### Only plot the reported figure

g_life   <- plot(conditional_effects(m_life), ask=FALSE, plot=FALSE)[1]$cond
g_life   <- g_life + ylab("% Life-threatening") + xlab("Condition")
g_flood  <- plot(conditional_effects(m_flood), ask=FALSE, plot=FALSE)[1]$cond
g_flood  <- g_flood + ylab("Flood depth (inch)") + xlab("Condition")
g_outage <- plot(conditional_effects(m_outage), ask=FALSE, plot=FALSE)[1]$cond
g_outage <- g_outage + ylab("Outage (day)") + xlab("Condition")
g_wind   <- plot(conditional_effects(m_wind), ask=FALSE, plot=FALSE)[1]$cond
g_wind   <- g_wind + ylab("Wind Speed (mph)") + xlab("Condition")

g <- ggarrange(g_life, g_flood, g_outage, g_wind,
               labels = c("A","B","C","D"), 
               ncol = 2, nrow =2)
print(g)


g_life   <- plot(conditional_effects(m_diff_life), ask=FALSE, plot=FALSE)[1]$cond
g_life   <- g_life + ylab("% Life-threatening") + xlab("Condition")
g_flood  <- plot(conditional_effects(m_diff_flood), ask=FALSE, plot=FALSE)[1]$cond
g_flood  <- g_flood + ylab("Flood depth (inch)") + xlab("Condition")
g_outage <- plot(conditional_effects(m_diff_outage), ask=FALSE, plot=FALSE)[1]$cond
g_outage <- g_outage + ylab("Outage (day)") + xlab("Condition")
g_wind   <- plot(conditional_effects(m_diff_wind), ask=FALSE, plot=FALSE)[1]$cond
g_wind   <- g_wind + ylab("Wind Speed (mph)") + xlab("Condition")

g <- ggarrange(g_life, g_flood, g_outage, g_wind,
               labels = c("A","B","C","D"), 
               ncol = 2, nrow =2)
print(g)


### Hypothesis Testing

hypo <- c("condStay < condNoDecision",
          "condStay < condEvac",
          "condEvac < condNoDecision")

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  return(df)
}

gen_hypo_dat <- function(models, hypos)
{
  dat <- data.frame()
  for(h in hypos)
  {
    for(m in models)
    {
      temp <- hypothesis(m,h)
      dat <- rbind(dat,data.frame(temp$hypothesis))
    }
  }

  return(round_df(dat,3))
}

m_hypo <- gen_hypo_dat(m_list, hypo)
m_fast_hypo <- gen_hypo_dat(m_fast_list, hypo)
m_ext_hypo  <- gen_hypo_dat(m_ext_list, hypo)


View(m_hypo)