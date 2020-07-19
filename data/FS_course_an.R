# 0 Install and/or load packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(dplyr, # data management
               ggplot2, # plots
               psych, # detailed descriptives
               summarytools, # frequency tables
               olsrr, # Breusch-Pagan test
               sandwich, # Robust variance-covariance matrices
               lmtest, # Robust test of SE
               multiwayvcov, # Cluster standard errors
               stargazer,# Regression tables
               lme4, # Mixed-effects models
               sjPlot) # Tables for mixed-effects models 
#1
## load dataset
analyse <- readRDS("data/Analyse.rds")

#2
## Data Check
qplot(analyse$justice, y = ..density.., 
      geom = "histogram", binwidth = 1)

qplot(analyse$justice, geom = "blank") +
    geom_histogram(aes(y = ..density..), binwidth = 1, 
                   fill = "orange",  col = "black") +
    stat_function(fun = dnorm, 
                  args = c(mean = mean(analyse$justice), 
                                        sd = sd(analyse$justice)), 
                  col = "darkgreen") +
    ylab("Proportion")

describe(analyse$justice)
freq(analyse$justice)

freq(analyse$deck)

analyse %>% 
    select(sex, age, degree, children, job, experience, tenure, income) %>% 
    mutate_if(is.factor, ~as.numeric(.) *-1 + 2) %>% 
    cor(.)  %>%
    round(4)
    
freq(analyse$r3)
freq(analyse$r3[analyse$vignr == 1])

#3
## Regression Analysis: OLS & diagnostics

## a: OLS, i.i.d. assumption
ols <- lm(justice ~ sex + age + degree + children + experience + siops + 
            tenure + lninc, data = analyse)
summary(ols)

## Tests for heteroskedasticity
ols_test_breusch_pagan(ols)

## b: OLS, robust (Huber/White)
# reproduce the Stata default
robust <- coeftest(ols, vcov = vcovHC(ols, "HC1"))
robust

## c: OLS, cluster robust (Huber-Sandwich)
ols$clse <- cluster.vcov(ols, analyse$id) 
clust_robust <- coeftest(ols, ols$clse)

# Print the results in a nicer way
stargazer(ols, robust, clust_robust, type = "text", model.names = F,
          column.labels = c("OLS", "OLS robust", "OLS cluster"),
          dep.var.labels.include = F)

#4
## Cross-level analysis: using vignette dimensions and respondent characteristics
ols_cl <- lm(justice ~ sex + age + degree + children + experience + siops + 
               tenure + lninc + resp_difficult + resp_sex, data = analyse)

ols_cl$clse <- cluster.vcov(ols_cl, analyse$id) 
clust_robust_cl <- coeftest(ols_cl, ols_cl$clse)

ols_cli <- lm(justice ~ sex*resp_sex + age + degree + children + experience + 
                siops + tenure + lninc + resp_difficult, data = analyse)
ols_cli$clse <- cluster.vcov(ols_cli, analyse$id) 
clust_robust_cli <- coeftest(ols_cli, ols_cli$clse)

stargazer(clust_robust_cl, clust_robust_cli, clust_robust, type = "text", 
          model.names = F, column.labels = c("Cross level", "Cross level int"),
          dep.var.labels.include = F)


## Figure for subgroup comparisons
ols_cl_f <- ols_cl <- lm(justice ~ sex + age + degree + children + experience + 
                           siops + tenure + lninc, 
                         data = analyse[analyse$resp_sex == "female",])

ols_cl_f$clse <- cluster.vcov(ols_cl_f, analyse$id[analyse$resp_sex == "female"]) 
cl_f <- coeftest(ols_cl_f, ols_cl_f$clse)

ols_cl_m <- ols_cl <- lm(justice ~ sex + age + degree + children + experience + 
                           siops + tenure + lninc, 
                         data = analyse[analyse$resp_sex == "male",])
ols_cl_m$clse <- cluster.vcov(ols_cl_m, analyse$id[analyse$resp_sex == "male"]) 
cl_m <- coeftest(ols_cl_m, ols_cl_m$clse)

## Tables for comparisons
stargazer(cl_f, cl_m, type = "text", 
          model.names = F, column.labels = c("Female", "Male"),
          dep.var.labels.include = F)

#5
## Multi-level Regression Analysis random-intercepts & random slopes: MLE estimator
lme_ri <- lmer(justice ~ sex + age + degree + children + experience + siops + 
                 tenure + lninc + (1 | id), data = analyse, REML = F)

tab_model(lme_ri, digits = 4, show.se = T)

lme_rirs <- lmer(justice ~ sex + age + degree + children + experience + siops + 
                 tenure + lninc + (1 + lninc | id), data = analyse, REML = F)

tab_model(lme_rirs, digits = 4, show.se = T)
