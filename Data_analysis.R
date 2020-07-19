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
               sjPlot, # Tables for mixed-effects models 
               fastDummies) 
qplot(joined_data$Donation, y = ..density.., 
      geom = "histogram", binwidth = 1)
describe(joined_data$Donation)

qplot(joined_data$Donation, geom = "blank") +
  geom_histogram(aes(y = ..density..), binwidth = 1, 
                 fill = "orange",  col = "black") +
  stat_function(fun = dnorm, 
                args = c(mean = mean(joined_data$Donation), 
                         sd = sd(joined_data$Donation)), 
                col = "darkgreen") +
  ylab("Proportion")

freq(joined_data$Donation)

ols <- lm(donation ~ gender + year_of_birth + degree + difficulty + amount_donated, data = df)
summary(ols)


ols_2 <- lm(donation ~ gender_Male + 
              origin_tax_refund + origin_gift_from_mother
            + income_rich_job
            + received_100+ received_1000 
            + goals_mothers + goals_state_capacity
            + channel_through_person
            + difficulty, data = df)
summary(ols_2)

ols_3 <- lm(donation ~ gender_Male + 
              origin
            + income_rich_job
            + received
            + goals
            + channel
            + origin_gift_from_mother*goals_mothers, data = df)
summary(ols_3)

ols_4 <- lm(donation ~ gender_Male + 
              origin_tax_refund + origin_gift_from_mother
            + income_rich_job
            + received_100+ received_1000 
            + goals_mothers + goals_state_capacity
            + channel_through_person
            + origin_gift_from_mother*goals_mothers*gender_Male, data = df)
summary(ols_4)

ols_5 <- lm(donation ~ gender_Male + 
              origin_tax_refund + origin_gift_from_mother
            + income_rich_job
            + received_100+ received_1000 
            + goals_mothers + goals_state_capacity
            + channel_through_person
           # + origin_gift_from_mother*goals_mothers*gender_Male
            + origin_tax_refund*goals_state_capacity, data = df)
summary(ols_5)

ols_6 <- lm(donation ~ gender_Male + 
              origin_tax_refund + origin_gift_from_mother
            + income_rich_job
            + received_100+ received_1000 
            + goals_mothers + goals_state_capacity
            + channel_through_person
            # + origin_gift_from_mother*goals_mothers*gender_Male
            + origin_tax_refund*goals_state_capacity*gender_Male, data = df)
summary(ols_6)

ols_7 <- lm(donation ~ gender_Male + 
              origin_tax_refund + origin_gift_from_mother
            + income_rich_job
            + received_100+ received_1000 
            + goals_mothers + goals_state_capacity
            + channel_through_person
            + origin_gift_from_mother*goals_mothers*gender_Male
            + origin_tax_refund*goals_state_capacity*gender_Male, data = df)
summary(ols_7)

df$question_nr = as.numeric(df$question_nr)
ols_test <- lm(donation ~ question_nr, data=df)
summary(ols_test)
