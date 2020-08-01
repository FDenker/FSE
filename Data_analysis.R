#######Dorian####
library(tidyverse)
library(magrittr)

dx <- readRDS("~/Desktop/ZU/S7/Factorial Survey Experiments/FSE/df.RDS")
View(dx)

cond1 <- (dx$origin == "gift_from_mother" & dx$goals == "mothers")
cond2 <- (dx$origin == "bonus" & dx$goals == "entrepreneurs")
cond3 <- (dx$origin == "tax_refund" & dx$goals == "state_capacity")

dx$equal <- ifelse(cond1 | cond2 | cond3, 1,0)
dx$received <- dx$received %>% as.numeric()
dx$origin %<>% as.factor()
dx$goals %<>% as.factor()

dx$age <- 2020-dx$year_of_birth

# Only Male and Income Rich Job significant 
lm(donation ~ gender+income+received+channel+age+times_donated+amount_donated+
     equal,
   data = dx) %>% summary

#normalize donations
normalize <- function(x){
  if(sd(x) %>% is.numeric()) return((x-mean(x))/sd(x))
  else print("damn")
}


dx %<>% 
  group_by(befragten_id) %>%
  mutate("normaldonations" = normalize(donation))

#Only income rich significant
lm(normaldonations ~ gender +  equal + income +
     received + channel + age +
     times_donated + amount_donated, data = dx) %>%
  summary

#Relax Assumption of equal:
lm(donation ~ gender +income + received+
     channel_through_person + age+times_donated+amount_donated+
     origin*goals, data = dx) %>% summary

lm(normaldonations ~ gender +income + received+
     channel_through_person + age+times_donated+amount_donated+
     origin*goals, data = dx) %>% summary
#Inclusion of sex
#Interactions of: 
# - genderFemale:origingift_from_mother:goalsmothers
# - genderFemale:origintax_refund:goalsmothers 
# and
# - origintax_refund
# become significant

lm(donation ~ gender + 
     + income
   + received + age+times_donated+amount_donated+
     + channel_through_person
   + origin*goals:gender, data = dx) %>% summary

#In summary: we have nothing :D

#Hypothesen Testen:

# t.test(dx$donation[which(dx$origin == "gift_from_mother")],
#        dx$donation[which(dx$origin != "gift_from_mother")])

#Alternative Option:
t_test_mother=t.test(dx$donation[which(dx$origin == "gift_from_mother")],mu =  0)

# t.test(dx$donation[which(dx$origin == "bonus")],
#        dx$donation[which(dx$origin != "bonus")])

t_test_bonus=t.test(dx$donation[which(dx$origin == "bonus")],mu =  0)


# t.test(dx$donation[which(dx$origin == "tax_refund")],
#        dx$donation[which(dx$origin != "tax_refund")])

t_test_tax_refund= t.test(dx$donation[which(dx$origin == "tax_refund")],mu =  0)


t_tests=data.frame("Variable" = c("Gift from mother","Bonus","Tax refund"),
           "mean of x" = c("-0.54","-0.083","-0.576"),
           "t-value" = c("-3.178","-0.472","-3.3849"),
           "p-value" = c("0.002***","0.639","0.001***"))


#Hypothesis 2
lm(normaldonations ~ income, data = dx) %>% summary

#Hypothesis 3
lm(normaldonations ~ equal, data = dx) %>% summary
#3a,b,c

#Before executing the following line check which level is where with levels(dx$...) 
#should be set to bonus entrepreneurs
levels(dx$origin)
levels(dx$goals)

#dx$origin <- relevel(dx$origin,2)
#dx$goals <- relevel(dx$goals,1)

regprob <- lm(normaldonations ~ relevel(goals,ref="entrepreneurs"):relevel(origin, ref = "bonus"),
              data = dx)
summary(regprob)


#Here we are excluding the main effects of the donation goal (which are the most prominent ones)
#This is - to say the least - dubious. But i'm addicted to them Sternchen <3
#Signifikant: goalsentrepreneurs:originbonus


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


View(df)
df$equal <-
        ifelse((df$origin_gift_from_mother == 1 & df$goals_mothers == 1) |
              (df$origin_tax_refund == 1 & df$goals_state_capacity == 1) |
              (df$origin_bonus == 1 & df$goals_entrepreneurs == 1),1,0)
       
 lm(donation ~ gender_Male + 
            + income_rich_job
            + received_100+ received_1000 
            + channel_through_person,
            + equal,
            data = df) %>% summary
 df$received <- 
   ifelse(df$received %in% c(10,100,1000), paste(df$received), NA) %>% as.numeric
 df$channel <- 
   ifelse(df$channel %in% c("through_person","direct_transfer"),paste(df$channel),NA)

 lm(donation ~ equal +
               gender + 
               income_rich_job +
               amount_donated + 
               times_donated + 
               channel,
               data = df) %>% summary
 
class(df$degree)

 

 summary(ols_7)


Responses
colnames(Responses) <- c("befragten_id","deck", "gender","year_of_birth","country_of_birth","degree","vig_1","vig_2","vig_3","vig_4","vig_5","vig_6","vig_7","vig_8","vig_9","vig_10","vig_11","vig_12","difficulty","times_donated","amount_donated")
View(Responses)
