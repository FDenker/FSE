library(readr)
library("readxl")
library(magrittr)
library(fastDummies)

# Starting with the responses

Responses <- read_csv("data/Befragung_Datenerhebung.csv")
colnames(Responses) <- c("befragten_id","deck", "gender","year_of_birth","country_of_birth","degree","vig_1","vig_2","vig_3","vig_4","vig_5","vig_6","vig_7","vig_8","vig_9","vig_10","vig_11","vig_12","difficulty","times_donated","amount_donated")

## Declaring NAs
Responses %<>% na_if("-99")
Responses$degree = recode(Responses$degree,"1"="High School","2"="Graduate","3"="Ph.D")
Responses$gender = recode(Responses$gender,"1"="Male","2"="Female")
Responses$country_of_birth = as.factor(Responses$country_of_birth)

Responses_long<- Responses %>% pivot_longer(starts_with("vig"),names_to = "question", values_to = "donation")
Responses_long$question_nr = substr(Responses_long$question,5,nchar(Responses_long$question))

Questions <- read_excel("data/vignette_wide2.xlsx")

#Deleting the full vignettes
Questions = Questions %>% select(-starts_with("vig"))
# Putting this into one wide data frame which can then be merged with the Responses_Long
Questions_sorted <- Questions[,c(1:7)]
temp = data.frame()
i= 2
while(i < 13){
  temp = Questions[,c(1,(i*6-4):(i*6+1))]
  print(names(temp))
  names(temp) <- names(Questions_sorted)
  Questions_sorted=as.data.frame(mapply(c, Questions_sorted,temp))
  i = i+1
}


names(Questions_sorted) = c("deck","id_vignette","origin","income","received","goals","channel")
freq(Questions_sorted$origin)
Questions_sorted$income = recode(Questions_sorted$income,"poor job"="poor_job",
                                 "rich job"="rich_job")

Questions_sorted$origin = recode(Questions_sorted$origin,"gift from mother"="gift_from_mother",
                                 "tax refund"="tax_refund")

Questions_sorted$channel = recode(Questions_sorted$channel,"direct transfer"="direct_transfer"
                                  ,"through person"="through_person")

Questions_sorted$goals = recode(Questions_sorted$goals, "state capacity"="state_capacity")

Questions_sorted$deck = as.numeric(Questions_sorted$deck)
Questions_sorted$id_vignette = as.numeric(Questions_sorted$id_vignette)
Questions_sorted$questions_nr = Questions_sorted$id_vignette- (Questions_sorted$deck-1)*12
Questions_sorted$join = paste0(Questions_sorted$deck, "|", Questions_sorted$questions_nr)
Responses_long$join = paste0(Responses_long$deck, "|", Responses_long$question_nr)

df = left_join(Responses_long,Questions_sorted, by = c("join"="join"))

## Cleaning up the table
df %<>% select(-c("questions_nr","join","deck.y","id_vignette"))
names(df)
## Recoding:



df = fastDummies::dummy_cols(df,select_columns = c("gender","degree","origin","income","received","goals","channel"),
                             remove_first_dummy =  TRUE )


## Deleting everything that is not needed anymore

rm(temp,i,Responses)