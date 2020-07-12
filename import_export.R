install.packages("foreign")
library(haven)
stata_data <- read_dta(file="C:/Users/Frederic Denker/OneDrive - Zeppelin-University gGmbH/Dokumente/Semester 7/Factorial Survey Experiments/Datenauswertung/FS-setup.dta")

View(stata_data)

write_dta(stata_data, path ="C:/Users/Frederic Denker/OneDrive - Zeppelin-University gGmbH/Dokumente/Semester 7/Factorial Survey Experiments/Datenauswertung/FS-setup_FD.dta", version=12)
?write_dta

stata_data_2 <- read_dta(file="C:/Users/Frederic Denker/OneDrive - Zeppelin-University gGmbH/Dokumente/Semester 7/Factorial Survey Experiments/Datenauswertung/Erhebungsdaten.dta")

write_dta(stata_data_2, path ="C:/Users/Frederic Denker/OneDrive - Zeppelin-University gGmbH/Dokumente/Semester 7/Factorial Survey Experiments/Datenauswertung/Erhebungsdaten_2.dta", version=12)
names(stata_data_2)

my_stata_data <- read.csv("C:/Users/Frederic Denker/OneDrive - Zeppelin-University gGmbH/Dokumente/Semester 7/Factorial Survey Experiments/Datenauswertung/Befragung_Datenerhebung.csv")

View(my_stata_data)
colnames(my_stata_data) <- c("Befragten_id","deck", "Gender","Year_of_Birth","Country_of_Birth","Degree","vig_1","vig_2","vig_3","vig_4","vig_5","vig_6","vig_7","vig_8","vig_9","vig_10","vig_11","vig_12","difficulty","times_donated","amount_donated")
write_dta(my_stata_data, path ="C:/Users/Frederic Denker/OneDrive - Zeppelin-University gGmbH/Dokumente/Semester 7/Factorial Survey Experiments/Datenauswertung/my_Erhebungsdaten.dta", version=12)


vignette_data <- read_dta(file="C:/Users/Frederic Denker/OneDrive - Zeppelin-University gGmbH/Dokumente/Semester 7/Factorial Survey Experiments/Datenauswertung/vignette_wide2.dta")
View(vignette_data)
