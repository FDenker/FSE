# 0 Install and/or load packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(haven,
               tidyr,
               dplyr)
#1
## load dataset with respondent data
erhebungsdaten <- read_dta("Erhebungsdaten.dta") %>% 
    mutate_at(vars(vig_1:r7), ~zap_labels(.)) %>% # keep only values for vars
    as_factor(.) # maintain labels for the rest

#2
## sort and save data
erhebungsdaten <- arrange(erhebungsdaten, deck)

#4
## load dataset with the vignette setup
fs_setup <- read_dta("FS-setup.dta") %>% 
    mutate_at(vars(degree), ~as_factor(.)) %>% 
    zap_labels()

#5
## reshape data to wide format and sort data
fs_setup_wd <- fs_setup %>% 
    arrange(vignr) %>% 
    pivot_wider(names_from = vignr, 
                values_from = c(sex:id_vignette))

## Reorder columns identical to Stata
cols <- names(fs_setup)[3:length(names(fs_setup))]
col_order <- paste(cols, rep(1:20, each = 9), sep = "_")
fs_setup_wd <- fs_setup_wd[, c("deck", col_order)]

#6
## merge both datasets, generate a numeric id (is good for some analyses) 
## and reshape data to long format

### Add columns from erhebungsdaten
fs_setup_wd <- fs_setup_wd %>% 
    left_join(erhebungsdaten, by = "deck") %>% 
    mutate(id_numeric = 1:nrow(.))

### From wide to long format
fs_setup_lg <- fs_setup_wd %>% 
    pivot_longer(c(sex_1:id_vignette_20, vig_1:vig_20),
                 names_to = c(".value", "vignr"),
                 names_pattern = "(.*)_(.*[0-9]$)")
### Reorder columns identical to Stata (diff is that vig_ is named vig here)
stata_ord <- c("id", "id_numeric", "vignr", "deck", "sex", "age", "degree", 
               "children", "job", "experience", "tenure", "income", "id_vignette", 
               "r1", "r2", "r3", "r5", "r6", "vig", "r7", "year")
fs_setup_lg <- fs_setup_lg[, stata_ord]

#7
## Generate new variables
### nat.log. of income because I assume a nonlinear connection
fs_setup_lg <- fs_setup_lg %>% 
    mutate(lninc = log(income),
           siops = recode(job, 
                          `1` = 19, 
                          `2` = 20, 
                          `3` = 43, 
                          `4` = 49, 
                          `5` = 32, 
                          `6` = 52, 
                          `7` = 51, 
                          `8` = 65, 
                          `9` = 70, 
                          `10` = 78),
           siops10 = siops/10,
           resp_age = year - r2,
           experience = factor(experience, labels = c("Short on", "Much")),
           tenure = factor(tenure, labels = c("entered recently", "entered a long time ago")),
           sex = relevel(factor(sex, labels = c("Male", "Female")), ref = "Female"))

### Rename variables
fs_setup_lg <- fs_setup_lg %>% 
    rename(resp_sex = r1,
           resp_earnings = r6,
           resp_difficult = r7,
           justice = vig) %>% 
    mutate(resp_foreign = r3,
           siops_centered = siops10 - mean(siops10),
           lninc_centered = lninc - mean(lninc))

#8
##  closeup and save data
saveRDS(fs_setup_lg, "Analyse.rds")
