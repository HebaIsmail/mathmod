
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")


tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
glimpse(tbl)
tbl = select(tbl, -("roll"))
tbl = tbl %>% mutate_if(is.character, factor)
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 

sapply(tbl,is.numeric)
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
cor_td = cor(tbl_numeric)
cor_td
cor_td = cor(drop_na(tbl_numeric))
cor_td
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .2] %>% na.exclude
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""))

teaching_tbl = sample_n(tbl, floor(length(tbl$date)*.7))
testing_tbl = sample_n(tbl, floor(length(tbl$date)*.3))
row_numbers = 1:length(tbl$date)
teach = sample(row_numbers, floor(length(tbl$date)*.7))
test = row_numbers[-teach]

teaching_tbl_unq = tbl[teach,]
testing_tbl_unq = tbl[test,]
tbl_numeric= filter(tbl_numeric,DOY>240&DOY<330, daytime ==FALSE)
model=lm(formula, data = tbl_numeric)
summary(model)
anova(model)

formula2 = co2_flux~LE+h2o_flux+un_LE+rand_err_LE+rand_err_h2o_flux+un_co2_flux+un_h2o_flux 
model2=lm(formula2, data=tbl_numeric)
summary(model2)
anova(model2)
