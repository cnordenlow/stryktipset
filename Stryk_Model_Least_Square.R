



require(tidyverse)
library(readxl)
library(csv)
library(tidyr)
library(modelr)

library(tidyverse)
library(dplyr)
library(reshape2)
library(gridExtra)
library(ggplot2)
options(na.action = na.warn)

setwd("c:/Users/chris/Documents/R_Studio/")

dat <-read_excel('data_stryk_test_loop.xlsx')
#indata <-read_excel('test_bucket_r.xlsm')

#import levels
url_row_levels <- ("https://raw.githubusercontent.com/cnordenlow/stryktipset/master/input_optimal_row.csv")
us_states <-read_csv2(url(url_row_levels))

#Expected outcome level
lvl = "level5"


#Criterias Row
###Criterias
###
min_1 = 3
min_0 = 3
min_2 = 3
max_1 = 6
max_0 = 5
max_2 = 4

#Min game value / spelvarde
min_value = 7
#Minimun totalt spelvarde 
min_total_value = 15

#Odds inom buckets
bucket_1_min = 15
bucket_1_max = 21
bucket_2_max = 35
bucket_3_max = 50
bucket_4_max = 65

#Min antal inom bucket
crit_min_buck_1 = 1
crit_min_buck_2 = 8
crit_min_buck_3 = 10
crit_min_buck_4 = 11


##Which row number?
select_row =1

#
#Criterias Garderingar
#
#Best?m antal 1
number_1 = 8

#max antal
number_0 = 5
number_2 = 8



#
number_gardering = 5
#
min_pos_gard_value = 1





#Borde man begransa hur manga av de med h?gst odds som man far?

###Nedan testar alla m?jiga kombinationer
#https://stackoverflow.com/questions/18705153/generate-list-of-all-possible-combinations-of-elements-of-vector
###



#Calculate value
dat <- dat %>%
  mutate(value_1 = odds_1 - svfo_1)%>%
  mutate(value_0 = odds_0 - svfo_0)%>%
  mutate(value_2 = odds_2 - svfo_2)

##Combinations odds
data_odds <- dat%>%
  select(odds_1, odds_0, odds_2)

data_odds <- as.data.frame(t(data_odds))

#all_combinations_odds <- expand(data_odds, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13)
all_combinations_odds <- expand.grid(data_odds)


all_combinations_odds <- tibble::rownames_to_column(all_combinations_odds, "Row")


##Combinations value
data_value <- dat%>%
  select(value_1, value_0, value_2)

data_value <- as.data.frame(t(data_value))

#all_combinations_value <- expand(data_value, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13)
all_combinations_value <- expand.grid(data_value)


all_combinations_value <- tibble::rownames_to_column(all_combinations_value, "Row")





#all combinations 1x2
data_1x2 <- dat%>%
  select(res_1, res_0, res_2)

data_1x2 <- as.data.frame(t(data_1x2))

#all_combinations_1x2 <- expand(data_1x2, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13)
all_combinations_1x2 <- expand.grid(data_1x2)


all_combinations_1x2 <- tibble::rownames_to_column(all_combinations_1x2, "Row")


###Calculation

### Criteriacount number of 1,x,2 per row
all_combinations_1x2$obs_1 <- rowSums(all_combinations_1x2[,2:14] == 1)
all_combinations_1x2$obs_0 <- rowSums(all_combinations_1x2[,2:14] == 0)
all_combinations_1x2$obs_2 <- rowSums(all_combinations_1x2[,2:14] == 2)

all_combinations_1x2 <- all_combinations_1x2 %>%
  filter(obs_1 <= max_1 & obs_0 <= max_0 & obs_2 <= max_2)%>%
  filter(obs_1 >= min_1 & obs_0 >= min_0 & obs_2 >= min_2)

#Decrease number of rows in all_combinations_odds
temp <- all_combinations_1x2 %>%
  select(Row)

all_combinations_odds <- left_join(temp, all_combinations_odds, by = "Row")
all_combinations_value <- left_join(temp, all_combinations_value, by = "Row")


### Criteria: Calculate value  / spelvarde.
#regel summa spelv?rde som minst -25
all_combinations_value$obs_value <- rowSums(all_combinations_value[,2:14] > 0)

all_combinations_value <- all_combinations_value%>%
  mutate(total_value = V1 + V2 + V3 +V4 +V5+V6+V7+V8+V9+V10+V11+V12+V13)%>%
  filter(obs_value >= min_value)%>%
  filter(total_value > min_total_value)


#Decrease number of rows from new criteria
temp <- all_combinations_value %>%
  select(Row)

all_combinations_odds <- left_join(temp, all_combinations_odds, by = "Row")%>%
  mutate(total_odds = V1 + V2 + V3 +V4 +V5+V6+V7+V8+V9+V10+V11+V12+V13)

all_combinations_1x2 <- left_join(temp, all_combinations_1x2, by = "Row")







##### Run least square method. Least deviation from model row.

####
###
data_model_row <- dat %>%
  select(level_5)

data_model_row <- as.data.frame(t(data_model_row))


# ta ut relevanta rader, sortera varje rad med apply
all_rows_to_be_predicted <- all_combinations_odds %>%
  select(V1:V13)
all_rows_to_be_predicted <- t(apply(all_rows_to_be_predicted, 1, sort))


#ta ut rad from combinations, sen binda med de sorterade matchoddsen. Detta steg ar da det ej gick att kora mutate pa tabellen dar apply anvants

help_table <- all_combinations_odds %>%
  select(Row)
all_rows_to_be_predicted <- cbind(help_table, all_rows_to_be_predicted)

colnames(all_rows_to_be_predicted) <- c("Row", "V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11", "V12","V13")

#berakna diff mot indata (gor detta sen dynamisk sa man kan valja olika optimala rader)
all_rows_to_be_predicted <- all_rows_to_be_predicted %>%
  mutate(
    diff_1 = (all_rows_to_be_predicted$V1 - data_model_row$V1)^2,
    diff_2 = (all_rows_to_be_predicted$V2 - data_model_row$V2)^2,
    diff_3 = (all_rows_to_be_predicted$V3 - data_model_row$V3)^2,
    diff_4 = (all_rows_to_be_predicted$V4 - data_model_row$V4)^2,
    diff_5 = (all_rows_to_be_predicted$V5 - data_model_row$V5)^2,
    diff_6 = (all_rows_to_be_predicted$V6 - data_model_row$V6)^2,
    diff_7 = (all_rows_to_be_predicted$V7 - data_model_row$V7)^2,
    diff_8 = (all_rows_to_be_predicted$V8 - data_model_row$V8)^2,
    diff_9 = (all_rows_to_be_predicted$V9 - data_model_row$V9)^2,
    diff_10 = (all_rows_to_be_predicted$V10 - data_model_row$V10)^2,
    diff_11 = (all_rows_to_be_predicted$V11 - data_model_row$V11)^2,
    diff_12 = (all_rows_to_be_predicted$V12 - data_model_row$V12)^2,
    diff_13 = (all_rows_to_be_predicted$V13 - data_model_row$V13)^2,
  )%>%
  select(Row, diff_1:diff_13)

all_rows_to_be_predicted <- all_rows_to_be_predicted %>%
  mutate(square_mean = sqrt(rowMeans(all_rows_to_be_predicted[2:14])))

#binda ihop med ursprungligt dataset

all_rows_to_be_predicted <- all_rows_to_be_predicted%>%
  select(Row, square_mean)

all_combinations_odds <- left_join(all_combinations_odds, all_rows_to_be_predicted, by = "Row", all.x = TRUE)
all_combinations_1x2 <- left_join(all_combinations_1x2, all_rows_to_be_predicted, by = "Row", all.x = TRUE)

all_combinations_odds <- arrange(all_combinations_odds, (square_mean))
all_combinations_1x2 <- arrange(all_combinations_1x2, (square_mean))




#vald rad  
best_row_1x2 <- all_combinations_1x2 %>%
  slice(which(row_number() == select_row))
#filter(total_odds == max(total_odds))
#filter(Row == 123455)

#ta ut number of 1,x,2
n_1_in_row = best_row_1x2$obs_1
n_x_in_row = best_row_1x2$obs_0
n_2_in_row = best_row_1x2$obs_2

best_row_1x2 <- best_row_1x2 %>%
  select(V1:V13)
best_row_1x2 <- as.data.frame(t(best_row_1x2))%>%
  rename(tecken = V1)
best_row_1x2 <- tibble::rownames_to_column(best_row_1x2, "Row")


best_row_odds <- all_combinations_odds %>%
  slice(which(row_number() == select_row))%>%
  #filter(total_odds == max(total_odds))%>%
  select(V1:V13)

best_row_odds <- as.data.frame(t(best_row_odds))%>%
  rename(Odds = V1)
best_row_odds <- tibble::rownames_to_column(best_row_odds, "Row")


#test
#test <- all_combinations_1x2%>%



best_row_1x2 <- full_join(best_row_1x2, best_row_odds, by = "Row", all = TRUE)





#########
#Gardering, chose the ones to gardera
#######

gardering_row <- arrange(best_row_1x2, Odds)       
gardering_row <- slice(gardering_row, 1:number_gardering)%>%
  select(-Odds)


#indata
gard_data <- dat%>%
  mutate(Match = paste("V",Match,sep=""))

#gard_data_2 <- left_join(gardering_row, gard_data, by = c(Row, Match), all.x = TRUE)


#gard_data <- column_to_rownames(gard_data, 'Match')
#gardering_row <- column_to_rownames(gardering_row, 'Row')

gard_data <- gard_data%>%
  mutate(Row = Match)
gard_data_2 <- inner_join(gardering_row, gard_data, by = "Row")

###Ta ut garderingsalternativen
gard_data_2 <- gard_data_2 %>%
  mutate(gard_alt_1 = case_when(
    tecken == 1 ~ res_0,
    tecken == 0 ~ res_2,
    tecken == 2 ~ res_1))%>%
  mutate(gard_alt_2 = case_when(
    tecken == 1 ~ res_2,
    tecken == 0 ~ res_1,
    tecken == 2 ~ res_0))%>%           
  
  mutate(gard_odds_1 = case_when(
    tecken == 1 ~ odds_0,
    tecken == 0 ~ odds_2,
    tecken == 2 ~ odds_1))%>% 
  mutate(gard_odds_2 = case_when(
    tecken == 1 ~ odds_2,
    tecken == 0 ~ odds_1,
    tecken == 2 ~ odds_0))%>%
  
  mutate(gard_value_1 = case_when(
    tecken == 1 ~ value_0,
    tecken == 0 ~ value_2,
    tecken == 2 ~ value_1))%>% 
  mutate(gard_value_2 = case_when(
    tecken == 1 ~ value_2,
    tecken == 0 ~ value_1,
    tecken == 2 ~ value_0))%>%
  select(Match, gard_alt_1,gard_alt_2,gard_odds_1,gard_odds_2,gard_value_1,gard_value_2)


####
##Garderings optimization
###

###Maste losa hur radnumret fortsatt ska hanga med

#criteria

max_gard_1 = number_1 - n_1_in_row
max_gard_0 = number_0 - n_x_in_row
max_gard_2 = number_2 - n_2_in_row

#antal 1. best?m antal 1
number_1_to_gardera = number_1 - n_1_in_row



##Combinations odds
data_gard_odds <- gard_data_2%>%
  select(Match, gard_odds_1, gard_odds_2)

data_gard_odds <- tibble::column_to_rownames(data_gard_odds, "Match")

data_gard_odds <- as.data.frame(t(data_gard_odds))

gardering_combinations_odds <- expand.grid(data_gard_odds)

gardering_combinations_odds <- tibble::rownames_to_column(gardering_combinations_odds, "Row")


##Gardering Combinations tecken
data_gard_tecken <- gard_data_2%>%
  select(Match, gard_alt_1, gard_alt_2)
data_gard_tecken <- tibble::column_to_rownames(data_gard_tecken, "Match")

data_gard_tecken <- as.data.frame(t(data_gard_tecken))

gardering_combinations_1x2 <- expand.grid(data_gard_tecken)

gardering_combinations_1x2 <- tibble::rownames_to_column(gardering_combinations_1x2, "Row")


##Gardering Combinations value
data_gard_value <- gard_data_2%>%
  select(Match, gard_value_1, gard_value_2)

data_gard_value <- tibble::column_to_rownames(data_gard_value, "Match")

data_gard_value <- as.data.frame(t(data_gard_value))

gardering_combinations_value <- expand.grid(data_gard_value)

gardering_combinations_value <- tibble::rownames_to_column(gardering_combinations_value, "Row")

####################3


columns = number_gardering + 1

### Criteriacount number of 1,x,2 per row
gardering_combinations_1x2$obs_1 <- rowSums(gardering_combinations_1x2[,2:columns] == 1)
gardering_combinations_1x2$obs_0 <- rowSums(gardering_combinations_1x2[,2:columns] == 0)
gardering_combinations_1x2$obs_2 <- rowSums(gardering_combinations_1x2[,2:columns] == 2)

gardering_combinations_1x2 <- gardering_combinations_1x2 %>%
  filter(obs_1 == number_1_to_gardera & obs_0 <= max_gard_0 & obs_2 <= max_gard_2)

#Decrease number of rows in all_combinations_odds
temp <- gardering_combinations_1x2 %>%
  select(Row)

gardering_combinations_odds <- left_join(temp, gardering_combinations_odds, by = "Row")
gardering_combinations_value <- left_join(temp, gardering_combinations_value, by = "Row")




gardering_combinations_value$pos_val <- rowSums(gardering_combinations_value[,2:columns] > 0)
gardering_combinations_value <- gardering_combinations_value %>%
  filter(pos_val >= min_pos_gard_value)

#Decrease number of rows in all_combinations_odds
temp <- gardering_combinations_value %>%
  select(Row)

gardering_combinations_odds <- left_join(temp, gardering_combinations_odds, by = "Row")
gardering_combinations_1x2 <- left_join(temp, gardering_combinations_1x2, by = "Row")






###Sum odds
gardering_combinations_odds$total_odds = rowSums(gardering_combinations_odds[,2:columns])


#joina
temp <- gardering_combinations_odds %>%
  select(Row, total_odds)

gardering_combinations_value <- left_join(temp, gardering_combinations_value, by = "Row")
gardering_combinations_1x2 <- left_join(temp, gardering_combinations_1x2, by = "Row")

gardering_combinations_odds <- arrange(gardering_combinations_odds, desc(total_odds))
gardering_combinations_1x2 <- arrange(gardering_combinations_1x2, desc(total_odds))
gardering_combinations_value <- arrange(gardering_combinations_value, desc(total_odds))


#  gardering_combinations_1x2 <- gardering_combinations_1x2 %>%


##join row with garderings
columns = columns +1    
choice_of_garderingar <- gardering_combinations_1x2 %>%
  filter(total_odds == max(total_odds))%>%
  select(3:columns)

choice_of_garderingar <- as.data.frame(t(choice_of_garderingar))%>%
  rename(Gardering = V1)

choice_of_garderingar <- tibble::rownames_to_column(choice_of_garderingar, "Row")


best_row_1x2 <- left_join(best_row_1x2, choice_of_garderingar, by = "Row")

view(best_row_1x2)






#Get optimala rader
#indata <- dat%>%
#  select(level_5)

### lagg till valmojlighet att valja olika levels

#indata_2 <- as.data.frame(t(indata))

#odds <- all_combinations_odds%>%
  #select(V1:V13)


#test_odds <- odds%>%
 # slice(1)
#test_odds <- as.data.frame(t(test_odds))

#test_odds <- arrange(test_odds, V1)
#data <- tibble::rownames_to_column(test_odds, "a1")

#colnames(data)[colnames(data) == "V1"] <- "a2" # Rename column


#model1 <- function(a, data){
#  a[1] + indata$optimal * a[2]
#}

#model1(data$a2, indata)




#testtest <- cbind(indata, data)

#testtest <- testtest %>%
#  mutate(diff = (a2 - optimal)^2)
#  sqrt(mean(testtest$diff))
  
  ##1 import data optimal row
  
  ##sortera varje rad, ber?kna diff mot optimal rad
  
  # ta ut relevanta rader, sortera varje rad med apply
#test_sort_row <- all_combinations_odds %>%
#    select(V1:V13)
#test_sort_row <- t(apply(test_sort_row, 1, sort))


#ta ut rad from combinations, sen binda med de sorterade matchoddsen. Detta steg ar da det ej gick att kora mutate pa tabellen dar apply anvants

#hej <- all_combinations_odds %>%
#  select(Row)
#hej1 <- cbind(hej, test_sort_row)

#colnames(hej1) <- c("Row", "V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11", "V12","V13")

#berakna diff mot indata (gor detta sen dynamisk sa man kan valja olika optimala rader)
#hej1 <- hej1 %>%
#  mutate(
 #   diff_1 = (hej1$V1 - indata_2$V1)^2,
#    diff_2 = (hej1$V2 - indata_2$V2)^2,
#   diff_3 = (hej1$V3 - indata_2$V3)^2,
#   diff_4 = (hej1$V4 - indata_2$V4)^2,
#   diff_5 = (hej1$V5 - indata_2$V5)^2,
#   diff_6 = (hej1$V6 - indata_2$V6)^2,
#   diff_7 = (hej1$V7 - indata_2$V7)^2,
#   diff_8 = (hej1$V8 - indata_2$V8)^2,
#   diff_9 = (hej1$V9 - indata_2$V9)^2,
#   diff_10 = (hej1$V10 - indata_2$V10)^2,
#   diff_11 = (hej1$V11 - indata_2$V11)^2,
#   diff_12 = (hej1$V12 - indata_2$V12)^2,
#   diff_13 = (hej1$V13 - indata_2$V13)^2,
# )%>%
# select(Row, diff_1:diff_13)

#hej1 <- hej1 %>%
#  mutate(square_mean = sqrt(rowMeans(hej1[2:14])))

#binda ihop med ursprungligt dataset

#hej1 <- hej1%>%
# select(Row, square_mean)

#all_combinations_odds <- left_join(all_combinations_odds, hej1, by = "Row", all.x = TRUE)
#all_combinations_1x2 <- left_join(all_combinations_1x2, hej1, by = "Row", all.x = TRUE)

#all_combinations_odds <- arrange(all_combinations_odds, (square_mean))
#all_combinations_1x2 <- arrange(all_combinations_1x2, (square_mean))
