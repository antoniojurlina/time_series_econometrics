#-------- packages --------
library(tidyverse)
library(tidylog)
library(rebus)
library(readxl)
library(lubridate)

#-------- data and directory --------
directory <- paste0(here::here(), "/data")
setwd(directory)

tab_2020 <- read_xlsx("Maine Traffic - DOT Sensor.xlsx", sheet = "2020", skip = 2)
tab_2019 <- read_xlsx("Maine Traffic - DOT Sensor.xlsx", sheet = "2019", skip = 2)
classifications_2019 <- read_xlsx("Maine Traffic - DOT Sensor.xlsx", range = "'2019 Classifications'!A1:CA37")
classifications_2020 <- read_xlsx("Maine Traffic - DOT Sensor.xlsx", sheet = "2020 Classifications")
other_class_data <- read_xlsx("Maine Traffic - DOT Sensor.xlsx", sheet = "Other Class Data")

class <- read_csv("class temp.csv")
interchange <- read_csv("interchange temp.csv")
comparison_1 <- read_csv("comparison1 temp.csv")
comparison_2 <- read_csv("comparison2 temp.csv")

tax_revenues <- read_csv("tax_revenues.csv") %>%
  select(-X18)

unemployment_claims <- read_csv("unemployment_claims.csv") %>%
  filter(!is.na(State))
unemployment <- read_csv("unemployment.csv") 
monthly_inflation <- read_csv("monthly_inflation.csv")
fuel_efficiency <- read_csv("fuel_efficiency.csv")

#-------- cleaning tab_2019 --------
fix <- c("3/31/19",	"4/1/19",	
         "4/2/19",	"4/3/19",	
         "4/4/19",	"4/5/19",	
         "4/6/19",	"4/7/19",	
         "4/8/19",	"4/9/19",	
         "4/10/19",	"4/11/19",	
         "4/12/19",	"4/13/19") %>%
  mdy() %>% as.character()
  

dates <- tibble(date = unlist(tab_2019[1, 5:41]),
                day = colnames(tab_2019)[5:41])

dates[24:37, 1] <- fix

dates <- dates %>%
  mutate(date = ymd(date))

tab_2019 <- tab_2019[-c(1, 66, 67), ] %>%
  select(-(42:46))

for(i in 5:41){
  vector <- tab_2019[,i] %>%
    as.character() %>%
    str_split(pattern = ",") %>%
    unlist()
  
  numbers <- vector %>%
    str_detect(pattern = one_or_more(DGT)) %>%
    which()
  
  NAs <- vector %>%
    str_detect(pattern = one_or_more("N")) %>%
    which()
  
  vector[numbers] <- str_extract(vector[numbers], pattern = one_or_more(DGT))
  
  vector[NAs] <- NA
  
  vector <- as.numeric(vector)
  
  tab_2019[, i] <- vector
}

tab_2019 <- tab_2019 %>%
  pivot_longer(cols = 5:41, names_to = "day", values_to = "traffic")

tab_2019 <- left_join(tab_2019, dates, by = "day") %>%
  mutate(day = str_remove_all(tab_2019$day, 
                              pattern = "..." %R% one_or_more(DGT)))

days_short <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
days_long <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

for(i in seq_along(days_short)) {
  tab_2019$day[which(str_detect(tab_2019$day, 
                                pattern = days_short[i]))] <- days_long[i]
}

tab_2019$code <- str_extract_all(tab_2019$`Site Name`, pattern = SPACE %R% one_or_more(DGT) %R% END) %>% unlist %>%
  str_remove_all(pattern = " ") %>% as.numeric()

tab_2019$`Site Name` <- str_remove_all(tab_2019$`Site Name`, 
                                       pattern = SPACE %R% one_or_more(DGT) %R% END) %>% 
  str_to_title()

colnames(tab_2019) <- c("id", "town", "county", "description", "day", "traffic", "date", "code")

tab_2019 <- tab_2019 %>%
  select(id, code, county, town, description, date, day, traffic)

#-------- cleaning tab_2020 --------
fix <- c("3/17/20",	"3/18/20",	"3/19/20",	
         "3/20/20",	"3/21/20",	"3/22/20",	
         "3/23/20",	"3/24/20",	"3/25/20",	
         "3/26/20",	"3/27/20",	"3/28/20",	
         "3/29/20",	"3/30/20",	"3/31/20",	
         "4/1/20", "4/2/20", "4/3/20", 
         "4/4/20",	"4/5/20",	"4/6/20",	
         "4/7/20",	"4/8/20",	"4/9/20",	
         "4/10/20", "4/11/20") %>%
  mdy() %>% as.character()

dates <- tibble(date = unlist(tab_2020[1, 5:41]),
                day = colnames(tab_2020)[5:41])

dates[12:37, 1] <- fix

dates <- dates %>%
  mutate(date = ymd(date))

tab_2020 <- tab_2020[-c(1, 67, 68), ]

for(i in 5:41){
  vector <- tab_2020[,i] %>%
    as.character() %>%
    str_split(pattern = ",") %>%
    unlist()
  
  numbers <- vector %>%
    str_detect(pattern = one_or_more(DGT)) %>%
    which()
  
  NAs <- vector %>%
    str_detect(pattern = one_or_more("N")) %>%
    which()
  
  vector[numbers] <- str_extract(vector[numbers], pattern = one_or_more(DGT))
  
  vector[NAs] <- NA
  
  vector <- as.numeric(vector)
  
  tab_2020[, i] <- vector
}

tab_2020 <- tab_2020 %>%
  pivot_longer(cols = 5:41, names_to = "day", values_to = "traffic")

tab_2020 <- left_join(tab_2020, dates, by = "day") %>%
  mutate(day = str_remove_all(tab_2020$day, 
                              pattern = "..." %R% one_or_more(DGT)))

days_short <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
days_long <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

for(i in seq_along(days_short)) {
  tab_2020$day[which(str_detect(tab_2020$day, 
                                pattern = days_short[i]))] <- days_long[i]
}

tab_2020$code <- str_extract_all(tab_2020$`Site Name`, pattern = SPACE %R% one_or_more(DGT) %R% END) %>% unlist %>%
  str_remove_all(pattern = " ") %>% as.numeric()

tab_2020$`Site Name` <- str_remove_all(tab_2020$`Site Name`, 
                                       pattern = SPACE %R% one_or_more(DGT) %R% END) %>% 
  str_to_title()

colnames(tab_2020) <- c("id", "town", "county", "description", "day", "traffic", "date", "code")

tab_2020 <- tab_2020 %>%
  select(id, code, county, town, description, date, day, traffic)

#-------- cleaning classifications_2019 --------
types <- tibble(type = unlist(classifications_2019[1, -1]),
                place = colnames(classifications_2019)[-1])

types$type[which(str_detect(types$type, pattern = "Pass"))] <- "Pass. Veh"
types$type[which(str_detect(types$type, pattern = "SU"))] <- "SU Trucks"

classifications_2019 <- classifications_2019[-1, ] 

classifications_2019$day <- str_extract_all(classifications_2019$...1,
                                            pattern = START %R% 
                                              one_or_more(WRD) %R% 
                                              SPACE) %>% 
  unlist() %>% 
  str_remove(pattern = " ")

classifications_2019$...1 <- str_remove_all(classifications_2019$...1, 
                                            pattern = START %R% 
                                              one_or_more(WRD) %R% 
                                              SPACE)

classifications_2019$...1 <- dmy(classifications_2019$...1)

for(i in 2:79){
  vector <- classifications_2019[,i] %>%
    as.character() %>%
    str_split(pattern = ",") %>%
    unlist()
  
  numbers <- vector %>%
    str_detect(pattern = one_or_more(DGT)) %>%
    which()
  
  NAs <- vector %>%
    str_detect(pattern = one_or_more("N")) %>%
    which()
  
  vector[numbers] <- str_extract(vector[numbers], pattern = one_or_more(DGT))
  
  vector[NAs] <- NA
  
  vector <- as.numeric(vector)
  
  classifications_2019[, i] <- vector
}

index <- str_detect(colnames(classifications_2019), pattern = DOT %R% DOT %R% DOT) %>%
  which()

names <- colnames(classifications_2019)[-index] %>% head(n = -1)

for(i in seq_along(names)) {
  number <- which(colnames(classifications_2019) == names[i])
  colnames(classifications_2019)[number + 1] <- paste0(names[i], "_1")
  colnames(classifications_2019)[number + 2] <- paste0(names[i], "_2")
}

colnames(classifications_2019)[1] <- "date"

types$place <- colnames(classifications_2019)[-c(1, 80)]

classifications_2019 <- pivot_longer(classifications_2019, cols = 2:79, names_to = "place", values_to = "traffic")

classifications_2019 <- left_join(classifications_2019, types, by = "place")

classifications_2019$place <- str_remove_all(classifications_2019$place, pattern = "_" %R% DGT)

classifications_2019 <- separate(classifications_2019, col = place, into = c("town", "description"), sep = "\\(")

classifications_2019$description <- str_remove_all(classifications_2019$description, pattern = "\\)")

#-------- cleaning classifications_2020 --------
types <- tibble(type = unlist(classifications_2020[1, -1]),
                place = colnames(classifications_2020)[-1])

types$type[which(str_detect(types$type, pattern = "Pass"))] <- "Pass. Veh"
types$type[which(str_detect(types$type, pattern = "SU"))] <- "SU Trucks"

classifications_2020 <- classifications_2020[-1, ] 

classifications_2020$day <- str_extract_all(classifications_2020$...1,
                                            pattern = START %R% 
                                              one_or_more(WRD) %R% 
                                              SPACE) %>% 
  unlist() %>% 
  str_remove(pattern = " ")

classifications_2020$...1 <- str_remove_all(classifications_2020$...1, 
                                            pattern = START %R% 
                                              one_or_more(WRD) %R% 
                                              SPACE)

classifications_2020$...1 <- dmy(classifications_2020$...1)

for(i in 2:79){
  vector <- classifications_2020[,i] %>%
    as.character() %>%
    str_split(pattern = ",") %>%
    unlist()
  
  numbers <- vector %>%
    str_detect(pattern = one_or_more(DGT)) %>%
    which()
  
  NAs <- vector %>%
    str_detect(pattern = one_or_more("N")) %>%
    which()
  
  vector[numbers] <- str_extract(vector[numbers], pattern = one_or_more(DGT))
  
  vector[NAs] <- NA
  
  vector <- as.numeric(vector)
  
  classifications_2020[, i] <- vector
}

index <- str_detect(colnames(classifications_2020), pattern = DOT %R% DOT %R% DOT) %>%
  which()

names <- colnames(classifications_2020)[-index] %>% head(n = -1)

for(i in seq_along(names)) {
  number <- which(colnames(classifications_2020) == names[i])
  colnames(classifications_2020)[number + 1] <- paste0(names[i], "_1")
  colnames(classifications_2020)[number + 2] <- paste0(names[i], "_2")
}

colnames(classifications_2020)[1] <- "date"

types$place <- colnames(classifications_2020)[-c(1, 80)]

classifications_2020 <- pivot_longer(classifications_2020, cols = 2:79, names_to = "place", values_to = "traffic")

classifications_2020 <- left_join(classifications_2020, types, by = "place")

classifications_2020$place <- str_remove_all(classifications_2020$place, pattern = "_" %R% DGT)

classifications_2020 <- separate(classifications_2020, col = place, into = c("town", "description"), sep = "\\(")

classifications_2020$description <- str_remove_all(classifications_2020$description, pattern = "\\)")

#-------- cleaning other_class_data --------
types <- tibble(type = unlist(other_class_data[1, -1]),
                place = colnames(other_class_data)[-1])

other_class_data <- other_class_data[-c(1, 9, 10, 11, 19), ] 

other_class_data$day <- str_extract_all(other_class_data$...1,
                                            pattern = START %R% 
                                              one_or_more(WRD) %R% 
                                              SPACE) %>% 
  unlist() %>% 
  str_remove(pattern = " ")

other_class_data$...1 <- str_remove_all(other_class_data$...1, 
                                            pattern = START %R% 
                                              one_or_more(WRD) %R% 
                                              SPACE)

other_class_data$...1 <- dmy(other_class_data$...1)

for(i in 2:55){
  vector <- other_class_data[,i] %>%
    as.character() %>%
    str_split(pattern = ",") %>%
    unlist()
  
  numbers <- vector %>%
    str_detect(pattern = one_or_more(DGT)) %>%
    which()
  
  NAs <- vector %>%
    str_detect(pattern = one_or_more("N")) %>%
    which()
  
  vector[numbers] <- str_extract(vector[numbers], pattern = one_or_more(DGT))
  
  vector[NAs] <- NA
  
  vector <- as.numeric(vector)
  
  other_class_data[, i] <- vector
}

index <- str_detect(colnames(other_class_data), pattern = DOT %R% DOT %R% DOT) %>%
  which()

names <- colnames(other_class_data)[-index] %>% head(n = -1)

for(i in seq_along(names)) {
  number <- which(colnames(other_class_data) == names[i])
  colnames(other_class_data)[number + 1] <- paste0(names[i], "_1")
  colnames(other_class_data)[number + 2] <- paste0(names[i], "_2")
}

colnames(other_class_data)[1] <- "date"

types$place <- colnames(other_class_data)[-c(1, 56)]

other_class_data <- pivot_longer(other_class_data, cols = 2:55, names_to = "place", values_to = "traffic")

other_class_data <- left_join(other_class_data, types, by = "place")

other_class_data$place <- str_remove_all(other_class_data$place, pattern = "_" %R% DGT)

other_class_data <- separate(other_class_data, col = place, into = c("town", "description"), sep = "\\(")

other_class_data$description <- str_remove_all(other_class_data$description, pattern = "\\)")

#-------- cleaning class data --------
class <- class %>%
  filter(!is.na(Vehicle)) %>%
  pivot_longer(cols = 2:159, names_to = "date", values_to = "transactions")

class <- class %>%
  mutate(date = mdy(date))

#-------- cleaning interchange data --------
interchange$X7 <- str_extract_all(interchange$plaza, 
                                  pattern = START %R% one_or_more(DGT)) %>% 
  unlist() %>% as.numeric()

interchange$plaza <- str_remove_all(interchange$plaza, 
                                    pattern = START %R% one_or_more(DGT) %R% SPACE)

interchange <- interchange %>%
  select(year, code = X7, plaza, revenue, transactions, 
         average_fare, average_daily_transactions = average_daily_trans)

#-------- cleaning comparison data --------
comparison_1 <- comparison_1 %>%
  select(date, revenue, type, subtype) %>%
  filter(!is.na(date)) %>%
  mutate(date = mdy(date))

comparison_2 <- comparison_2 %>%
  select(date, transactions, vehicle) %>%
  filter(!is.na(date)) %>%
  mutate(date = mdy(date))

#-------- cleaning tax revenue data --------
tax_revenues <- tax_revenues %>%
  pivot_longer(cols = 6:17, names_to = "month", values_to = "revenue") %>%
  mutate(year = fiscal_year)

change <- which(tax_revenues$month %in% c("July", "August", 
                                           "September", "October", 
                                           "November", "December"))

tax_revenues$year[change] <- tax_revenues$year[change] - 1

tax_revenues <- tax_revenues %>%
  mutate(date = ymd(paste0(year, "-", month, "-", "01")),
         quarter = recode(month, "July" = "Quarter 1",
                                 "August" = "Quarter 1",
                                 "September" = "Quarter 1",
                                 "October" = "Quarter 2",
                                 "November" = "Quarter 2",
                                 "December" = "Quarter 2",
                                 "January" = "Quarter 3",
                                 "February" = "Quarter 3",
                                 "March" = "Quarter 3",
                                 "April" = "Quarter 4",
                                 "May" = "Quarter 4",
                                 "June" = "Quarter 4")) %>%
  select(date, fiscal_year, quarter, month, fund, category, code, tax, revenue)

#-------- cleaning unemployment claims data --------
unemployment_claims <- unemployment_claims %>%
  mutate(date = mdy(`Filed week ended`),
         month = month(date),
         year = year(date))%>%
  group_by(year, month) %>%
  summarise(claims = sum(`Initial Claims`)) %>%
  ungroup() %>%
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  select(date, claims)

#-------- cleaning unemployment data --------
unemployment <- unemployment %>%
  pivot_longer(cols = 3:14, names_to = "month", values_to = "value") %>% 
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  filter(!is.na(value)) %>%
  select(date, category, value)
  

#-------- cleaning inflation data --------
monthly_inflation <- monthly_inflation %>%
  filter(!is.na(date)) %>%
  select(date, inflation) %>%
  mutate(date = mdy(date))

#-------- merging data --------
traffic_summary <- bind_rows(tab_2019, tab_2020)

traffic_breakdown <- bind_rows(classifications_2019, 
                                    classifications_2020,
                                    other_class_data)

rm(classifications_2019, classifications_2020, other_class_data, dates,
   types, tab_2019, tab_2020, days_long, days_short, i, fix, index, names,
   NAs, number, numbers, vector, change)

#-------- saving data --------
save(traffic_summary, file = "traffic_summary.RData")
save(traffic_breakdown, file = "traffic_breakdown.RData")
save(class, file = "class.RData")
save(interchange, file = "interchange.RData")
save(comparison_1, file = "comparison1.RData")
save(comparison_2, file = "comparison2.RData")
save(tax_revenues, file = "tax_revenues.RData")
save(unemployment_claims, file = "unemployment_claims.RData")
save(monthly_inflation, file = "monthly_inflation.RData")
save(unemployment, file = "unemployment.RData")
save(fuel_efficiency, file = "fuel_efficiency.RData")

write_csv(traffic_summary, "traffic_summary.csv")
write_csv(traffic_breakdown, "traffic_breakdown.csv")
write_csv(class, "class.csv")
write_csv(interchange, "interchange.csv")
write_csv(comparison_1, "comparison1.csv")
write_csv(comparison_2, "comparison2.csv")

