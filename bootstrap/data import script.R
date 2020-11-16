library(readr)
library(dplyr)

directory <- paste0(here::here(), "/bootstrap")
setwd(directory)

csv <- "Income_credit_card_data.csv"

income_credit_card_data <- read_csv(csv)

colnames(income_credit_card_data) <- c("age", "income", "incper", "spending")

save(income_credit_card_data, file = "income_credit_card_data.RData")
