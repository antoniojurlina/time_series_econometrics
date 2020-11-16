#-------- packages --------
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(ggthemes)
library(scales)
library(xts)
library(astsa)
library(forecast)
library(tseries)
library(broom)
library(zeallot)

#-------- data and directories --------
directory <- paste0(here::here(), "/data")
setwd(directory)

files <- list.files()[str_detect(list.files(), pattern = ".RData") %>% which()]
data <- map(files, function(x) get(load(x)))
file_names <- str_remove_all(files, pattern = ".RData")
names(data) <- file_names

directory <- paste0(here::here(), "/output")
setwd(directory)

options(scipen = "100")

# adjusting tax revenue data for inflation so that Jan/2018 = 100
data$tax_revenues <- data$tax_revenues %>% 
  left_join(data$monthly_inflation, by = "date") %>%
  mutate(revenue = revenue + revenue*inflation) %>%
  select(-inflation)

rm(files, file_names)

#-------- functions --------
linear_reg <- function(data, formula){
  lm(formula, data)
}

#-------- formatted and cleaned data --------
gas_tax_data <- data$tax_revenues %>%
  filter(revenue != 0, !is.na(revenue)) %>%
  filter(code == "0321") %>%
  filter(date <= "2020-03-01", date >= "2007-01-01") %>%
  group_by(date) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE),
            fiscal_year = fiscal_year[1],
            quarter = quarter[1]) %>%
  ungroup() %>%
  mutate(revenue = ifelse(revenue > 24000000, NA, revenue),
         revenue = ifelse(revenue < 12000000, NA, revenue),
         revenue = ifelse(is.na(revenue), mean(revenue, na.rm = T), revenue))

traffic_data <- data$comparison2 %>%
  group_by(date) %>%
  summarise(traffic = sum(transactions, na.rm = TRUE)) %>%
  ungroup()

traffic_change_data <- data$traffic_summary %>%
  filter(!is.na(county)) %>%
  group_by(county, date) %>%
  summarize(traffic = sum(traffic, na.rm = T)) %>%
  ungroup() %>%
  mutate(year = year(date),
         day = ymd(paste0(2000, "-", month(date), "-", day(date)))) %>%
  select(-date) %>%
  pivot_wider(names_from = "year", values_from = "traffic") %>% 
  filter(!is.na(`2019`), !is.na(`2020`)) %>% 
  mutate(change = 100*(`2020`-`2019`)/`2019`) %>%
  filter(change < 0) %>%
  group_by(county) %>%
  summarize(min = max(change),
            average = mean(change),
            max = min(change)) %>%
  pivot_longer(cols = 2:4, names_to = "statistic", values_to = "value")

ols <- gas_tax_data %>%
  left_join(traffic_data,
            by = "date") %>%
  add_row(date = ymd(c("2020-04-01", "2020-05-01", 
                       "2020-06-01", "2020-07-01", 
                       "2020-08-01", "2020-09-01"))) %>%
  mutate(year = year(date)) %>%
  left_join(data$fuel_efficiency,
            by = "year") %>%
  select(-year) %>%
  mutate(traffic = lag(traffic, 1))


#-------- change in traffic volume --------
traffic_change_data %>%
  group_by(statistic) %>%
  summarize(average_change = round(mean(value),0)) %>%
  ggplot(aes(x = reorder(statistic, average_change), 
             y = average_change, group = statistic, 
             colour = statistic, fill = statistic)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(average_change, "%")), colour = "black", 
            nudge_y = -1) +
  theme_wsj() +
  scale_colour_wsj() +
  scale_fill_wsj() +
  labs(title = "Average % Change in Traffic Volume")

traffic_change_data %>% 
  ggplot(aes(x = county, y = value, group = statistic, 
             colour = statistic, fill = statistic)) +
  geom_col(position = "dodge") +
  theme_wsj() +
  scale_colour_wsj() +
  scale_fill_wsj() +
  labs(title = "% Change in County Traffic Volume")

#-------- time series and expectations --------
time_series <- list()
expectations <- list()

# gas tax
time_series$gas <- xts(gas_tax_data$revenue, order.by = gas_tax_data$date)

expectations$gas <- sarima.for(time_series$gas, n.ahead = 6, 2, 1, 1, 1, 1, 0, 12)
expectations$gas <- tibble(expectations$gas$pred, 
                           expectations$gas$se,
                           ymd(c("2020-04-01", "2020-05-01", "2020-06-01", 
                                 "2020-07-01", "2020-08-01", "2020-09-01")))
colnames(expectations$gas) <- c("expected_gas", "error_gas", "date")

# traffic
time_series$traffic <- xts(traffic_data$traffic, order.by = traffic_data$date)

expectations$traffic <- sarima.for(time_series$traffic, n.ahead = 7, 3, 1, 2, 1, 1, 1, 12)
expectations$traffic <- tibble(expectations$traffic$pred, 
                               expectations$traffic$se,
                               ymd(c("2020-03-01",
                                     "2020-04-01", "2020-05-01", "2020-06-01", 
                                     "2020-07-01", "2020-08-01", "2020-09-01")),
                               expectations$traffic$pred - (expectations$traffic$pred * 0.69),
                               expectations$traffic$pred - (expectations$traffic$pred * 0.35),
                               expectations$traffic$pred - (expectations$traffic$pred * 0.02))
colnames(expectations$traffic) <- c("expected_traffic", "error_traffic", "date", "max_traffic", "average_traffic", "min_traffic")

#-------- predictions --------
ols %>%
  linear_reg(log(revenue) ~ log(traffic) + log(fuel_efficiency)) %>%
  tidy()

c(reg_output, reg_statistic, reg_residuals) %<-% list(
  ols %>%
    linear_reg(revenue ~ traffic + fuel_efficiency) %>%
    tidy(),
  ols %>%
    linear_reg(revenue ~ traffic + fuel_efficiency) %>%
    glance(),
  ols %>%
    linear_reg(revenue ~ traffic + fuel_efficiency) %>%
    augment()
  )

prediction <- ols %>%
  left_join(expectations$traffic,
            by = "date") %>%
  mutate(traffic_expected.lower = expected_traffic - error_traffic,
         traffic_expected.upper = expected_traffic + error_traffic) %>%
  select(-error_traffic) %>% 
  rename(revenue.actual = revenue,
         traffic.actual = traffic,
         traffic_expected.actual = expected_traffic,
         traffic_expected.max = max_traffic,
         traffic_expected.average = average_traffic,
         traffic_expected.min = min_traffic) %>%
  left_join(expectations$gas %>%
              mutate(revenue_expected.lower = expected_gas - error_gas,
                     revenue_expected.upper = expected_gas + error_gas) %>%
              rename(revenue_expected.actual = expected_gas) %>%
              select(-error_gas), by = "date") %>%
  mutate(prediction_max.actual = ifelse(date >= 2020-03-01, 
                                    pull(reg_output[1, 2]) + 
                                      pull(reg_output[2, 2])*traffic_expected.max +
                                      pull(reg_output[3, 2])*fuel_efficiency, NA),
         prediction_max.upper = ifelse(date >= 2020-03-01, 
                                          (pull(reg_output[1, 2])+pull(reg_output[1, 3]))+ 
                                            (pull(reg_output[2, 2])+pull(reg_output[2, 3]))*traffic_expected.max +
                                            (pull(reg_output[3, 2])+pull(reg_output[3, 3]))*fuel_efficiency, NA),
         prediction_max.lower = ifelse(date >= 2020-03-01, 
                                          (pull(reg_output[1, 2])-pull(reg_output[1, 3]))+ 
                                            (pull(reg_output[2, 2])-pull(reg_output[2, 3]))*traffic_expected.max +
                                            (pull(reg_output[3, 2])-pull(reg_output[3, 3]))*fuel_efficiency, NA),
         prediction_average.actual = ifelse(date >= 2020-03-01, 
                                 pull(reg_output[1, 2]) + 
                                   pull(reg_output[2, 2])*traffic_expected.average +
                                   pull(reg_output[3, 2])*fuel_efficiency, NA),
         prediction_average.upper = ifelse(date >= 2020-03-01, 
                                       (pull(reg_output[1, 2])+pull(reg_output[1, 3]))+ 
                                         (pull(reg_output[2, 2])+pull(reg_output[2, 3]))*traffic_expected.average +
                                         (pull(reg_output[3, 2])+pull(reg_output[3, 3]))*fuel_efficiency, NA),
         prediction_average.lower = ifelse(date >= 2020-03-01, 
                                        (pull(reg_output[1, 2])-pull(reg_output[1, 3]))+ 
                                          (pull(reg_output[2, 2])-pull(reg_output[2, 3]))*traffic_expected.average +
                                          (pull(reg_output[3, 2])-pull(reg_output[3, 3]))*fuel_efficiency, NA),
         prediction_min.actual = ifelse(date >= 2020-03-01, 
                                     pull(reg_output[1, 2]) + 
                                       pull(reg_output[2, 2])*traffic_expected.min +
                                       pull(reg_output[3, 2])*fuel_efficiency, NA),
         prediction_min.upper = ifelse(date >= 2020-03-01, 
                                           (pull(reg_output[1, 2])+pull(reg_output[1, 3]))+ 
                                             (pull(reg_output[2, 2])+pull(reg_output[2, 3]))*traffic_expected.min +
                                             (pull(reg_output[3, 2])+pull(reg_output[3, 3]))*fuel_efficiency, NA),
         prediction_min.lower = ifelse(date >= 2020-03-01, 
                                           (pull(reg_output[1, 2])-pull(reg_output[1, 3]))+ 
                                             (pull(reg_output[2, 2])-pull(reg_output[2, 3]))*traffic_expected.min +
                                             (pull(reg_output[3, 2])-pull(reg_output[3, 3]))*fuel_efficiency, NA)) %>% 
  pivot_longer(cols = c(2, 5, 7:24), names_to = "statistic", values_to = "value") %>% 
  separate(statistic, into = c("parameter", "statistic"), sep = "\\.") %>%
  pivot_wider(names_from = "statistic", values_from = "value")


prediction %>%
  filter(date > "2019-06-01") %>% 
  filter(parameter != "traffic", parameter != "traffic_expected", parameter != "prediction_min") %>%
  mutate(parameter = recode(parameter, 
                            "prediction_average" = "revenue with 35% traffic decrease",
                            "prediction_max" = "revenue with 69% traffic decrease",
                            "revenue" = "gasoline tax revenue",
                            "revenue_expected" = "expected revenue w/o COVID-19")) %>%
  ggplot(aes(x = date, colour = parameter, fill = parameter)) +
  geom_point(aes(y = actual), show.legend = F) +
  geom_line(aes(y = actual)) +
  geom_point(aes(y = max), show.legend = F) +
  geom_line(aes(y = max)) +
  geom_point(aes(y = min), show.legend = F) +
  geom_line(aes(y = min)) +
  geom_point(aes(y = average), show.legend = F) +
  geom_line(aes(y = average)) +
  scale_y_continuous(label = dollar) +
  scale_color_wsj() +
  theme_wsj() +
  theme(axis.title.x = element_blank()) +
  labs(title = "Gasoline Tax Revenue Predictions",
       colour = "legend")


prediction_data <- ols %>%
  left_join(expectations$traffic,
            by = "date") %>%
  select(-error_traffic) %>% 
  rename(revenue.actual = revenue,
         traffic.actual = traffic,
         traffic_expected.actual = expected_traffic,
         traffic_expected.max = max_traffic,
         traffic_expected.average = average_traffic,
         traffic_expected.min = min_traffic) %>%
  left_join(expectations$gas %>%
              rename(revenue_expected.actual = expected_gas) %>%
              select(-error_gas), by = "date") %>%
  mutate(revenue_expected.max = ifelse(date >= 2020-03-01, 
                                        pull(reg_output[1, 2]) + 
                                          pull(reg_output[2, 2])*traffic_expected.max +
                                          pull(reg_output[3, 2])*fuel_efficiency, NA),
         revenue_expected.average = ifelse(date >= 2020-03-01, 
                                            pull(reg_output[1, 2]) + 
                                              pull(reg_output[2, 2])*traffic_expected.average +
                                              pull(reg_output[3, 2])*fuel_efficiency, NA),
         revenue_expected.min = ifelse(date >= 2020-03-01, 
                                        pull(reg_output[1, 2]) + 
                                          pull(reg_output[2, 2])*traffic_expected.min +
                                          pull(reg_output[3, 2])*fuel_efficiency, NA)) %>% 
  pivot_longer(cols = c(2, 5, 7:14), names_to = "statistic", values_to = "value") %>% 
  separate(statistic, into = c("parameter", "statistic"), sep = "\\.") %>%
  pivot_wider(names_from = "statistic", values_from = "value")

prediction_data$fiscal_year[which(prediction_data$date %in% ymd(c("2020-04-01", "2020-05-01", "2020-06-01")))] <- 2020
prediction_data$quarter[which(prediction_data$date %in% ymd(c("2020-04-01", "2020-05-01", "2020-06-01")))] <- "Quarter 4"
prediction_data$fiscal_year[which(prediction_data$date %in% ymd(c("2020-07-01", "2020-08-01", "2020-09-01")))] <- 2021
prediction_data$quarter[which(prediction_data$date %in% ymd(c("2020-07-01", "2020-08-01", "2020-09-01")))] <- "Quarter 1"

prediction_data <- prediction_data %>%
  filter(date > "2020-03-01") %>%
  filter(parameter != "revenue", parameter != "traffic") %>%
  select(-min)

prediction_data %>%
  pivot_longer(cols = 6:8, names_to = "statistic", values_to = "value") %>%
  mutate(parameter = recode(parameter, "revenue_expected" = "Gasoline Tax Revenue ($)",
                                       "traffic_expected" = "Traffic"),
         statistic = recode(statistic, "average" = "35% decrease in traffic",
                                       "max" = "69% decrease in traffic",
                                       "actual" = "w/o COVID-19")) %>% 
  ggplot(aes(x = date, y = value, colour = statistic, fill = statistic)) +
  geom_col(position = "dodge") +
  facet_wrap(. ~ parameter, ncol = 1, scales = "free") +
  scale_y_continuous(labels = comma) +
  theme_wsj() +
  scale_color_wsj() + 
  scale_fill_wsj() +
  labs(title = "Expected Lockdown Effects")

prediction_data %>%
  mutate(average = actual - average,
       max = actual - max) %>%
  select(-actual) %>%
  pivot_longer(cols = 6:7, names_to = "statistic", values_to = "value") %>%
  mutate(parameter = recode(parameter, "revenue_expected" = "Gasoline Tax Revenue Losses ($)",
                            "traffic_expected" = "Traffic Decrease"),
         statistic = recode(statistic, "average" = "Traffic reduced by 35%",
                            "max" = "Traffic reduced by 69%")) %>%
  ggplot(aes(x = date, y = value, colour = statistic, fill = statistic)) +
  geom_col(position = "dodge") +
  facet_wrap(. ~ parameter, ncol = 1, scales = "free") +
  scale_y_continuous(labels = comma) +
  theme_wsj() +
  scale_color_wsj() + 
  scale_fill_wsj() +
  labs(title = "Expected Lockdown Losses")

#-------- other --------
ggplot(data$fuel_efficiency, aes(x = year, y = fuel_efficiency)) +
  geom_line() +
  geom_point() +
  geom_label_repel(aes(label = fuel_efficiency)) +
  theme_wsj() +
  labs(title = "Average U.S. Fuel Efficiency")

data$monthly_inflation %>%
  ggplot(aes(x = date, y = inflation)) + 
  geom_line() +
  geom_vline(aes(xintercept = ymd("2018-01-01")), colour = "#BB5566") +
  annotate("text", x = ymd("2018-01-01"), y = 0.12, label = "Jan 2018", color = "#BB5566", size = 7) +
  theme_wsj() +
  labs(title = "Monthly Inflation Rate (Jan 2018 = 100)")






