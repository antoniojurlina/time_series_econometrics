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

#-------- data and directory --------
directory <- paste0(here::here(), "/data")
setwd(directory)

load("traffic_breakdown.RData")
load("traffic_summary.RData")
load("class.RData")
load("comparison1.RData")
load("comparison2.RData")
load("interchange.RData")
load("tax_revenues.RData")
load("unemployment.RData")
load("unemployment_claims.RData")
load("monthly_inflation.RData")

directory <- paste0(here::here(), "/output")
setwd(directory)

options(scipen = "100")

#-------- functions --------
linear_reg <- function(data, formula){
  lm(formula, data)
}

#-------- analysis --------
tax_revenues <- tax_revenues %>% 
  left_join(monthly_inflation, by = "date") %>%
  mutate(revenue = revenue + revenue*inflation)

# gasoline tax
gas <- tax_revenues %>%
  filter(revenue != 0, !is.na(revenue)) %>%
  filter(code == "0321") %>%
  filter(date <= "2020-03-01", date >= "2007-01-01") %>%
  group_by(date) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE)) %>%
  ungroup() 

gas <- gas %>%
  mutate(revenue = ifelse(revenue > 24000000, NA, revenue),
         revenue = ifelse(revenue < 12000000, NA, revenue),
         revenue = ifelse(is.na(revenue), mean(revenue, na.rm = T), revenue))

gas <- xts(gas$revenue, order.by = gas$date)
lgas = log(gas)
dgas <- na.locf(diff(gas), fromLast = T)
ddgas <- na.locf(diff(dgas, lag = 12), fromLast = T)

data <- ddgas
length(data)
adf.test(data)
plot(data)
acf2(data, max.lag = 100, main = '')

sarima(gas, 2, 1, 1, 2, 1, 0, 12)
sarima.for(gas, n.ahead = 12, 2, 1, 1, 2, 1, 0, 12)

auto.arima(ddgas, trace = TRUE, ic = "aic", seasonal = TRUE, seasonal.test = "ocsb")

# sales tax
tax_revenues %>%
  filter(revenue != 0, !is.na(revenue)) %>%
  filter(category == "Sales & Use Tax") %>%
  filter(date < "2020-04-01", date >= "2007-01-01") %>%
  group_by(date) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE)) %>%
  ungroup() -> sales

sales <- xts(sales$revenue, order.by = sales$date)
lsales <- na.locf(log(sales))
dsales <- na.locf(diff(sales), fromLast = T)

data <- dsales
length(data)
adf.test(data)
plot(diff(data))
acf2(data, max.lag = 100)

sarima(sales, 6, 1, 1, 1, 0, 0, 12)
sarima.for(sales, n.ahead = 36, 6, 1, 1, 1, 0, 0, 12)$pred %>% unlist() -> pred

auto.arima(sales, trace = TRUE, ic = "bic", seasonal = TRUE, seasonal.test = "ocsb")

# traffic
class %>%
  group_by(date) %>%
  summarise(transactions = sum(transactions, na.rm = TRUE)) %>%
  ungroup() -> traffic

traffic <- xts(traffic$transactions, order.by = traffic$date)
dtraffic <- na.locf(diff(traffic), fromLast = T)
ltraffic <- na.locf(diff(log(traffic)), fromLast = T)

data <- ltraffic
length(data)
adf.test(data)
plot(data)
acf2(data, max.lag = 150)

sarima(log(traffic), 11, 1, 2, 1, 0, 0, 12)
sarima.for(log(traffic), n.ahead = 36, 11, 1, 2, 1, 0, 0, 12)$pred %>% unlist() -> pred

auto.arima(log(traffic), trace = TRUE, ic = "bic", seasonal = TRUE, seasonal.test = "ocsb")

# OLS
ols <- tax_revenues %>%
  filter(code == "0321", fiscal_year %in% c(2007:2019)) %>%
  filter(revenue != 0, !is.na(revenue)) %>%
  filter(date < "2020-04-01") %>% 
  group_by(date) %>%
  mutate(revenue = sum(revenue, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(revenue = ifelse(revenue > 24000000, NA, revenue),
         revenue = ifelse(revenue < 12000000, NA, revenue),
         revenue = ifelse(is.na(revenue), mean(revenue, na.rm = T), revenue)) %>%
  left_join(comparison_2 %>%
              group_by(date) %>%
              summarise(traffic = sum(transactions, na.rm = TRUE)) %>%
              mutate(month = month(date) + 1,
                     year = year(date),
                     year = if_else(month > 12, year + 1, year),
                     month = if_else(month > 12, month - 12, month),
                     date_shifted = ymd(paste0(year, "-", month, "-01"))),
            by = "date")

plot <- ols %>%
  ggplot(aes(x = date, y = revenue, color = quarter)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = traffic)) +
    geom_point(aes(y = traffic)) +
    facet_wrap(. ~ fiscal_year, scales = "free_x", nrow = 1) +
    scale_color_ptol() +
    theme_linedraw() +
    scale_y_continuous(label = comma) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = "none")

ggsave("plot.png", plot, height = 3, width = 7)

ols %>%  
  ggplot(aes(x = log(lag(traffic,1)), y = log(revenue), colour = quarter)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(. ~ quarter, scales = "free") +
  scale_color_ptol() +
  theme_par() +
  xlab("log(traffic)") +
  ylab("log(revenue)") +
  theme(legend.position = "none")

ols %>%
  ggplot(aes(x = traffic, y = revenue)) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_y_continuous(label = comma) +
  scale_x_continuous(label = comma) +
  scale_color_ptol() +
  theme_par() +
  xlab("traffic") +
  ylab("revenue") +
  theme(legend.position = "none")
  
ols_output <- map(c("Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4"), function(x) {
  bind_rows(
    ols %>%
      filter(quarter == x) %>% 
      linear_reg(log(revenue) ~ log(lag(traffic, 1))) %>%
      tidy() %>%
      mutate(type = "log",
             quarter = x),
    ols %>%
      filter(quarter == x) %>% 
      linear_reg(revenue ~ lag(traffic, 1)) %>%
      tidy() %>%
      mutate(type = "normal",
             quarter = x)
  )
}) %>% data.table::rbindlist()

ols_output_log <- ols_output %>%
  filter(type == "log")
ols_output <- ols_output %>%
  filter(type == "normal")

ols_output <- left_join(
  ols_output %>% 
    filter(term == "lag(traffic, 1)") %>%
    rename(slope = estimate,
           slope_se = std.error) %>%
    select(slope, slope_se, quarter),
  ols_output %>%
    filter(term != "lag(traffic, 1)") %>%
    rename(intercept = estimate,
           intercept_se = std.error) %>%
    select(intercept, intercept_se, quarter),
  by = "quarter")

expectation <- sarima.for(gas, n.ahead = 12, 2, 1, 1, 2, 1, 0, 12)
expectation <- tibble(expectation$pred, 
                      expectation$se,
                      tax_revenues %>%
                        filter(fiscal_year == 2020) %>%
                        select(date) %>% unique())
colnames(expectation) <- c("expectation", "error", "date")

prediction <- tax_revenues %>%
  filter(code == "0321") %>%
  filter(revenue != 0, !is.na(revenue)) %>%
  filter(date < "2020-04-01") %>% 
  left_join(comparison_2 %>%
              group_by(date) %>%
              summarise(traffic = sum(transactions, na.rm = TRUE)) %>%
              mutate(month = month(date) + 1,
                     year = year(date),
                     year = if_else(month > 12, year + 1, year),
                     month = if_else(month > 12, month - 12, month),
                     date_shifted = ymd(paste0(year, "-", month, "-01"))),
            by = "date") %>% 
  left_join(ols_output,
            by = "quarter") %>% 
  left_join(expectation,
            by = "date") %>% 
  mutate(traffic = lag(traffic, 1),
         prediction = ifelse(fiscal_year == 2020, intercept + slope*traffic, NA),
         prediction_upper = ifelse(fiscal_year == 2020, (intercept + intercept_se) + (slope + slope_se) * traffic, NA),
         prediction_lower = ifelse(fiscal_year == 2020, (intercept - intercept_se) + (slope - slope_se) * traffic, NA),
         prediction_error = revenue - prediction,
         expectation_upper = expectation + error,
         expectation_lower = expectation - error)

prediction %>%
  filter(fiscal_year == 2020) %>% 
  ggplot(aes(x = date)) +
  geom_point(aes(y = revenue), colour = "#DDAA33") +
  geom_line(aes(y = revenue), colour = "#DDAA33", size = 1.5) +
  geom_point(aes(y = expectation), colour = "#BB5566") +
  geom_line(aes(y = expectation), colour = "#BB5566", size = 1.5) +
  geom_point(aes(y = prediction), colour = "#228833") +
  geom_line(aes(y = prediction), colour = "#228833", size = 1.5) +
  geom_ribbon(aes(ymin = prediction_lower, ymax = prediction_upper), 
              fill = "#228833", colour = "transparent", alpha = 0.1) +
  geom_ribbon(aes(ymin = expectation_lower, ymax = expectation_upper), 
              fill = "#BB5566", colour = "transparent", alpha = 0.3) +
  scale_y_continuous(label = comma) +
  theme_par() +
  theme(axis.title.x = element_blank())

  
prediction %>%
  filter(fiscal_year == 2020) %>%
  group_by(quarter) %>%
    summarize(revenue = sum(revenue, na.rm = T),
              prediction = sum(prediction, na.rm = T),
              prediction_upper = sum(prediction_upper, na.rm = T),
              prediciton_lower = sum(prediction_lower, na.rm = T),
              expectation = sum(expectation, na.rm = T),
              expectation_upper = sum(expectation_upper, na.rm = T),
              expectation_lower = sum(expectation_lower, na.rm = T)) %>%
  View()

  
#-------- plots --------
plot <- data$traffic_summary %>% 
  filter(!is.na(county)) %>%
  mutate(year = year(date),
         day = ymd(paste0(2000, "-", month(date), "-", day(date)))) %>% 
  group_by(year, day, county) %>%
  summarize(traffic = sum(traffic, na.rm = TRUE)) %>% 
  ggplot(aes(x = day, y = traffic, group = year, colour = factor(year))) +
  geom_line() +
  scale_y_continuous(label = comma) +
  facet_wrap(. ~ county, scales = "free") +
  theme_wsj() +
  theme(axis.title.x = element_blank()) +
  scale_colour_wsj() +
  labs(title = "County Traffic", 
       colour = "year")
  
ggsave("county traffic.jpeg", plot, height = 10, width = 18)

tax_revenues %>%
  filter(fiscal_year > 2006) %>%
  filter(category %in% c("Motor Fuel Tax", "Gasoline Tax", "Special Fuel Tax")) %>%
  filter(month %in% c( "January", "February", "March")) %>%
  mutate(year = year(date),
         day = ymd(paste0("2000", "-", month(date), "-", day(date)))) %>% 
  group_by(fiscal_year, category, day) %>%
  summarize(revenue = sum(revenue, na.rm = TRUE)) %>% 
  ggplot(aes(x = day, y = revenue, group = fiscal_year, colour = factor(fiscal_year), label = fiscal_year)) +
  geom_line() +
  geom_text_repel(show.legend = F) +
  scale_y_continuous(label = comma) +
  facet_wrap(. ~ category, scales = "free") +
  labs(title = "Tax Revenue",
       colour = "fiscal year") +
  theme_wsj()

tax_revenues %>%
  filter(date < "2020-04-01") %>%
  filter(!is.na(category)) %>%
  group_by(category, date) %>%
  summarize(revenue = sum(revenue, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = revenue)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  facet_wrap(. ~ category, scales = "free") +
  labs(title = "Tax Revenue ($US)") +
  theme_wsj()

tax_revenues %>%
  filter(date < "2020-04-01") %>%
  filter(!is.na(category)) %>%
  group_by(fund, date) %>%
  summarize(revenue = sum(revenue, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = revenue)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  facet_wrap(. ~ fund, scales = "free", ncol = 1) +
  labs(title = "Tax Revenue ($US)") +
  theme_wsj()

tax_revenues %>%
  filter(date < "2020-04-01") %>%
  group_by(date) %>%
  summarize(revenue = sum(revenue, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = revenue)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  labs(title = "Tax Revenue ($US)") +
  theme_wsj()

turnpike_data %>%
  pivot_longer(cols = 2:3, names_to = "type", values_to = "data") %>%
  ggplot(aes(x = date, y = data, group = type, colour = type)) +
  geom_line() +
  theme_wsj() +
  scale_colour_wsj() +
  scale_y_continuous(labels = comma)
 
turnpike_data %>% 
  filter(revenue != 0, !is.na(revenue)) %>%
  mutate(average_fare = revenue / transactions) %>%
  ggplot(aes(x = date, y = average_fare)) +
  geom_line() +
  theme_wsj() +
  scale_colour_wsj() +
  scale_y_continuous(labels = comma)

tax_revenues %>%
  filter(!is.na(fund)) %>%
  filter(!is.na(category)) %>%
  filter(category != "Gross Receipts Tax", category != "Income & Sales Tax Transfers to Municipal Revenue Sharing") %>%
  group_by(fiscal_year) %>%
  mutate(total_revenue = sum(revenue, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(fiscal_year, fund, category) %>%
  mutate(category_revenue = sum(revenue, na.rm = TRUE)) %>%
  ggplot(aes(x = fiscal_year, y = category_revenue, colour = category, fill = category)) +
  geom_col() +
  scale_color_ptol() +
  scale_fill_ptol() +
  scale_y_continuous(label = comma) +
  theme_wsj() +
  labs(title = "Tax Revenue by Fiscal Year")

tax_revenues %>%
  filter(!is.na(revenue), revenue != 0 ) %>%
  filter(code == "0321", fiscal_year > 2006) %>%
  filter(date < "2020-04-01") %>%
  #filter(category == "Sales & Use Tax", fiscal_year > 2006, fiscal_year < 2020) %>%
  group_by(fiscal_year, quarter) %>%
  mutate(revenue = sum(revenue, na.rm = TRUE)) %>%
  ungroup() %>% 
  ggplot(aes(x = date, y = revenue, colour = quarter)) +
  geom_line(aes(colour = quarter)) +
  geom_point(aes(colour = quarter)) +
  #stat_smooth(method = "lm") +
  scale_color_ptol() +
  theme_wsj() +
  scale_y_continuous(label = comma)

data$unemployment_claims %>%
  ggplot(aes(x = date, y = claims)) +
  geom_line() +
  theme_wsj() +
  scale_y_continuous(label = comma) +
  labs(title = "Monthly Unemployment Claims")

data$unemployment %>% 
  filter(category == "Unemployment Rate") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  theme_wsj() +
  scale_y_continuous(label = comma) +
  labs(title = "Monthly % Unemployment")

data$monthly_inflation %>%
  ggplot(aes(x = date, y = inflation)) + 
  geom_line() +
  geom_vline(aes(xintercept = ymd("2018-01-01")), colour = "#BB5566") +
  annotate("text", x = ymd("2018-01-01"), y = 0.12, label = "Jan 2018", color = "#BB5566", size = 7) +
  theme_wsj() +
  labs(title = "Monthly Inflation Rate (Jan 2018 = 100)")
  




turnpike_data <- left_join(
  comparison_1 %>% 
    group_by(date) %>%
    summarize(revenue = sum(revenue, na.rm = TRUE)),
  comparison_2 %>%
    group_by(date) %>%
    summarize(transactions = sum(transactions, na.rm = TRUE)),
  by = "date"
)

ols <- left_join(
  class %>%
    group_by(date) %>%
    summarize(transactions = sum(transactions, na.rm = TRUE)),
  tax_revenues %>%
    filter(category %in% c("Gasoline Tax")) %>%
    group_by(date, category) %>%
    summarize(tax_revenue = sum(revenue, na.rm = TRUE)),
  by = "date"
) 

ols %>%
  group_by(year(date)) %>%
  summarize(transactions = sum(transactions, na.rm = T),
            tax_revenue = sum(tax_revenue, na.rm = T)) %>%
  rename(date = `year(date)`) %>%
  mutate(transactions = 100 * transactions / first(transactions),
         tax_revenue =  100 * tax_revenue  / first(tax_revenue)) %>%
  pivot_longer(cols = 2:3, names_to = "legend", values_to = "data") %>%
  ggplot(aes(x = date, y = data, group = legend, colour = legend)) +
  geom_line() +
  theme_wsj() +
  scale_colour_wsj() +
  scale_y_continuous(labels = comma) 


ols %>%
  group_by(year(date)) %>%
  summarize(transactions = sum(transactions, na.rm = T),
            tax_revenue = sum(tax_revenue, na.rm = T)) %>%
  rename(date = `year(date)`) %>%
  mutate(transactions = 100 * transactions / first(transactions),
         tax_revenue =  100 * tax_revenue  / first(tax_revenue)) %>%
  ggplot(aes(x = transactions, y = tax_revenue)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, alpha = 0.3) +
  theme_wsj() +
  scale_colour_wsj() +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) 


ols %>%
  group_by(year(date)) %>%
  summarize(transactions = sum(transactions, na.rm = T),
            tax_revenue = sum(tax_revenue, na.rm = T)) %>%
  rename(date = `year(date)`) %>%
  mutate(transactions = 100 * transactions / first(transactions),
         tax_revenue =  100 * tax_revenue  / first(tax_revenue)) %>%
  linear_reg(log(tax_revenue) ~ log(transactions))

ols <- tax_revenues %>%
  filter(date >= "2007-01-01", date < "2020-01-01") %>%
  group_by(fiscal_year) %>%
  mutate(total_tax_revenue = sum(revenue, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(category %in% c("Gasoline Tax", "Sales & Use Tax", 
                         "Individual Income Tax" , "Other Tax", 
                         "Business & Special Industry Tax", "Corporate Income Tax")) %>% 
  group_by(date, category) %>%
  summarize(tax_revenue = sum(revenue, na.rm = TRUE),
            total_tax_revenue = mean(total_tax_revenue, na.rm = TRUE)) %>%
  pivot_wider(names_from = "category", values_from = "tax_revenue") %>% 
  left_join(unemployment, by = "date")

ols %>%
  linear_reg(log(total_tax_revenue) ~ log(`Gasoline Tax`) + 
               log(`Individual Income Tax`) + 
               log(`Other Tax`) + 
               log(`Sales & Use Tax`) + 
               log(`Business & Special Industry Tax`) +
               log(`Corporate Income Tax`) +
               log(claims)) %>%
  tidy() %>%
  View()


tax_revenues %>% 
  group_by(fiscal_year) %>%
  mutate(year_revenue = sum(revenue, na.rm = T)) %>%
  ungroup() %>%
  group_by(fiscal_year, fund) %>%
  mutate(fund_revenue = sum(revenue, na.rm = T)) %>%
  ungroup() %>%
  group_by(fiscal_year, category) %>%
  mutate(category_revenue = sum(revenue, na.rm = T)) %>%
  ungroup() %>%
  group_by(fiscal_year) %>%
  mutate(percent_fund = 100* category_revenue / fund_revenue) %>%
  ungroup() %>%
  group_by(fiscal_year) %>%
  mutate(percent_category = 100 * category_revenue / year_revenue) %>%
  filter(category == "Gasoline Tax") %>%
  View()
  
  




  

