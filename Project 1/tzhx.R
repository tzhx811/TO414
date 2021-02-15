library(tidyverse)
library(dplyr)

hotel_data = read.csv("hotel_bookings.csv")
hotel_data$month_num = match(hotel_data$arrival_date_month, month.name)
hotel_data$company <- as.factor(hotel_data$company)
summary(hotel_data$company)
num_cancelations_by_year_month = group_by(hotel_data, arrival_date_year, month_num, is_canceled) %>%
  summarize(count = n())

ggplot(num_cancelations_by_year_month, aes(x = month_num, y = count)) +
  geom_bar(aes(fill = as.factor(is_canceled)),stat = "identity") +
  facet_wrap(~arrival_date_year) +
  ggtitle("Number of Cancellations and Reservations in Each Year For Each Month") +
  scale_x_continuous(name="Month", breaks = seq(0,12, 1))
