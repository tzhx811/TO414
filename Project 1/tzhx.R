library(tidyverse)
library(dplyr)

hotel_data = read.csv("hotel_bookings.csv")
hotel_data$month_num = match(hotel_data$arrival_date_month, month.name)
hotel_data$company <- as.factor(hotel_data$company)

hotel_data$agent <- as.factor(hotel_data$agent)
#look at frequency of top agents
group_by(hotel_data, agent) %>% filter((n() >= 2000) & (agent != "NULL"))  %>% ggplot(aes(x=agent)) + geom_bar()
#find which ones generate the most revenue
agent_revenue = c()
agent_total = c()
agent_cancel = c()
agents <- levels(hotel_data$agent)
for (a in agents){
  if(a!="NULL"){
    num_count = NROW(hotel_data$agent[hotel_data$agent==a])
    agent_revenue <- c(agent_revenue,sum(hotel_data$adr[hotel_data$agent == a & hotel_data$is_canceled==0] * 
              (hotel_data$stays_in_weekend_nights[hotel_data$agent == a & hotel_data$is_canceled==0] +
                 hotel_data$stays_in_week_nights[hotel_data$agent == a & hotel_data$is_canceled==0])/ 
                num_count))
    agent_total <- c(agent_total,sum(hotel_data$adr[hotel_data$agent == a & hotel_data$is_canceled==0]  * 
                                           (hotel_data$stays_in_weekend_nights[hotel_data$agent == a & hotel_data$is_canceled==0] +
                                              hotel_data$stays_in_week_nights[hotel_data$agent == a & hotel_data$is_canceled==0])))
    agent_cancel <- c(agent_cancel, NROW(hotel_data$agent[hotel_data$agent == a & hotel_data$is_canceled==1])/num_count)
  }
}
agent_table = data.frame(agents = agents[1:length(agent_revenue)], agent_revenue = agent_revenue, agent_total = agent_total, agent_cancel =agent_cancel)

top_n(agent_table, n=5, agent_revenue) %>% ggplot(., aes(x=agents, y=agent_revenue)) + geom_bar(stat='identity', fill="steelblue")
top_n(agent_table, n=5, agent_total) %>% ggplot(., aes(x=agents, y=agent_total)) + geom_bar(stat='identity' ,fill="maroon")
top_n(agent_table, n=5, agent_cancel) %>% ggplot(., aes(x=agents, y=agent_cancel)) + geom_bar(stat='identity' ,fill="red")
top_n(agent_table, n=-5, agent_cancel) %>% ggplot(., aes(x=agents, y=agent_cancel)) + geom_bar(stat='identity' ,fill="red")



#look at frequency of top company
group_by(hotel_data, company) %>% filter((n() >= 200) & (company != "NULL"))  %>% ggplot(aes(x=company)) + geom_bar()
#find which ones generate the most revenue
company_revenue = c()
company_total = c()
company_cancel = c()
companies <- levels(hotel_data$company)
for (a in companies){
  if(a!="NULL"){
    num_count = NROW(hotel_data$company[hotel_data$company==a])
    company_revenue <- c(agent_revenue,sum(hotel_data$adr[hotel_data$company == a & hotel_data$is_canceled==0] * 
                                           (hotel_data$stays_in_weekend_nights[hotel_data$company == a & hotel_data$is_canceled==0] +
                                              hotel_data$stays_in_week_nights[hotel_data$company == a & hotel_data$is_canceled==0])/ 
                                           num_count))
    company_total <- c(agent_total,sum(hotel_data$adr[hotel_data$company == a & hotel_data$is_canceled==0]  * 
                                       (hotel_data$stays_in_weekend_nights[hotel_data$company == a & hotel_data$is_canceled==0] +
                                          hotel_data$stays_in_week_nights[hotel_data$company == a & hotel_data$is_canceled==0])))
    company_cancel <- c(agent_cancel, NROW(hotel_data$company[hotel_data$company == a & hotel_data$is_canceled==1])/num_count)
  }
}
company_table = data.frame(company = companies[1:length(company_revenue)], company_revenue = company_revenue, company_total = company_total, company_cancel = company_cancel)

top_n(company_table, n=5, company_revenue) %>% ggplot(., aes(x=company, y=company_revenue)) + geom_bar(stat='identity', fill="steelblue")
top_n(company_table, n=5, company_total) %>% ggplot(., aes(x=company, y=company_total)) + geom_bar(stat='identity' ,fill="maroon")
top_n(company_table, n=5, company_cancel) %>% ggplot(., aes(x=company, y=company_cancel)) + geom_bar(stat='identity' ,fill="red")
top_n(company_table, n=-5, company_cancel) %>% ggplot(., aes(x=company, y=company_cancel)) + geom_bar(stat='identity' ,fill="red")

############
hotel_data$country <- as.character(hotel_data$country)
countryorigin <- hotel_data[,c(1,14)]
##Transform 3 digit codes into country names
library(countrycode)
countryorigin$country <- countrycode(countryorigin$country, "iso3c", "country.name", nomatch = NULL)
countryorigin$country <- as.factor(countryorigin$country)
library("maps")
Country <- map_data("world")
names <- levels(countryorigin$country)
bookings <- c()
for(i in 1:length(names)){
  n = names[i]
  bookings <- c(bookings, NROW(countryorigin$hotel[countryorigin$country==n]))
  if(n=="United States"){
    names[i] = "USA"
  }
  if(n=="United Kingdom"){
    names[i] = "UK"
  }
}
counts <- data.frame(region = names, count = bookings)
Europe <- c()
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus", "Albania", "Kosovo",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia", "Norway",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland","Montenegro",
                   "Portugal","Romania","Slovakia","Slovenia","Spain", "Serbia", "Macedonia",
                   "Sweden","UK", "Switzerland", "Czech Republic", "Bosnia and Herzegovina")
for(c in europeanUnion){
  Europe <- rbind(Europe, Country[Country$region==c,])
}
arrestsbyregion <- merge(Europe, counts, sort = FALSE, by = "region", all.x=TRUE)
#arrestsbyregion <- arrestsbyregion[arrestsbyregion$order<=200, ]
arrestsbyregion <- arrestsbyregion[order(arrestsbyregion$order), ]

# Plot a map, filling in the states based on murder rate
ggplot(data=arrestsbyregion, aes(x=long, y=lat, group=group, fill=count)) + geom_polygon()


