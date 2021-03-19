library(readr)
library(tidyverse)

data <- read_csv("../data/lab_sodra.csv")
summary(data)

#1 uzd
#atfiltruoja duomenu faila
data1 <- data %>%
  filter(ecoActCode == 702200)
#grafikas
data1 %>%
  ggplot(aes(x=avgWage)) +
  theme_minimal() +
  geom_histogram(fill = "red", col = "black", bins = 100) +
  labs(title = "Awerage wage of employees")

#2 uzd
#isskiria menesi is datos
data1 <- data1 %>% mutate(month_value=as.integer(substr(month, 5 ,7)))
#apskaiciuoja vidutini imones metu atlyginima
data2 <- data1 %>%
  group_by(name) %>%
  summarise(avg = mean(avgWage)) %>%
  arrange(desc(avg))
#apjungia abu duomenu failus
data3 <- merge(data1, data2)
#grafikas
data3 %>%
  arrange(desc(avg)) %>%
  head(60) %>%
  ggplot(aes(x = month_value, y = avgWage, group = name)) +
  theme_minimal() +
  geom_point(aes(colour = name)) +
  scale_x_continuous("month",breaks=1:12,limits=c(1,12)) + 
  geom_line(aes(colour = name)) +
  labs(title = "Average wage of employees", x = "Month", y = "Average wage")

#3 uzd
#grafikas
data3 %>%
  arrange(desc(avg)) %>%
  head(60) %>%
  group_by(name) %>%
  slice_max(numInsured, with_ties = FALSE) %>%
  head(5) %>%
  ggplot(aes(x = reorder(name, -numInsured), y = numInsured)) +
  geom_col(aes(fill = name)) +
  theme(axis.text.x = element_blank()) +
  theme_minimal() +
  labs(title = "Number of insured employees", x = "Company", y = "Count")

