library(tidyverse)

# Cleaning Data

wide_dat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")


long_dat <- pivot_longer(wide_dat, cols = 5:52,
             names_to = "Date",
             values_to = "Confirmed_Cases")


dat <- long_dat %>% 
  filter(Confirmed_Cases > 0)


dat <- dat[order(dat$Date),]


# Reading in Benford's Law

benford <- data.frame(FirstDigit = c(1,2,3,4,5,6,7,8,9), perc = c(.301,.176,.125,.097,.079,.067,.058,.051,.046))



# Looking at China Specifically

cn_dat <- dat %>% 
  filter(`Country/Region` == "China")


cn_dat <- cn_dat %>% 
  separate(Confirmed_Cases, into = c("FirstDigit"), sep = 1)


cn_dat_first <- cn_dat %>% 
  group_by(FirstDigit) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))


total <- sum(cn_dat_first$count)

chisq <- sum((cn_dat_first$count - (total * benford$perc))^2 /  (total * benford$perc))

ch <- pchisq(chisq, 8, lower.tail = F)


cn_dat_first %>% 
  ggplot() +
  geom_histogram(aes(x = reorder(as.factor(FirstDigit), FirstDigit), y = perc), stat = "identity", fill = "firebrick", color = "black") +
  geom_point(data = benford, aes(x = reorder(FirstDigit, FirstDigit), y = perc, color = "Expected Value")) +
  geom_text(aes(x = FirstDigit, y = perc/2, label = round(perc, digits = 3)), fontface = "bold") +
  labs(x = "First Digit", y = "Percentage of Total", title = "The Cumulative Daily Reports of the Coronavirus for all of China", subtitle = paste0("P-value: ", round(ch, 2)), color = "  Benford's Law") +
  scale_color_manual(values = "black") +
  theme_minimal() +
  theme(legend.position = c(.6,.73),
        plot.background = element_blank())



# Provinces individually

prov_dat_first <- cn_dat %>% 
  group_by(`Province/State`, FirstDigit) %>%
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))


prov_dat_first %>% 
  ggplot() +
  geom_histogram(aes(x = FirstDigit, y = perc), stat = "identity", fill = "red", color = "black") +
  facet_wrap(~`Province/State`) +
  geom_point(data = benford, aes(x = FirstDigit, y = perc)) + 
  theme_minimal() +
  labs(x = "First Digit", y = "Percentage of Total", title = "Cumulative Daily Reports of the Coronavirus per Province")


prov_dat_first %>% 
  filter(`Province/State` == "Guangdong") %>% 
  pull(count) %>% 
  sum()

test <- prov_dat_first %>% 
  filter(`Province/State` == "Guangdong") %>% 
  pull(count)

benford$perc*48

sum((test - (48*benford$perc))^2/(48*benford$perc))


pchisq(28.10656, 8, lower.tail = F)



# Outside of China - All Countries



ncn_dat <- dat %>%
  filter(`Country/Region` != "China")


ncn_dat <- ncn_dat %>% 
  separate(Confirmed_Cases, into = c("FirstDigit"), sep = 1)

ncn_dat_first <- ncn_dat %>% 
  group_by(FirstDigit) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

total <- sum(ncn_dat_first$count)

chisq <- sum((ncn_dat_first$count - (total * benford$perc))^2 /  (total * benford$perc))

ch <- pchisq(chisq, 8, lower.tail = F)


ncn_dat_first %>% 
  ggplot() +
  geom_histogram(aes(x = reorder(as.factor(FirstDigit), FirstDigit), y = perc), stat = "identity", fill = "firebrick", color = "black") +
  geom_point(data = benford, aes(x = reorder(FirstDigit, FirstDigit), y = perc, color = "Expected Value")) +
  geom_text(aes(x = FirstDigit, y = perc/2, label = round(perc, digits = 3)), fontface = "bold") +
  labs(x = "First Digit", y = "Percentage of Total", title = "The Cumulative Daily Reports of the Coronavirus for all of China", subtitle = paste0("P-value: ", round(ch, 2)), color = "  Benford's Law") +
  scale_color_manual(values = "black") +
  theme_minimal() +
  theme(legend.position = c(.6,.73),
        plot.background = element_blank())



# Outside of China - By Country



ncn_dat_country <- ncn_dat %>% 
  group_by(`Country/Region`, FirstDigit) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))



ncn_dat_country %>% 
  ggplot() +
  geom_histogram(aes(x = FirstDigit, y = perc), stat = "identity", fill = "red", color = "black") +
  facet_wrap(~`Country/Region`) +
  geom_point(data = benford, aes(x = FirstDigit, y = perc)) + 
  theme_minimal() +
  labs(x = "First Digit", y = "Percentage of Total", title = "Cumulative Daily Reports of the Coronavirus per Country")


ncn_dat_country %>%
  filter(`Country/Region` == "US") %>% 
  pull(count) %>% 
  sum()

test <- ncn_dat_country %>%
  filter(`Country/Region` == "US") %>% 
  pull(count)


853*benford$perc


sum((test - (853*benford$perc))^2/(853*benford$perc))


pchisq(509.255, 8, lower.tail = F)




#### Everyone

dat_all <- dat %>% 
  separate(Confirmed_Cases, into = c("FirstDigit"), sep = 1)


dat_all <- dat_all %>% 
  group_by(FirstDigit) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))



total <- sum(dat_all$count)

chisq <- sum((dat_all$count - (total * benford$perc))^2 /  (total * benford$perc))

ch <- pchisq(chisq, 8, lower.tail = F)


dat_all %>% 
  ggplot() +
  geom_histogram(aes(x = reorder(as.factor(FirstDigit), FirstDigit), y = perc), stat = "identity", fill = "firebrick", color = "black") +
  geom_point(data = benford, aes(x = reorder(FirstDigit, FirstDigit), y = perc, color = "Expected Value")) +
  geom_text(aes(x = FirstDigit, y = perc/2, label = round(perc, digits = 3)), fontface = "bold") +
  labs(x = "First Digit", y = "Percentage of Total", title = "The Cumulative Daily Reports of the Coronavirus for the World", subtitle = paste0("P-value: ", round(ch, 2)), color = "  Benford's Law") +
  scale_color_manual(values = "black") +
  theme_minimal() +
  theme(legend.position = c(.6,.73),
        plot.background = element_blank())



























