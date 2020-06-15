library(tidyverse)

# # Cleaning Data
# 
# wide_dat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
# 
# 
# long_dat <- pivot_longer(wide_dat, cols = 5:73,
#              names_to = "Date",
#              values_to = "Confirmed_Cases")
# 
# 
# dat <- long_dat %>% 
#   filter(Confirmed_Cases > 0)
# 
# 
# dat <- dat[order(dat$Date),]
# 
# 
# # # Reading in Benford's Law
# # 
# # benford <- data.frame(FirstDigit = c(1,2,3,4,5,6,7,8,9), perc = c(.301,.176,.125,.097,.079,.067,.058,.051,.046))
# # 
# # 
# # 
# # # Looking at China Specifically
# # 
# # cn_dat <- dat %>% 
# #   filter(`Country/Region` == "China")
# # 
# # 
# # cn_dat <- cn_dat %>% 
# #   separate(Confirmed_Cases, into = c("FirstDigit"), sep = 1)
# # 
# # 
# # cn_dat_first <- cn_dat %>% 
# #   group_by(FirstDigit) %>% 
# #   summarise(count = n()) %>% 
# #   mutate(perc = count/sum(count))
# # 
# # 
# # ch <- chisq.test(cn_dat_first$count, p = benford$perc)
# # 
# # ch <- ch$p.value
# # 
# # cn_dat_first %>% 
# #   ggplot() +
# #   geom_histogram(aes(x = reorder(as.factor(FirstDigit), FirstDigit), y = perc), stat = "identity", fill = "firebrick", color = "black") +
# #   geom_point(data = benford, aes(x = reorder(FirstDigit, FirstDigit), y = perc, color = "Expected Value")) +
# #   geom_text(aes(x = FirstDigit, y = perc/2, label = round(perc, digits = 3)), fontface = "bold") +
# #   labs(x = "First Digit", y = "Percentage of Total", title = "The Cumulative Daily Reports of the Coronavirus for all of China", subtitle = paste0("P-value: ", ch), color = "  Benford's Law") +
# #   scale_color_manual(values = "black") +
# #   theme_minimal() +
# #   theme(legend.position = c(.6,.73),
# #         plot.background = element_blank())
# # 
# # 
# # 
# # # Provinces individually
# # 
# # prov_dat_first <- cn_dat %>% 
# #   group_by(`Province/State`, FirstDigit) %>%
# #   summarise(count = n()) %>% 
# #   mutate(perc = count/sum(count))
# # 
# # 
# # prov_dat_first %>% 
# #   ggplot() +
# #   geom_histogram(aes(x = FirstDigit, y = perc), stat = "identity", fill = "red", color = "black") +
# #   facet_wrap(~`Province/State`) +
# #   geom_point(data = benford, aes(x = FirstDigit, y = perc)) + 
# #   theme_minimal() +
# #   labs(x = "First Digit", y = "Percentage of Total", title = "Cumulative Daily Reports of the Coronavirus per Province")
# # 
# # 
# # prov_dat_first %>% 
# #   filter(`Province/State` == "Jiangxi") %>% 
# #   pull(count) %>% 
# #   sum()
# # 
# # test <- prov_dat_first %>% 
# #   filter(`Province/State` == "Jiangxi") %>% 
# #   pull(count)
# # 
# # benford$perc*69
# # 
# # sum((test - (69*benford$perc))^2/(48*benford$perc))
# # 
# # 
# # pchisq(876.1421, 8)
# # 
# # 
# # 
# # # Outside of China - All Countries
# # 
# # 
# # 
# # ncn_dat <- dat %>%
# #   filter(`Country/Region` != "China")
# # 
# # 
# # ncn_dat <- ncn_dat %>% 
# #   separate(Confirmed_Cases, into = c("FirstDigit"), sep = 1)
# # 
# # ncn_dat_first <- ncn_dat %>% 
# #   group_by(FirstDigit) %>% 
# #   summarise(count = n()) %>% 
# #   mutate(perc = count/sum(count))
# # 
# # total <- sum(ncn_dat_first$count)
# # 
# # chisq <- sum((ncn_dat_first$count - (total * benford$perc))^2 /  (total * benford$perc))
# # 
# # ch <- pchisq(chisq, 8)
# # 
# # 
# # ncn_dat_first %>% 
# #   ggplot() +
# #   geom_histogram(aes(x = reorder(as.factor(FirstDigit), FirstDigit), y = perc), stat = "identity", fill = "firebrick", color = "black") +
# #   geom_point(data = benford, aes(x = reorder(FirstDigit, FirstDigit), y = perc, color = "Expected Value")) +
# #   geom_text(aes(x = FirstDigit, y = perc/2, label = round(perc, digits = 3)), fontface = "bold") +
# #   labs(x = "First Digit", y = "Percentage of Total", title = "The Cumulative Daily Reports of the Coronavirus for all of China", subtitle = paste0("P-value: ", round(ch, 2)), color = "  Benford's Law") +
# #   scale_color_manual(values = "black") +
# #   theme_minimal() +
# #   theme(legend.position = c(.6,.73),
# #         plot.background = element_blank())
# # 
# # 
# # 
# # # Outside of China - By Country
# # 
# # 
# # 
# # ncn_dat_country <- ncn_dat %>% 
# #   group_by(`Country/Region`, FirstDigit) %>% 
# #   summarise(count = n()) %>% 
# #   mutate(perc = count/sum(count))
# # 
# # 
# # 
# # ncn_dat_country %>% 
# #   ggplot() +
# #   geom_histogram(aes(x = FirstDigit, y = perc), stat = "identity", fill = "red", color = "black") +
# #   facet_wrap(~`Country/Region`) +
# #   geom_point(data = benford, aes(x = FirstDigit, y = perc)) + 
# #   theme_minimal() +
# #   labs(x = "First Digit", y = "Percentage of Total", title = "Cumulative Daily Reports of the Coronavirus per Country")
# # 
# # 
# # ncn_dat_country %>%
# #   filter(`Country/Region` == "US") %>% 
# #   pull(count) %>% 
# #   sum()
# # 
# # test <- ncn_dat_country %>%
# #   filter(`Country/Region` == "US") %>% 
# #   pull(count)
# # 
# # 
# # 853*benford$perc
# # 
# # 
# # sum((test - (853*benford$perc))^2/(853*benford$perc))
# # 
# # 
# # pchisq(509.255, 8, lower.tail = F)
# 
# 
# 
# 
# #### Everyone
# 
# dat_all <- dat %>% 
#   separate(Confirmed_Cases, into = c("FirstDigit"), sep = 1)


#write.csv(dat_all, "benford_data.csv", row.names = F)

# dat_all <- dat_all %>% 
#   group_by(FirstDigit) %>% 
#   summarise(count = n()) %>% 
#   mutate(perc = count/sum(count))
# 
# 
# ch <- chisq.test(dat_all$count, p = benford$perc)
# 
# 
# dat_all %>% 
#   ggplot() +
#   geom_histogram(aes(x = reorder(as.factor(FirstDigit), FirstDigit), y = perc), stat = "identity", fill = "firebrick", color = "black") +
#   geom_point(data = benford, aes(x = reorder(FirstDigit, FirstDigit), y = perc, color = "Expected Value")) +
#   geom_text(aes(x = FirstDigit, y = perc/2, label = round(perc, digits = 3)), fontface = "bold") +
#   labs(x = "First Digit", y = "Percentage of Total", title = "The Cumulative Daily Reports of the Coronavirus for the World", subtitle = paste0("P-value: ", ch$p.value), color = "  Benford's Law") +
#   scale_color_manual(values = "black") +
#   theme_minimal() +
#   theme(legend.position = c(.6,.73),
#         plot.background = element_blank())



dat <- read_csv("benford_data.csv")
benford <- data.frame(FirstDigit = c(1,2,3,4,5,6,7,8,9), perc = c(.301,.176,.125,.097,.079,.067,.058,.051,.046))

# What I'm going to do is create every possible sample for 20 different observations then calculate their p-values using the chi-squared test, etc.


my_func <- function(seed) {
  
  
  
  set.seed(seed)
  
  
  test <- sample_n(dat, 5000)
  
  test <- test %>% 
    group_by(FirstDigit) %>% 
    summarise(count = n())
  
  return(test)
  
  
}


#map(1:20, my_func)



pvalues <- function(seed) {
  
  set.seed(seed)
  
  
  test <- sample_n(dat, 5000)
  
  test <- test %>% 
    group_by(FirstDigit) %>% 
    summarise(count = n())
  
  chisq.test(test$count, p = benford$perc)$p.value
  
  
}

#map(1:20, pvalues)










