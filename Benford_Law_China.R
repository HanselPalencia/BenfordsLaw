library(coronavirus)
library(tidyverse)
library(geofacet)
library(nCov2019)
library(stats)
library(circular)
library(readxl)


# This is to load the data in, it seems I have to restore the github on my computer to be able to update the data that is coming i n daily.

#  I will use this script to read in the data and then save that data as an xlsx file to be stored in my personal computer

# remotes::install_github("GuangchuangYu/nCov2019")
#
data <- load_nCov2019(lang = "en")
# 
# cn_test <- data$data
# 
# cn_test <- cn_test %>% 
#   unique()
# 
# 
# cn_test <- cn_test[order(cn_test$province, cn_test$time),]
#
#writexl::write_xlsx(cn_test, "co_dat.xlsx")


### Now we have to improve the actual reported per day data (Not sure why this isn't already a column)

cn_test <- read_xlsx("co_dat.xlsx")

cn_test <- cn_test[order(cn_test$province, cn_test$time),]


# They changed the way that they report the data so now this for loop that I was doing below is no longer necessary... (On a future note a lag function might have been easier but who knows.)

# result <- c()
# prov <- c()
# confirmed <- c()
# 
# for (i in 1:nrow(cn_test)) {
#   
#   prov <- cn_test$province
#   confirmed <- cn_test$cum_confirm
#   
#   
#   if (i == 1) {
#     
#     result[i] <- confirmed[i]
#     
#   }
#   
#   else {
#     
#     if (prov[i] == prov[i-1]) {
#       
#       result[i] <- confirmed[i] - confirmed[i-1]
#       
#     }
#     
#     else {
#       
#       result[i] <- confirmed[i]
#       
#     }
#     
#   }
#   
#   
# }


# result <- data.frame(result)
# 
# cn_test <- cbind(cn_test, result)
# 
# names(cn_test)[7] <- "DailyReported"
# 

 cn_test2 <- cn_test %>% 
   filter(cum_confirm > 0)
 
 cn_test <- cn_test %>% 
   filter(cum_confirm > 0)


cn_ben <- cn_test2 %>% 
  separate(cum_confirm, into = c("FirstDigit"), sep = 1)


# Creating an expected from Benford's Law


benford <- data.frame(FirstDigit = c(1,2,3,4,5,6,7,8,9), perc = c(.301,.176,.125,.097,.079,.067,.058,.051,.046))




# Proof China as a whole is not collaborating to lie?
cn_ben1 <- cn_ben %>% 
  group_by(FirstDigit) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

cn_ben1 %>% 
  ggplot() +
  geom_histogram(aes(x = FirstDigit, y = perc), stat = "identity", fill = "red") +
  geom_point(data = benford, aes(x = FirstDigit, y = perc)) + 
  theme_minimal()


chisq.test(cn_ben1$count, p = benford$perc)



cn_ben2 <- cn_ben %>% 
  group_by(province, FirstDigit) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>%
  filter(FirstDigit %in% c(1,2,3,4,5,6,7,8,9))


cn_ben2 %>% 
  ggplot() +
  geom_histogram(aes(x = FirstDigit, y = perc), stat = "identity", fill = "red", color = "black") +
  facet_wrap(~province) +
  geom_point(data = benford, aes(x = FirstDigit, y = perc)) + 
  theme_minimal() +
  labs(x = "First Digit", y = "Percentage of Total", title = "")

# Counts <- cn_test %>% 
#   unique() %>% 
#   group_by(province) %>% 
#   summarise(count = n())




# Kolmogorov–Smirnov
ks.test(cn_ben2$perc[cn_ben2$province == "Anhui" & cn_ben2$FirstDigit != 0], benford$perc, exact = TRUE)
ks.test(cn_ben1$count, benford$perc, alternative = "less")


# Kuiper Test
kuiper.test(cn_ben2$perc, alpha = .05)
kuiper.test(cn_ben1$perc, alpha = .05)


# Chi-Squared


mytest <- cn_ben2 %>% 
  filter(province == "Hubei",
         FirstDigit != 0)


chisq.test(mytest$count, p = benford$perc)
chisq.test(cn_ben1$count, p = benford$perc)




# 
# data <- coronavirus::coronavirus
# 
# 
# # ben <- data %>% 
# #   group_by(date) %>% 
# #   summarise(count = n())
# # 
# 
# Counts <- data %>% 
#   group_by(Province.State) %>% 
#   summarise(count = n())
# 
# 
# ben <- data %>% 
#   separate(cases, into = c("FirstNumber"), sep = 1)
# 
# 
# ben %>% 
#   filter(Country.Region == "Mainland China") %>% 
#   ggplot() +
#   geom_histogram(aes(x = FirstNumber), stat = "count") +
#   facet_geo(~Province.State, grid = "china_prov_grid1")
# 



#### Doing using  CUM_CONFIRMED  ########

#### DATA FORMAT CHANGE -- THIS IS NO LONGER NEEDED #####


# 
# cn_ben_cum <- cn_test %>% 
#   separate(cum_confirm, into = c("FirstDigit"), sep = 1)
# 
# 
# cn_ben_cum_total <- cn_ben_cum %>% 
#   group_by(FirstDigit) %>% 
#   summarise(count = n()) %>% 
#   mutate(perc = count/sum(count))
# 
# 
# 
# test <- table(cn_ben_cum_total$perc, benford$perc)
# 
# chisq <- sum((cn_ben_cum_total$count - sum(cn_ben_cum_total$count) * benford$perc)^2 /  (sum(cn_ben_cum_total$count) * benford$perc))
# 
# 
# ch <- pchisq(11.18, 8)
# 
# 
# 
# cn_ben_cum_total %>% 
#   ggplot() +
#   geom_histogram(aes(x = reorder(as.factor(FirstDigit), FirstDigit), y = perc), stat = "identity", fill = "firebrick", color = "black") +
#   geom_point(data = benford, aes(x = reorder(FirstDigit, FirstDigit), y = perc, color = "Expected Value")) +
#   geom_text(aes(x = FirstDigit, y = perc/2, label = round(perc, digits = 3)), fontface = "bold") +
#   labs(x = "First Digit", y = "Percentage of Total", title = "The Cumulative Daily Reports of the Coronavirus for all of China", subtitle = paste0("P-value: ", round(ch, 2)), color = "  Benford's Law") +
#   scale_color_manual(values = "black") +
#   theme_minimal() +
#   theme(legend.position = c(.6,.73),
#         plot.background = element_blank())
# 
# 
# 
# ks <- ks.test(cn_ben_cum_total$perc, benford$perc)
# 1-ks$p.value
# 
# kuiper.test(cn_ben_cum_total$perc, alpha = .05)
# 
# 
# 
# 
# cn_ben_prov <- cn_ben_cum %>% 
#   group_by(province, FirstDigit) %>% 
#   summarise(count = n()) %>% 
#   mutate(perc = count/sum(count))
# 
# 
# 
# 
# cn_ben_prov %>% 
#   ggplot() +
#   geom_histogram(aes(x = FirstDigit, y = perc), stat = "identity", fill = "red", color = "black") +
#   facet_wrap(~province) +
#   geom_point(data = benford, aes(x = FirstDigit, y = perc)) + 
#   theme_minimal() +
#   labs(x = "First Digit", y = "Percentage of Total", title = "Cumulative Daily Reports of the Coronavirus per Province")
# 
# 
# mytest_prov <- cn_ben_prov %>% 
#   filter(province == "Anhui")
# 
# 
# chisq.test(mytest_prov$perc, benford$perc)
# 
# ks.test(mytest_prov$perc, benford$perc)



### I can use the same nCov2019 package to pull country... I'm probably going to have to read in John Hopkins data if I want to be able to be able to look at state specific cases, etc...


# Let's take a look at reported us cases

dat <- data$global


datu <- dat %>% 
  filter(country == "United Kingdom")

datu <- datu %>% 
  separate(cum_confirm, into = c("FirstDigit"), sep = 1) %>% 
  group_by(FirstDigit) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

# Luckily each one has at least 5 observations so "technically" we can say that we fulfill the 5 expected value assumptions

print(datu)

datu %>% 
  ggplot() +
  geom_histogram(aes(x = FirstDigit, y = perc), stat = "identity", fill = "red", color = "black") +
  geom_point(data = benford, aes(x = FirstDigit, y = perc)) + 
  theme_minimal() +
  labs(x = "First Digit", y = "Percentage of Total", title = "")


chisq.test(datu$count, p = benford$perc)

























numbers <- c(5, 23, 19, 30, 12, 1, 90, 82, 105, 83, 72, 65, 54, 203, 221, 26, 37, 48, 50)


numbers <- data.frame(numbers)

numbers <- numbers %>% 
  separate(numbers, into = "FirstDigit", sep = 1)

numbers <- numbers %>% 
  group_by(FirstDigit) %>% 
  summarise(count = n())


numbers %>% 
  ggplot() +
  geom_histogram(aes(x = FirstDigit, y = count), stat = "identity")













