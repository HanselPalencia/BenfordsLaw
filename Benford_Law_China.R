library(coronavirus)
library(tidyverse)
library(geofacet)
library(nCov2019)
library(stats)
library(circular)



data <- load_nCov2019(lang = "en")

cn_test <- data$province

cn_test <- cn_test %>% 
  unique()


cn_test <- cn_test[order(cn_test$province, cn_test$time),]


### Now we have to improve the actual reported per day data (Not sure why this isn't already a column)

result <- c()
prov <- c()
confirmed <- c()

for (i in 1:nrow(cn_test)) {
  
  prov <- cn_test$province
  confirmed <- cn_test$cum_confirm
  
  
  if (i == 1) {
    
    result[i] <- confirmed[i]
    
  }
  
  else {
    
    if (prov[i] == prov[i-1]) {
      
      result[i] <- confirmed[i] - confirmed[i-1]
      
    }
    
    else {
      
      result[i] <- confirmed[i]
      
    }
    
  }
  
  
}


result <- data.frame(result)

cn_test <- cbind(cn_test, result)

names(cn_test)[7] <- "DailyReported"

cn_test2 <- cn_test %>% 
  filter(DailyReported > 0)


cn_ben <- cn_test2 %>% 
  separate(DailyReported, into = c("FirstDigit"), sep = 1)


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




cn_ben2 <- cn_ben %>% 
  group_by(province, FirstDigit) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>%
  filter(FirstDigit %in% c(1,2,3,4,5,6,7,8,9))

cn_ben2 %>% 
  ggplot() +
  geom_histogram(aes(x = FirstDigit, y = perc), stat = "identity", fill = "red") +
  facet_wrap(~province) +
  geom_point(data = benford, aes(x = FirstDigit, y = perc)) + 
  theme_minimal()

# Counts <- cn_test %>% 
#   unique() %>% 
#   group_by(province) %>% 
#   summarise(count = n())



# Kolmogorov–Smirnov
ks.test(cn_ben2$perc[cn_ben2$province == "Anhui" & cn_ben2$FirstDigit != 0], benford$perc, exact = TRUE)
ks.test(cn_ben1$perc, benford$perc)


# Kuiper Test
kuiper.test(cn_ben2$perc, alpha = .05)
kuiper.test(cn_ben1$perc, alpha = .05)


# Chi-Squared


mytest <- cn_ben2 %>% 
  filter(province == "Anhui",
         FirstDigit != 0)


chisq.test(mytest$perc, benford$perc)
chisq.test(cn_ben1$perc, benford$perc)




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


cn_ben_cum <- cn_test %>% 
  separate(cum_confirm, into = c("FirstDigit"), sep = 1)


cn_ben_cum_total <- cn_ben_cum %>% 
  group_by(FirstDigit) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))


cn_ben_cum_total %>% 
  ggplot() +
  geom_histogram(aes(x = FirstDigit, y = perc), stat = "identity", fill = "red") +
  geom_point(data = benford, aes(x = FirstDigit, y = perc)) + 
  theme_minimal()


chisq.test(cn_ben_cum_total$perc, benford$perc)

ks <- ks.test(cn_ben_cum_total$perc, benford$perc)
1-ks$p.value

kuiper.test(cn_ben_cum_total$perc, alpha = .05)





cn_ben_prov <- cn_ben_cum %>% 
  group_by(province, FirstDigit) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))




cn_ben_prov %>% 
  ggplot() +
  geom_histogram(aes(x = FirstDigit, y = perc), stat = "identity", fill = "red") +
  facet_wrap(~province) +
  geom_point(data = benford, aes(x = FirstDigit, y = perc)) + 
  theme_minimal()


mytest_prov <- cn_ben_prov %>% 
  filter(province == "Anhui")


chisq.test(mytest_prov$perc, benford$perc)

ks.test(mytest_prov$perc, benford$perc)


































