#library(dygraphs)
library(tidyverse)



dat1 <- sample("1", 10000*0.3010, replace = T)
dat2 <- sample("2", 10000*0.1761, replace = T)
dat3 <- sample("3", 10000*0.1249 , replace = T)
dat4 <- sample("4", 10000*0.0969 , replace = T)
dat5 <- sample("5", 10000*0.0792 , replace = T)
dat6 <- sample("6", 10000*0.0670 , replace = T)
dat7 <- sample("7", 10000*0.0580, replace = T)
dat8 <- sample("8", 10000*0.0512, replace = T)
dat9 <- sample("9", 10000*0.0458, replace = T)

dat1 <- data.frame(dat1)
dat2 <- data.frame(dat2)
dat3 <- data.frame(dat3)
dat4 <- data.frame(dat4)
dat5 <- data.frame(dat5)
dat6 <- data.frame(dat6)
dat7 <- data.frame(dat7)
dat8 <- data.frame(dat8)
dat9 <- data.frame(dat9)

names(dat1)[1] <- "Numbers"
names(dat2)[1] <- "Numbers"
names(dat3)[1] <- "Numbers"
names(dat4)[1] <- "Numbers"
names(dat5)[1] <- "Numbers"
names(dat6)[1] <- "Numbers"
names(dat7)[1] <- "Numbers"
names(dat8)[1] <- "Numbers"
names(dat9)[1] <- "Numbers"


dat <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9)



dat <- as.factor(dat$Numbers)



dat <- data.frame(dat)


names(dat)[1] <- "Numbers"


dat <- dat %>% 
  group_by(Numbers) %>% 
  summarise(count = n())


dat %>% 
  ggplot() +
  geom_histogram(aes(x = Numbers, y = count/10000), stat = "identity", fill = "black") +
  geom_text(aes(x = Numbers, y = count/10000, label = round(count/10000, digits = 3)), vjust = -.15, fontface = "bold") +
  theme_minimal() +
  labs(x = "First Digit", y = "Percentage of Total", title = "Benford's Law")
  
  












