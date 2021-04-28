library(dplyr)
library(zoo)
library(DataCombine)

df = expand.grid(site = factor(seq(10)),
                 year = 2000:2004,
                 day = 1:50)
# use Poisson to make math easy to check moving means of temperature
df$temp = rpois(dim(df)[1], 5) 
# Assume rains 33% of the days and averages 5 mm each time but highly variable
df$precip = rbinom(dim(df)[1], 1, 1/3) * rlnorm(dim(df)[1], log(5), 1)

# Order by group and date
df = df[order(df$site, df$year, df$day), ]
df.slide = slide(df, 
                 Var = "temp", 
                 GroupVar = c("site", "year"), 
                 slideBy = -1, 
                 NewVar='temp.lag1')
head(df.slide, 75)

# moving mean for that day and previous days (e.g. 5 represents the mean of that day and the for previous days)
df2 = df %>%
  group_by(site, year) %>%
  arrange(site, year, day) %>%
  mutate(temp.5 = rollmean(x = temp, 5, align = "right", fill = NA))
head(df2, 75)

# moving mean for the previous days not including the current day (e.g. 5 represents the mean of the 5 previous days)
df2 = df2 %>%
  mutate(temp.lag1 = lag(temp, n = 1)) %>%
  mutate(temp.5.previous = rollapply(data = temp.lag1, 
                                     width = 5, 
                                     FUN = mean, 
                                     align = "right", 
                                     fill = NA, 
                                     na.rm = T))
head(df2, 75)
