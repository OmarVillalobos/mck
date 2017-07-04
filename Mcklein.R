library(gbm)
library(rattle)
library(nnet)
library(tree)
library(randomForest)
library(plyr)
library(dplyr)
library(Hmisc)
library(lubridate)
library(tseries)
library(forecast)
library(ggplot2)

# Sys.setlocale("LC_TIME", "C") will help recognize AM PM  time format

# Posts -------------------------------------------------------------------
getwd()
setwd("C:/Users/OmarVr/Documents/R/DATUM")
datos <- read.csv("keymetrics.csv", header = TRUE)
datos$Posted <- as.character(datos$Posted)
datos$Posted <- as.POSIXct(strptime(datos$Posted, "%m/%d/%y %I:%M %p", tz="GMT"))
datos$Posted.hour <- as.factor(hour(datos$Posted))
datos$Posted.day <- as.factor(wday(datos$Posted, label= TRUE, abbr = TRUE))


ggplot(data=datos, aes(x=post_impressions_organic_unique.lifetime,
                       y=post_impressions_unique.lifetime, colour = post_consumptions.lifetime,
                       scale_fill_gradient(low = 'red', high = 'yellow'))) + 
                       geom_point(shape=2) +
                       geom_smooth(method='lm') +
                       facet_wrap(~Type)

ggplot( data=datos, aes(x=Posted.hour, y=post_impressions_organic_unique.lifetime 
                        )) + 
                        geom_boxplot(aes(colour=Type))

ggplot( data=datos, aes(x=Posted.day, y=post_impressions_organic_unique.lifetime,
                        colour=Type)) + 
                        geom_boxplot() + 
                        scale_fill_brewer(palette = "Blues") + 
                        facet_wrap(~Type)

ggplot(datos,aes(x=post_impressions_organic_unique.lifetime, y=post_impressions_paid_unique.lifetime)) +
  geom_point() + 
  geom_smooth(method='lm')

df <- datos[datos$post_impressions_paid.lifetime > 15000,]

summary(df$Type)
summary(df$Posted.day)
summary(df$Posted.hour)
summary(df$post_negative_feedback.lifetime)




# Page  -------------------------------------------------------------------
library(wesanderson)
library(ggthemes)
getwd()
setwd("C:/Users/OmarVr/Documents/R/DATUM")
page <- read.csv("keymetrics-page.csv", header = TRUE)
page$Date <- as.POSIXct(strptime(page$Date, "%m/%d/%y", tz="GMT"))
page$date.day <- as.factor(wday(page$Date, label= TRUE, abbr = TRUE))


savePlot <- function(myPlot) {
  jpeg("myPlot.jpeg", units="in", width=12, height=8, res=300)
  print(myPlot)
  dev.off()
}


mid <- mean(page$page_engaged_users.day)
pic1 <- ggplot(page, aes(x=date.day, y=page_fan_adds_unique.day, size=page_impressions.day, 
                 color=page_engaged_users.day,fill=date.day)) +
                 scale_color_gradient(high = "#2c3e50", low  = "#bdc3c7")  +
                 scale_fill_brewer(palette="Spectral") +
                 scale_size_area() +
                 geom_boxplot() + 
                 geom_jitter(width = 0.18) + 
                 theme_minimal() + 
                 theme(legend.position = "right")

pic1 <- ggplot(page, aes(x=date.day, y=page_fan_adds_unique.day, size=page_impressions.day, 
  color=page_engaged_users.day,fill=date.day)) +
  scale_color_gradient(high = "#2c3e50", low  = "#bdc3c7")  +
  scale_fill_manual(values = c("#F8BA6D","#8FC072", "#FF9666","#8368BF","#C93130","#C0E1AD","#3E86EA")) +
  scale_size_area() +
  geom_boxplot() + 
  geom_jitter(width = 0.18) + 
  theme_minimal() + 
  theme(legend.position = "right")
pic1
savePlot(pic1)


page_fan_remover_cum <- page %>% 
  transmute(cumsum=cumsum(page_fan_removes_unique.day))

summary(page)

ggplot(page, aes(page_fans.lifetime,page_fan_removes_unique.day)) + geom_point()
ggplot(page, aes(Date,page_fan_removes_unique.day)) + geom_line()


  transmute(cumsum=cumsum(page_fan_removes_unique.day))
  