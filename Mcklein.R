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
require(scales)
ggplot(data = datos, aes(x=post_impressions_organic_unique.lifetime,
                         y=post_impressions_unique.lifetime))+
                         geom_point() +
                         scale_x_continuous(labels = comma) + 
                         scale_y_continuous(labels = comma)

ggplot(data=datos, aes(x=post_impressions_unique.lifetime,
                       y=post_impressions_organic_unique.lifetime, colour = post_consumptions.lifetime)) + 
                       geom_point(shape=1) +
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


savePlot <- function(myPlot, plotname) {
  png(paste(plotname,".png", sep = ""), units="in", width=9.5, height=4.5, res=250)
  print(myPlot)
  dev.off()
}

  

# Fans FB Plots -------------------------------------------------------------
fans1 <- ggplot(
  page,
  aes(
    x = date.day,
    y = page_fan_adds_unique.day,
    size = page_impressions.day,
    color = page_engaged_users.day
  )
) +
  scale_color_gradient(high = "#274046",
                       low  = "#e6dada")  +
  geom_boxplot(fill = "white",
               colour = "#0899DB", show.legend = FALSE) +
  geom_jitter(width = 0.19,show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Dia de la semana",
       y = "Nuevos Fans ",
       size = "Vistas de pagina",
       color = "Actividad de Fans") + 
  theme(text = element_text(size=15)) + 
  theme_fivethirtyeight()

fans2 <- ggplot(
  page,
  aes(
    x = date.day,
    y = page_fan_adds_unique.day,
    size = page_impressions.day
  )
) +
  geom_boxplot(fill = "white",
               colour = "#0899DB", show.legend = FALSE) +
  geom_jitter(width = 0.19,show.legend = FALSE, fill = "white") +
  theme_minimal() +
  labs(x = "Dia de la semana",
       y = "Nuevos Fans ",
       size = "Vistas de pagina",
       color = "Actividad de Fans") + 
  theme(text = element_text(size=15)) + 
  theme_fivethirtyeight()

fans3 <- ggplot(
  page,
  aes(
    x = date.day,
    y = page_fan_adds_unique.day
  )
) +
  geom_boxplot(fill = "white",
               colour = "#0899DB", show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Dia de la semana",
       y = "Nuevos Fans ",
       size = "Vistas de pagina",
       color = "Actividad de Fans") + 
  theme(text = element_text(size=15)) + 
  theme_fivethirtyeight()

fans4 <- ggplot(
  page,
  aes(
    x = 1,
    y = page_fan_adds_unique.day
  )
) +
  geom_boxplot(fill = "white",
               colour = "#0899DB", show.legend = FALSE ) +
  theme_minimal() +
  labs(x = "Dia de la semana",
       y = "Nuevos Fans ",
       size = "Vistas de pagina",
       color = "Actividad de Fans") + 
  theme(text = element_text(size=15)) + 
  theme_fivethirtyeight()


savePlot(fans1,"fans1")
savePlot(fans2,"fans2")
savePlot(fans3,"fans3")
savePlot(fans4,"fans4")

summary(page$page_fan_adds_unique.day)





# Else --------------------------------------------------------------------


page_fan_remover_cum <- page %>% 
  transmute(cumsum=cumsum(page_fan_removes_unique.day))

summary(page)

ggplot(page, aes(page_fans.lifetime,page_fan_removes_unique.day)) + geom_point()
ggplot(page, aes(Date,page_fan_removes_unique.day)) + geom_line()


  transmute(cumsum=cumsum(page_fan_removes_unique.day))
  