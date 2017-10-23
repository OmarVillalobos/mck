library(rattle)
library(plyr)
library(dplyr)
library(Hmisc)
library(lubridate)
library(ggplot2)

Sys.setlocale("LC_TIME", "C") 

getwd()


datos <- read.csv("Post Analysis as of Oct 11.csv", header = TRUE, nrows = 194)
datos[is.na(datos)] <- 0
datos$stories <- datos$share + datos$like + datos$comment
datos.fb <- select(datos,Permalink,Post.Message,Posted,
                   Lifetime.Post.Organic.Impressions,Lifetime.Post.organic.reach,
                   stories,Lifetime.Post.Consumers,Lifetime.Engaged.Users)

datos.fb$Posted <- as.POSIXct(strptime(datos.fb$Posted, "%d/%m/%Y %H", tz="GMT"))
datos.fb$Posted.hour <- as.factor(hour(datos.fb$Posted))
datos.fb$Posted.day <- as.factor(wday(datos.fb$Posted, label= TRUE, abbr = TRUE))



