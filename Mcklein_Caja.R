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

Sys.setlocale("LC_TIME", "C") 

getwd()
datos <- read.csv("CAJA-JUL-AGO.csv", header = TRUE)
datos$Fecha <- as.character(datos$Fecha)
datos$Fecha <- as.POSIXct(strptime(datos$Fecha,"%d/%m/%y %I:%M %p"))
datos$horas <- as.factor(hour(datos$Fecha))

ggplot(datos, aes(x=Ventas.netas,fill=horas)) +
  geom_histogram(aes(y=..density..), binwidth = 20)




salary.hist.density <- ggplot(df, aes(x=Salary, fill = Overpaid)) + 
  geom_histogram(aes(y = ..density..), binwidth = 3000) + 
  scale_fill_brewer(palette="Set3") + 
  geom_vline(xintercept = mid, size = 1, colour = "#FF3721",
             linetype = "dashed") + 
  geom_vline(xintercept = med, size = 1, colour = "#2c3e50",
             linetype = "dashed") + 
  theme_grey()
