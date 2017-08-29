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
datos <- datos[datos$Ventas.netas > 0,]
datos <- datos[datos$Ventas.netas < 500,]


mea <- mean(datos$Ventas.netas)
devup <- mea + sd(datos$Ventas.netas)
devdow <- mea - sd(datos$Ventas.netas)
  
ggplot(datos, aes(x=Ventas.netas,fill=horas)) +
  geom_histogram(aes(y=..density..), binwidth = 20)

ggplot(datos, aes(horas,Ventas.netas), fill= Total.recaudado) + 
  geom_boxplot() + 
  scale_fill_gradient(low = "red", high = "yellow") +
  geom_hline(yintercept = mea, size= 1, colour="#FF3721",
             linetype = "dashed",show.legend = TRUE) + 
  geom_hline(yintercept = devup, size= 1, colour="#053FC9",
             linetype = "dashed",show.legend = TRUE) + 
  geom_hline(yintercept = devdow, size= 1, colour="#053FC9",
             linetype = "dashed",show.legend = TRUE) + 
  scale_y_continuous(limits = c(0,500)) + 
  annotate("text", x = 450, , label = "Some text")
  


summary(datos$Ventas.netas)



