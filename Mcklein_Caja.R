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
library(tidyr)

Sys.setlocale("LC_TIME", "C") 

getwd()
datos <- read.csv("CAJA-ENE-JUN.csv", header = TRUE)
datos <- datos %>% unite("Fecha",c("Fecha","Hora"), sep = " ")
datos$Fecha <- as.POSIXct(strptime(datos$Fecha,"%d/%m/%Y %H:%M:%S"))
datos$horas <- as.factor(hour(datos$Fecha))
datos$day <- as.factor(wday(datos$Fecha, label= TRUE, abbr = TRUE))
datos$month <- as.factor(month(datos$Fecha, label = TRUE, abbr = TRUE))
datos$Ventas.netas <- as.numeric(datos$Ventas.netas)
datos <- datos[datos$Fecha < as.Date("2017-06-30 23:00:00"),]
datos.h1 <- datos %>% select(Fecha, Ventas.netas, day, month, horas)


datos <- read.csv("CAJA-JUL-AGO.csv", header = TRUE)
datos$Fecha <- as.character(datos$Fecha)
datos$Fecha <- as.POSIXct(strptime(datos$Fecha,"%d/%m/%y %I:%M %p"))
datos$horas <- as.factor(hour(datos$Fecha))
datos$day <- as.factor(wday(datos$Fecha, label= TRUE, abbr = TRUE))
datos$month <- as.factor(month(datos$Fecha, label = TRUE, abbr = TRUE))
datos <- datos[datos$Ventas.netas > 0,]
datos.h2 <- datos %>% select(Fecha, Ventas.netas, day, month, horas) 
datos <- rbind(datos.h1,datos.h2)

mea <- mean(datos$Ventas.netas)
devup <- mea + sd(datos$Ventas.netas)
devdow <- mea - sd(datos$Ventas.netas)

ggplot(datos, aes(day,Ventas.netas)) + 
  geom_boxplot() + 
  scale_fill_gradient(low = "red", high = "yellow") +
  geom_hline(yintercept = mea, size= 1, colour="#FF3721",
             linetype = "dashed",show.legend = TRUE) + 
  geom_hline(yintercept = devup, size= 1, colour="#053FC9",
             linetype = "dashed",show.legend = TRUE) + 
  geom_hline(yintercept = devdow, size= 1, colour="#053FC9",
             linetype = "dashed",show.legend = TRUE) + 
  scale_y_continuous(limits = c(0,500)) + 
  annotate("text", x = 5, y= 450, label = "Ventas a la media tarde (5,6,7,8 P.M.)")

  
venta.hora <- ggplot(datos, aes(x=Ventas.netas,fill=horas)) +
  geom_histogram(aes(y=..density..), binwidth = 20) + 
  geom_vline(xintercept = mea, size= 1, colour = "#FF3721",
             linetype= "dashed", show.legend = TRUE)

venta.day <- ggplot(datos, aes(x=Ventas.netas,fill=day)) +
  geom_histogram(aes(y=..density..), binwidth = 20) + 
  geom_vline(xintercept = mea, size= 1, colour = "#FF3721",
             linetype= "dashed", show.legend = TRUE)
venta.day

venta.month <- ggplot(datos, aes(x=Ventas.netas,fill=month)) +
  geom_histogram(aes(y=..density..), binwidth = 20) + 
  geom_vline(xintercept = mea, size= 1, colour = "#FF3721",
             linetype= "dashed", show.legend = TRUE)
venta.month

ventas.jitter <- ggplot(
  data = datos,
  aes(x = horas, y = day,
      colour = Ventas.netas)
) +
  geom_jitter(width = 0.20, alpha = 1, height = 0.20,
              size = 2.5, shape = 19) + 
  scale_color_gradient(low = "red",high = "yellow")
ventas.jitter




