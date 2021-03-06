library(ggplot2)
library(hrbrthemes)
library(grid)
library(gridExtra)
library(stringr)
library(dplyr)
library(scales)

# Punto 1: Limpiar
Igbebe <- read.csv("liberia_datos_climaticos.csv", sep = ",", na.strings = "", dec = ",")
Igbebe[!complete.cases(Igbebe),]
Igbebe <- na.omit(Igbebe)

View(Igbebe)
str(Igbebe)

names (Igbebe) = c("Fecha", "Temperatura", "Humedad", "Viento", "Lluvia", "Irradiaci�n", "Evapotranspiraci�n")
names (Igbebe)

Igbebe <- 
  Igbebe %>%
mutate (Fecha = as.Date(Fecha, format = "%d/%m/%Y"))

str(Igbebe)

# Punto 2: Histogramas

grid.arrange(
ggplot(Igbebe, aes(y= Temperatura, group = 1, colour = Temperatura)) +
  geom_histogram(
    col = "#F8A91C",
    fill = "#FAC462"
  ) + 
  ggtitle("Variaci�n de la temperatura") +
  xlab("Data") +
  ylab("Temperatura �C") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE, 
    axis = "y",
    grid = "Y,y"
  ),
ggplot(Igbebe, aes(y= Humedad, group = 1, colour = Humedad)) + 
  geom_histogram(
    col = "#26EE94",
    fill = "#10C674"
  ) + 
  ggtitle("Variaci�n de la humedad") +
  xlab("Data") +
  ylab("Humedad %") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE, 
    axis = "y",
    grid = "Y,y"
  ),
ggplot(Igbebe, aes(y= Viento, group = 1, colour = Viento)) + 
  geom_histogram(
    col = "#91BCEF",
    fill = "#6DA9B7"
  ) +
  ggtitle("Velocidad del viento") +
  xlab("Data") +
  ylab("Viento m/s") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE, 
    axis = "y",
    grid = "Y,y"
  ),
ggplot(Igbebe, aes(y= Lluvia, group = 1, colour = Lluvia)) + 
  geom_histogram(
    col = "#1E307C",
    fill = "#848DBA"
  ) + 
  ggtitle("Cantidad de lluvia") +
  xlab("Data") +
  ylab("Lluvia mm") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE, 
    axis = "y",
    grid = "Y,y"
  ),
ggplot(Igbebe, aes(y= Irradiaci�n, group = 1, colour = Irradiaci�n)) + 
  geom_histogram(
    col = "#B67D30",
    fill = "#D39D55"
  ) + 
  ggtitle("Variaci�n de la Irradiaci�n") +
  xlab("Data") +
  ylab("Irradiaci�n km2") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE, 
    axis = "y",
    grid = "Y,y"
  ),
ggplot(Igbebe, aes(y= Evapotranspiraci�n, group = 1, colour = Evapotranspiraci�n)) + 
  geom_histogram(
    col = "#ECEC14",
    fill = "#F0F04E"
  ) +
  ggtitle("Variaci�n de la evapotranspiraci�n") +
  xlab("Data") +
  ylab("Evapotranspiraci�n mm") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE, 
    axis = "y",
    grid = "Y,y"
  ))





# Cosas de l�nea


Promedios <- 
  Igbebe %>%
select(Fecha, Temperatura, Humedad, Viento, Lluvia, Irradiaci�n, Evapotranspiraci�n)%>%
mutate (Fecha = as.Date(Fecha, format = "%d/%m/%Y"))%>%
group_by(Fecha = format(Fecha, "%Y"))%>%
summarise(Temperatura = mean(Temperatura), Humedad = mean(Humedad),
          Viento = mean(Viento), Lluvia = sum(Lluvia),
          Irradiaci�n = mean(Irradiaci�n), Evapotranspiraci�n = sum(Evapotranspiraci�n))
  
grid.arrange(ggplot(Promedios, aes(x=Fecha, y=Temperatura, group = 1, colour = Temperatura)) + 
   geom_line(col = "#F8A91C",
             fill = "#F8A91C") +
     ggtitle("Variaci�n de la temperatura seg�n el a�o") +
     xlab("A�o") +
     ylab("Temperatura �C") +
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"),
ggplot(Promedios, aes(x=Fecha, y=Humedad, group = 1, colour = Humedad)) + 
   geom_line(col = "#26EE94",
             fill = "#26EE94") +
  ggtitle("Variaci�n de la humedad seg�n la a�o") +
  xlab("A�o") +
  ylab("Humedad %") + 
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"),
ggplot(Promedios, aes(x=Fecha, y=Viento, group = 1, colour = Viento)) + 
   geom_line(col = "#91BCEF",
             fill = "#91BCEF") + 
  ggtitle("Velocidad del viento seg�n la a�o") +
  xlab("A�o") +
  ylab("Viento m/s") +
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"),
ggplot(Promedios, aes(x=Fecha, y=Lluvia, group = 1, colour = Lluvia)) + 
   geom_line(col = "#1E307C",
             fill = "#1E307C") + 
  ggtitle("Cantidad de lluvia seg�n el a�o") +
  xlab("A�o") +
  ylab("Lluvia mm") +
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"),
ggplot(Promedios, aes(x=Fecha, y=Irradiaci�n, group = 1, colour = Irradiaci�n)) + 
   geom_line(col = "#B67D30",
             fill = "#B67D30") + 
  ggtitle("Variaci�n de la Irradiaci�n seg�n el a�o") +
  xlab("A�o") +
  ylab("Irradiaci�n km2") +
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"),
ggplot(Promedios, aes(x=Fecha, y=Evapotranspiraci�n, group = 1, colour= Evapotranspiraci�n)) + 
   geom_line(col = "#ECEC14",
             fill = "#ECEC14") + 
  ggtitle("Variaci�n de la evapotranspiraci�n seg�n el a�o") +
  xlab("A�o") +
  ylab("Evapotranspiraci�n mm") +
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"))
  

