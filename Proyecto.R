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

names (Igbebe) = c("Fecha", "Temperatura", "Humedad", "Viento", "Lluvia", "Irradiación", "Evapotranspiración")
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
  ggtitle("Variación de la temperatura") +
  xlab("Data") +
  ylab("Temperatura °C") +
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
  ggtitle("Variación de la humedad") +
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
ggplot(Igbebe, aes(y= Irradiación, group = 1, colour = Irradiación)) + 
  geom_histogram(
    col = "#B67D30",
    fill = "#D39D55"
  ) + 
  ggtitle("Variación de la Irradiación") +
  xlab("Data") +
  ylab("Irradiación km2") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE, 
    axis = "y",
    grid = "Y,y"
  ),
ggplot(Igbebe, aes(y= Evapotranspiración, group = 1, colour = Evapotranspiración)) + 
  geom_histogram(
    col = "#ECEC14",
    fill = "#F0F04E"
  ) +
  ggtitle("Variación de la evapotranspiración") +
  xlab("Data") +
  ylab("Evapotranspiración mm") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE, 
    axis = "y",
    grid = "Y,y"
  ))





# Cosas de línea


Promedios <- 
  Igbebe %>%
select(Fecha, Temperatura, Humedad, Viento, Lluvia, Irradiación, Evapotranspiración)%>%
mutate (Fecha = as.Date(Fecha, format = "%d/%m/%Y"))%>%
group_by(Fecha = format(Fecha, "%Y"))%>%
summarise(Temperatura = mean(Temperatura), Humedad = mean(Humedad),
          Viento = mean(Viento), Lluvia = sum(Lluvia),
          Irradiación = mean(Irradiación), Evapotranspiración = sum(Evapotranspiración))
  
grid.arrange(ggplot(Promedios, aes(x=Fecha, y=Temperatura, group = 1, colour = Temperatura)) + 
   geom_line(col = "#F8A91C",
             fill = "#F8A91C") +
     ggtitle("Variación de la temperatura según el año") +
     xlab("Año") +
     ylab("Temperatura °C") +
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"),
ggplot(Promedios, aes(x=Fecha, y=Humedad, group = 1, colour = Humedad)) + 
   geom_line(col = "#26EE94",
             fill = "#26EE94") +
  ggtitle("Variación de la humedad según la año") +
  xlab("Año") +
  ylab("Humedad %") + 
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"),
ggplot(Promedios, aes(x=Fecha, y=Viento, group = 1, colour = Viento)) + 
   geom_line(col = "#91BCEF",
             fill = "#91BCEF") + 
  ggtitle("Velocidad del viento según la año") +
  xlab("Año") +
  ylab("Viento m/s") +
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"),
ggplot(Promedios, aes(x=Fecha, y=Lluvia, group = 1, colour = Lluvia)) + 
   geom_line(col = "#1E307C",
             fill = "#1E307C") + 
  ggtitle("Cantidad de lluvia según el año") +
  xlab("Año") +
  ylab("Lluvia mm") +
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"),
ggplot(Promedios, aes(x=Fecha, y=Irradiación, group = 1, colour = Irradiación)) + 
   geom_line(col = "#B67D30",
             fill = "#B67D30") + 
  ggtitle("Variación de la Irradiación según el año") +
  xlab("Año") +
  ylab("Irradiación km2") +
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"),
ggplot(Promedios, aes(x=Fecha, y=Evapotranspiración, group = 1, colour= Evapotranspiración)) + 
   geom_line(col = "#ECEC14",
             fill = "#ECEC14") + 
  ggtitle("Variación de la evapotranspiración según el año") +
  xlab("Año") +
  ylab("Evapotranspiración mm") +
   geom_point() +
   theme_ipsum(ticks = TRUE, 
               axis = "y",
               grid = "Y,y"))
  

