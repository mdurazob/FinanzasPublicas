library(timeSeries)

timeSeries <- as.numeric(c(
  372245.00,
  339740.00,
  632331.00,
  406360.00,
  351912.00,
  719355.00,
  433904.00,
  367560.00,
  376228.00,
  415174.00,
  368799.00,
  332213.00,
  376706.00,
  377180.00,
  421843.00,
  465903.00,
  389014.00,
  406633.00,
  453860.00,
  377439.00,
  405868.00,
  511065.00,
  399458.00,
  378529.00,
  428536.00,
  436876.20,
  458282.80,
  447959.30,
  405985.40,
  442585.70,
  491028.70,
  413058.10,
  423033.50,
  428244.10,
  428932.10,
  496617.40,
  391173.00,
  473438.10,
  543159.60,
  540277.50,
  394936.50,
  527803.00,
  406331.60
  ))
  
  
  
  
  
  
  
  
library(dplyr)
## import IngresosPresupuestarios Dataset
library(tseries)
library(forecast)
library(astsa)
library(stats)
library(seasonal)
## Aqui usamos meedias moviles para "suavizar" para hacerla más estable la serie que despues se desesetacionalizaràn
UnivariateTSIngresosPresupuestarios <- ts(IngresosPresupuestarios, start = c(2016, 10), end = c(2020,4), frequency=12)
TSIngresosPresupuestarios <- ts(IngresosPresupuestariosDataset, start = c(2016, 10), end = c(2020,4), frequency=12)

tsclean(TSIngresosPresupuestarios)
plot(TSIngresosPresupuestarios)

#Correr Test para ver si hay extacionariedad
TSIngresosPresupuestariosStationarityTest <- adf.test(TSIngresosPresupuestarios, alternative="stationary")
TSIngresosPresupuestariosStationarityTest

## Usar medias moviles para suavisar

IngresosPresupuestariosSuavizado <- ma(TSIngresosPresupuestarios, order=7)
IngresosPresupuestariosSuavizado <- na.omit(IngresosPresupuestariosSuavizado)

plot(IngresosPresupuestariosSuavizado)
TSIngresosPresupuestariosStationarityTest <- adf.test(IngresosPresupuestariosSuavizado, alternative="stationary")
TSIngresosPresupuestariosStationarityTest 

## necesitas que sea estacionaria para usar ARIMA, sólo las estacionarias aplica,
## que sean estacionarias significa que su media y su varianza son constantes. es mas facil predecir cuando media y varianza son constantes
## que sea estacional (seasonal) es que cada tiempo se comporta muy similar, 
## si es seasonal es SARIMA
## Si no es seasonal usas ARIMAs

## Primero desestacionalizar, para ver la tendencia real, no una influida por ciclos
#Calcular componente estacional
#Quitar NAS para calcularlo
## TSIngresosPresupuestariosSinNASuavizado <- na.omit(TSIngresosPresupuestarios4)
componenteEstacional <- stl(IngresosPresupuestariosSuavizado, s.window="periodic") ## usamos la ya suavizada
IngresosPresupuestariosDesestacionalizado <- seasadj(componenteEstacional)
plot(IngresosPresupuestariosDesestacionalizado)
plot(componenteEstacional)
plot(perro)
## Desestacionalizarla

##☺ Residuos estan variando alrededor del 0 y eso es bueno, prueba para que modelo funcione son pruebas de residuos
## los resiudos deben distribuirse de forma normal, tener media 0 y varianza 1 para
## ver eso debes hacer un histograma para ver que se asemeje a una normal

##@ MODELO ARIMA (no da la tendencia, esa chamba se hizo antes) este es mas bien predictivo
modeloarima <- auto.arima(IngresosPresupuestariosDesestacionalizado, seasonal = FALSE)

prediccion1 <- forecast(modeloarima, h=12)
plot(prediccion1)

## Modelo alternativo 1: (graficas para ver si el modelo funciona correctamente)
## La forma de checar que el modelo funcione es el análisis de residuso tanto de ARIMA COMO SARIMA)
#En la primer gráfica varia alrededor del cero
# en la de ACF se debe ver aleatoriamente alrededor del 0
# En la normal QQ PLot se debe ver como una normal
# La ultima de p values se deberían de ver valores cercanos al 0, el p value crece conforme a mas adelante se quiere predecir, entre más adelante más error se está teniendo
# tiene sentido porque tenemos 40 datos, el p value se comporta bien al principio pero después conforme queremos predecir tiene un valor más grande
# en el eje x están los períodos a predecir, a valores más grandes el p-value se vuelve grande
modeloalternativo <- sarima(UnivariateTSIngresosPresupuestarios, 0, 1, 1, 1, 1, 0, 12)


predicciones1<-sarima.for(UnivariateTSIngresosPresupuestarios, 0, 1, 1, 1, 1, 0, 12, n.ahead=12)
predicciones1<-sarima.for(IngresosPresupuestarios, 0, 1, 1, 1, 1, 0, 12, n.ahead=12)


fit <- arima(UnivariateTSIngresosPresupuestarios, seasonal = TRUE)
## lo mas cercano a sarima, igual hubiera quedado con 2,0,0
fit <- arima(UnivariateTSIngresosPresupuestarios, order=c(0,1,1),seasonal = list(order = c(1,1,0), period = 12),method="ML")

prediccion1 <- forecast(fit, h=12)

prediccion1
plot(prediccion1)
# plot arima v2.0 como sarima
## add the date to the list in prediccion1
library(dplyr)
library(highcharter)
library(timetk)
library(xts)


hchart(prediccion1) %>%
  hc_xAxis(type = "datetime", labels = list(format = '{value:%b %y}')) %>%
  hc_add_annotation( 
    labels = list(
      list(
        point = list(
          xAxis = 200,
          yAxis = 0,
          x = Fechas$Fecha,
          y = Lvl1dfStatus$y[x_position]
        ),
        text = "Is it OK?"
      )
    )
  )

#Ingresos crecen a nada, a solo 0.10% respecto al año pasado, no pasamos de 400- 700k ?¿ al mes


library(highcharter)
hchart(prediccion1)
hchart(fits1)
library(forecast)
library(ggplot2)
library(highcharter)
ggmonthplot(TSIngresosPresupuestarios)
ggseason <- ggseasonplot(TSIngresosPresupuestarios)
hchart(ggseason)
library(plotly)
ggseasonplot <- ggplotly(ggseason)
ggseasonplot

ggplotly(ggiris)
ggplotly(
  p = ggseason::last_plot(),
  width = NULL,
  height = NULL,
  tooltip = "all",
  dynamicTicks = FALSE,
  layerData = 1,
  originalData = TRUE,
  source = "A",
  ...
)

autoplot(predicciones1)
IngresosPresupuestariosSerieCortada <- slice(IngresosPresupuestariosSerie, -c(40,41,42,43))

##Mochar el modelo para demostrar que el comportamiento de la pandemia fue anormal al predicho
## Impacto de la pandemia: predecir con base en pandemia... hubo un comportamiento anormal a como venia comportándose
## Impacto influyó a que hubiera un comportamiento anormal creciente en los últimos meses
# puedo predecir con base en el resultado sin pandemia, antes de la pandemia el modelo
# estimaba que el costo financiero iba a ser x pero con la pandemia el valor real es y
modeloalternativoCortado <- sarima(IngresosPresupuestariosSerieCortada, 0, 1, 1, 1, 1, 0, 12)
prediccionescortadas<-sarima.for(IngresosPresupuestariosSerieCortada, 0, 1, 1, 1, 1, 0, 12, n.ahead=16)
prediccionescortadas$pred #Para imprimir los valores predichos
predicciones1$pred
