library(timeSeries)
timeSeries <- as.numeric(c(
                15604.00,
                19029.00,
                133237.00,
                45432.00,
                17134.00,
                39129.00,
                26664.00,
                14694.00,
                136562.00,
                39194.00,
                10939.00,
                29378.00,
                13057.00,
                15596.00,
                145413.00,
                58743.00,
                23012.00,
                43916.00,
                41607.00,
                14443.00,
                150007.00,
                32649.00,
                24611.00,
                32466.00,
                17060.00,
                15227.10,
                161289.30,
                69905.20,
                24697.70,
                44227.00,
                43852.00,
                19231.30,
                160033.00,
                37977.70,
                24180.50,
                40778.90,
                18096.00,
                16181.00,
                167313.70,
                63242.00,
                32323.00,
                52079.30,
                52148.00
))

library(dplyr)
SerieCostoFinanciero <- timeSeries %>% select(CostoFinanciero)

library(tseries)
library(forecast)
library(astsa)
library(stats)
library(seasonal)
## Aqui usamos meedias moviles para "suavizar" para hacerla más estable la serie que despues se desesetacionalizaràn
TSCostoFinanciero <- ts(CostoFinancieroSerie, start = c(2016, 10), end = c(2020,4), frequency=12)

tsclean(TSCostoFinanciero)
plot(TSCostoFinanciero)

#Correr Test para ver si hay extacionariedad
TSCostoFinanciero7 <- adf.test(TSCostoFinanciero, alternative="stationary")
TSCostoFinanciero7

## Usar medias moviles
TSCostoFinanciero4 <- ma(TSCostoFinanciero, order=7)

plot(TSCostoFinanciero4)

## necesitas que sea estacionaria para usar ARIMA, sólo las estacionarias aplica,
## que sean estacionarias significa que su media y su varianza son constantes. es mas facil predecir cuando media y varianza son constantes
## que sea estacional (seasonal) es que cada tiempo se comporta muy similar, 
## si es seasonal es SARIMA
## Si no es seasonal usas ARIMAs

## Primero desestacionalizar, para ver la tendencia real, no una influida por ciclos
#Calcular componente estacional
#Quitar NAS para calcularlo
TSCostoFinancieroSinNASuavizado <- na.omit(TSCostoFinanciero4)
componenteEstacional <- stl(TSCostoFinancieroSinNASuavizado, s.window="periodic") ## usamos la ya suavizada
desestacionalizado <- seasadj(componenteEstacional)
plot(desestacionalizado)
plot(componenteEstacional)
plot(perro)
## Desestacionalizarla

##☺ Residuos estan variando alrededor del 0 y eso es bueno, prueba para que modelo funcione son pruebas de residuos
## los resiudos deben distribuirse de forma normal, tener media 0 y varianza 1 para
## ver eso debes hacer un histograma para ver que se asemeje a una normal

##@ MODELO ARIMA (no da la tendencia, esa chamba se hizo antes) este es mas bien predictivo
modeloarima <- auto.arima(desestacionalizado, seasonal = FALSE)

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
modeloalternativo <- sarima(CostoFinancieroSerie, 0, 1, 1, 1, 1, 0, 12)


sarima.for(CostoFinancieroSerie, 0, 1, 1, 1, 1, 0, 12, n.ahead=12)

CostoFinancieroSerieCortada <- slice(CostoFinancieroSerie, -c(40,41,42,43))

##Mochar el modelo para demostrar que el comportamiento de la pandemia fue anormal al predicho
## Impacto de la pandemia: predecir con base en pandemia... hubo un comportamiento anormal a como venia comportándose
## Impacto influyó a que hubiera un comportamiento anormal creciente en los últimos meses
# puedo predecir con base en el resultado sin pandemia, antes de la pandemia el modelo
# estimaba que el costo financiero iba a ser x pero con la pandemia el valor real es y
modeloalternativoCortado <- sarima(CostoFinancieroSerieCortada, 0, 1, 1, 1, 1, 0, 12)
sarima.for(CostoFinancieroSerieCortada, 0, 1, 1, 1, 1, 0, 12, n.ahead=12)
