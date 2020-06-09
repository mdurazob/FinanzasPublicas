## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
mayo2019Resumen <- read.table(file = 'mayo2019Resumen.tsv', sep = '\t', header = FALSE, na.strings=c(""," ","NA"))
## mayo2019Resumen <- slice(mayo2019Resumen, -c(1,2,3,4,5,6,7))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



mayo2019DeudaInterna <- read.table(file = 'mayo2019DeudaInterna.tsv', sep = '\t', header = FALSE)
mayo2019DeudaExterna <- read.table(file = 'mayo2019DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
mayo2019Resumen <- subset(mayo2019Resumen, V2!= "NA")

mayo2019Resumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
mayo2019Resumen <- mayo2019Resumen %>% select(Concepto, V2, V3, V4)
##ya viene splited
##mayo2019SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(mayo2019Resumen$V2), " ", fixed = TRUE)))
##mayo2019SplitedResumen <- mayo2019SplitedResumen %>% select(X1, X2)
##mayo2019SplitedResumen$Concepto <- mayo2019Resumen$Concepto

colnames(mayo2019Resumen) <- c("Concepto", "2018", "2019", "Variación")
## mayo2019SplitedResumen$Variacion <- 100 * (as.numeric(as.character(gsub(",","", mayo2019SplitedResumen$"2019"))) - as.numeric(as.character(gsub(",","", mayo2019SplitedResumen$"2018")))) / as.numeric(as.character(gsub(",","", mayo2019SplitedResumen$"2019")))

## mayo2019Resumen <- mayo2019SplitedResumen

# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


mayo2019SituacionFinanciera <- read.table(file = 'mayo2019SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), fill=TRUE, skip='1')

#Dropping Nas in second colu,n
mayo2019SituacionFinanciera <- subset(mayo2019SituacionFinanciera, V2!= "Partidas informativas")

#mayo2019SituacionFinanciera <- slice(mayo2019SituacionFinanciera, -c(1, 2, 3, 4, 5))

#mayo2019SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(mayo2019SituacionFinanciera$V2), " ", fixed = TRUE)))




#Renombrar columnas
mayo2019SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

mayo2019SituacionFinanciera <- mayo2019SituacionFinanciera  %>%
  select(V5, V8, V11, Concepto)
colnames(mayo2019SituacionFinanciera) <- c("2018","2019", "Crecimiento real", "Concepto")


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
mayo2019DeudaInterna <- slice(mayo2019DeudaInterna, -c(1, 2, 3, 4))

## colnames(mayo2019DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
# mayo2019SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(mayo2019DeudaInterna$V3), " ", fixed = TRUE)))
#mayo2019SplitedDeudaInterna$V1 <- mayo2019DeudaInterna$V1
# mayo2019SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", mayo2019SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", mayo2019SplitedDeudaInterna$X1)))
mayo2019DeudaInterna <- mayo2019DeudaInterna %>%
  select(Concepto, V2, V3, V4)

colnames(mayo2019DeudaInterna) <- c("Concepto", "Intereses", "Comisiones", "Total")

 #mayo2019DeudaInterna <- mayo2019SplitedDeudaInterna
# Select only v2
mayo2019DeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")
## Cambiar columna de "concepto"

##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
 mayo2019DeudaExterna <- slice(mayo2019DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
 mayo2019SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(mayo2019DeudaExterna$V3), " ", fixed = TRUE)))
 mayo2019SplitedDeudaExterna$V1 <- mayo2019DeudaExterna$V1

##Join
mayo2019MergedDeudaExterna <- merge(mayo2019DeudaExterna, mayo2019SplitedDeudaExterna, by = "V1")

mayo2019MergedDeudaExterna <- mayo2019MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
mayo2019DeudaExterna <- mayo2019MergedDeudaExterna  
colnames(mayo2019DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(mayo2019Resumen, file = "mayo2019Resumen.csv", fileEncoding = "UTF-8")
write.csv(mayo2019SituacionFinanciera, file ="mayo2019SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(mayo2019DeudaExterna, file ="mayo2019DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(mayo2019DeudaInterna, file ="mayo2019DeudaInterna.csv", fileEncoding = "UTF-8")

