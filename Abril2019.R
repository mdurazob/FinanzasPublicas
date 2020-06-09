## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
abril2019Resumen <- read.table(file = 'abril2019Resumen.tsv', sep = '\t', header = FALSE, na.strings=c(""," ","NA"))
## abril2019Resumen <- slice(abril2019Resumen, -c(1,2,3,4,5,6,7))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



abril2019DeudaInterna <- read.table(file = 'abril2019DeudaInterna.tsv', sep = '\t', header = FALSE)
abril2019DeudaExterna <- read.table(file = 'abril2019DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
abril2019Resumen <- subset(abril2019Resumen, V2!= "NA")

abril2019Resumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
abril2019Resumen <- abril2019Resumen %>% select(Concepto, V2, V3, V4)
##ya viene splited
##abril2019SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(abril2019Resumen$V2), " ", fixed = TRUE)))
##abril2019SplitedResumen <- abril2019SplitedResumen %>% select(X1, X2)
##abril2019SplitedResumen$Concepto <- abril2019Resumen$Concepto

colnames(abril2019Resumen) <- c("Concepto", "2018", "2019", "Variación")
## abril2019SplitedResumen$Variacion <- 100 * (as.numeric(as.character(gsub(",","", abril2019SplitedResumen$"2019"))) - as.numeric(as.character(gsub(",","", abril2019SplitedResumen$"2018")))) / as.numeric(as.character(gsub(",","", abril2019SplitedResumen$"2019")))

## abril2019Resumen <- abril2019SplitedResumen

# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


abril2019SituacionFinanciera <- read.table(file = 'abril2019SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), fill=TRUE, skip='1')

#Dropping Nas in second colu,n
abril2019SituacionFinanciera <- subset(abril2019SituacionFinanciera, V2!= "Partidas informativas")

#abril2019SituacionFinanciera <- slice(abril2019SituacionFinanciera, -c(1, 2, 3, 4, 5))

#abril2019SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(abril2019SituacionFinanciera$V2), " ", fixed = TRUE)))




#Renombrar columnas
abril2019SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

abril2019SituacionFinanciera <- abril2019SituacionFinanciera  %>%
  select(V5, V8, V11, Concepto)
colnames(abril2019SituacionFinanciera) <- c("2018","2019", "Crecimiento real", "Concepto")


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
abril2019DeudaInterna <- slice(abril2019DeudaInterna, -c(1, 2, 3, 4))

## colnames(abril2019DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
abril2019SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(abril2019DeudaInterna$V3), " ", fixed = TRUE)))
abril2019SplitedDeudaInterna$V1 <- abril2019DeudaInterna$V1
abril2019SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", abril2019SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", abril2019SplitedDeudaInterna$X1)))
#abril2019DeudaInterna <- abril2019SplitedDeudaInterna %>%
  ##select(Concepto, V2, V3, V4)

colnames(abril2019SplitedDeudaInterna) <- c("Comisiones", "Total", "Concepto", "Intereses")

 abril2019DeudaInterna <- abril2019SplitedDeudaInterna
# Select only v2
abril2019DeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")
## Cambiar columna de "concepto"

##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
 abril2019DeudaExterna <- slice(abril2019DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
 abril2019SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(abril2019DeudaExterna$V3), " ", fixed = TRUE)))
 abril2019SplitedDeudaExterna$V1 <- abril2019DeudaExterna$V1

##Join
abril2019MergedDeudaExterna <- merge(abril2019DeudaExterna, abril2019SplitedDeudaExterna, by = "V1")

abril2019MergedDeudaExterna <- abril2019MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
abril2019DeudaExterna <- abril2019MergedDeudaExterna  
colnames(abril2019DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(abril2019Resumen, file = "abril2019Resumen.csv", fileEncoding = "UTF-8")
write.csv(abril2019SituacionFinanciera, file ="abril2019SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(abril2019DeudaExterna, file ="abril2019DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(abril2019DeudaInterna, file ="abril2019DeudaInterna.csv", fileEncoding = "UTF-8")

