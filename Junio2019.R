## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
junio2019Resumen <- read.table(file = 'junio2019Resumen.tsv', sep = '\t', header = FALSE, na.strings=c(""," ","NA"))
## junio2019Resumen <- slice(junio2019Resumen, -c(1,2,3,4,5,6,7))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



junio2019DeudaInterna <- read.table(file = 'junio2019DeudaInterna.tsv', sep = '\t', header = FALSE)
junio2019DeudaExterna <- read.table(file = 'junio2019DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
junio2019Resumen <- subset(junio2019Resumen, V2!= "NA")

junio2019Resumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
junio2019Resumen <- junio2019Resumen %>% select(Concepto, V2, V3, V4)
##ya viene splited
##junio2019SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(junio2019Resumen$V2), " ", fixed = TRUE)))
##junio2019SplitedResumen <- junio2019SplitedResumen %>% select(X1, X2)
##junio2019SplitedResumen$Concepto <- junio2019Resumen$Concepto

colnames(junio2019Resumen) <- c("Concepto", "2018", "2019", "Variación")
## junio2019SplitedResumen$Variacion <- 100 * (as.numeric(as.character(gsub(",","", junio2019SplitedResumen$"2019"))) - as.numeric(as.character(gsub(",","", junio2019SplitedResumen$"2018")))) / as.numeric(as.character(gsub(",","", junio2019SplitedResumen$"2019")))

## junio2019Resumen <- junio2019SplitedResumen

# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


junio2019SituacionFinanciera <- read.table(file = 'junio2019SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), fill=TRUE, skip='1')

#Dropping Nas in second colu,n
junio2019SituacionFinanciera <- subset(junio2019SituacionFinanciera, V2!= "Partidas informativas")

#junio2019SituacionFinanciera <- slice(junio2019SituacionFinanciera, -c(1, 2, 3, 4, 5))

#junio2019SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(junio2019SituacionFinanciera$V2), " ", fixed = TRUE)))




#Renombrar columnas
junio2019SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

junio2019SituacionFinanciera <- junio2019SituacionFinanciera  %>%
  select(V5, V8, V11, Concepto)
colnames(junio2019SituacionFinanciera) <- c("2018","2019", "Crecimiento real", "Concepto")


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
junio2019DeudaInterna <- slice(junio2019DeudaInterna, -c(1, 2, 3, 4))
## colnames(junio2019DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
 junio2019SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(junio2019DeudaInterna$V3), " ", fixed = TRUE)))
junio2019SplitedDeudaInterna$V1 <- junio2019DeudaInterna$V1
 junio2019SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", junio2019SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", junio2019SplitedDeudaInterna$X1)))
 colnames(junio2019SplitedDeudaInterna) <- c("Comisiones", "Total", "Concepto", "Intereses")
junio2019DeudaInterna <- junio2019SplitedDeudaInterna
# Select only v2
junio2019DeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")
## Cambiar columna de "concepto"

##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
 junio2019DeudaExterna <- slice(junio2019DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
 junio2019SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(junio2019DeudaExterna$V3), " ", fixed = TRUE)))
 junio2019SplitedDeudaExterna$V1 <- junio2019DeudaExterna$V1

##Join
junio2019MergedDeudaExterna <- merge(junio2019DeudaExterna, junio2019SplitedDeudaExterna, by = "V1")

junio2019MergedDeudaExterna <- junio2019MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
junio2019DeudaExterna <- junio2019MergedDeudaExterna  
colnames(junio2019DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(junio2019Resumen, file = "junio2019Resumen.csv", fileEncoding = "UTF-8")
write.csv(junio2019SituacionFinanciera, file ="junio2019SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(junio2019DeudaExterna, file ="junio2019DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(junio2019DeudaInterna, file ="junio2019DeudaInterna.csv", fileEncoding = "UTF-8")

