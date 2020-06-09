## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
octubre2019Resumen <- read.table(file = 'octubre2019Resumen.tsv', sep = '\t', header = FALSE, na.strings=c(""," ","NA"))
octubre2019Resumen <- slice(octubre2019Resumen, -c(1))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



octubre2019DeudaInterna <- read.table(file = 'octubre2019DeudaInterna.tsv', sep = '\t', header = FALSE)
octubre2019DeudaExterna <- read.table(file = 'octubre2019DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
octubre2019Resumen <- subset(octubre2019Resumen, V2!= "NA")

octubre2019Resumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
octubre2019Resumen <- octubre2019Resumen %>% select(Concepto, V2, V3, V4)
colnames(octubre2019Resumen) <- c("Concepto", "2019", "2018", "Variacion")


# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


octubre2019SituacionFinanciera <- read.table(file = 'octubre2019SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), fill=TRUE, skip='1')

#Dropping Nas in second colu,n
## octubre2019SituacionFinanciera <- slice(octubre2019SituacionFinanciera, -c(1, 2, 3))

octubre2019SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(octubre2019SituacionFinanciera$V2), " ", fixed = TRUE)))

octubre2019SituacionFinanciera <- subset(octubre2019SituacionFinanciera, V2!= "NA")

octubre2019SituacionFinanciera <- subset(octubre2019SituacionFinanciera, V2!= "Partidas informativas")


#Renombrar columnas
octubre2019SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

octubre2019SituacionFinanciera <- octubre2019SituacionFinanciera  %>%
  select(V5, V8, V11, Concepto)
colnames(octubre2019SituacionFinanciera) <- c("2018","2019", "Crecimiento real", "Concepto")


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
octubre2019DeudaInterna <- slice(octubre2019DeudaInterna, -c(1, 2, 3, 4, 5))
colnames(octubre2019DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
## octubre2019SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(octubre2019DeudaInterna$V3), " ", fixed = TRUE)))
## octubre2019SplitedDeudaInterna$V1 <- octubre2019DeudaInterna$V1
##  octubre2019SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", octubre2019SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", octubre2019SplitedDeudaInterna$X1)))
## colnames(octubre2019SplitedDeudaInterna) <- c("Comisiones", "Total", "Concepto", "Intereses")
## octubre2019DeudaInterna <- octubre2019DeudaInterna %>% select(V1,V2)
# Select only v2
octubre2019DeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")
## Cambiar columna de "concepto"

##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
 octubre2019DeudaExterna <- slice(octubre2019DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
 octubre2019SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(octubre2019DeudaExterna$V3), " ", fixed = TRUE)))
 octubre2019SplitedDeudaExterna$V1 <- octubre2019DeudaExterna$V1

##Join
octubre2019MergedDeudaExterna <- merge(octubre2019DeudaExterna, octubre2019SplitedDeudaExterna, by = "V1")

octubre2019MergedDeudaExterna <- octubre2019MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
octubre2019DeudaExterna <- octubre2019MergedDeudaExterna  
colnames(octubre2019DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(octubre2019Resumen, file = "octubre2019Resumen.csv", fileEncoding = "UTF-8")
write.csv(octubre2019SituacionFinanciera, file ="octubre2019SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(octubre2019DeudaExterna, file ="octubre2019DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(octubre2019DeudaInterna, file ="octubre2019DeudaInterna.csv", fileEncoding = "UTF-8")
