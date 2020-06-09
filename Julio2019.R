## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
julio2019Resumen <- read.table(file = 'julio2019Resumen.tsv', sep = '\t', header = FALSE, na.strings=c(""," ","NA"))
julio2019Resumen <- slice(julio2019Resumen, -c(1,2,3,4,5,6,7))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



julio2019DeudaInterna <- read.table(file = 'julio2019DeudaInterna.tsv', sep = '\t', header = FALSE)
julio2019DeudaExterna <- read.table(file = 'julio2019DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
julio2019Resumen <- subset(julio2019Resumen, V2!= "NA")

julio2019Resumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
julio2019Resumen <- julio2019Resumen %>% select(Concepto, V2, V5)
##split
julio2019SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(julio2019Resumen$V2), " ", fixed = TRUE)))
julio2019SplitedResumen <- julio2019SplitedResumen %>% select(X1, X2)
julio2019SplitedResumen$Concepto <- julio2019Resumen$Concepto

colnames(julio2019SplitedResumen) <- c("2018", "2019", "Concepto")
julio2019SplitedResumen$Variacion <- 100 * (as.numeric(as.character(gsub(",","", julio2019SplitedResumen$"2019"))) - as.numeric(as.character(gsub(",","", julio2019SplitedResumen$"2018")))) / as.numeric(as.character(gsub(",","", julio2019SplitedResumen$"2019")))

julio2019Resumen <- julio2019SplitedResumen

# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


julio2019SituacionFinanciera <- read.table(file = 'julio2019SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), fill=TRUE, skip='1')

#Dropping Nas in second colu,n
julio2019SituacionFinanciera <- subset(julio2019SituacionFinanciera, V2!= "Partidas informativas")

#julio2019SituacionFinanciera <- slice(julio2019SituacionFinanciera, -c(1, 2, 3, 4, 5))

#julio2019SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(julio2019SituacionFinanciera$V2), " ", fixed = TRUE)))




#Renombrar columnas
julio2019SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

julio2019SituacionFinanciera <- julio2019SituacionFinanciera  %>%
  select(V5, V8, V11, Concepto)
colnames(julio2019SituacionFinanciera) <- c("2018","2019", "Crecimiento real", "Concepto")


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
julio2019DeudaInterna <- slice(julio2019DeudaInterna, -c(1, 2, 3, 4))
## colnames(julio2019DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
 julio2019SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(julio2019DeudaInterna$V3), " ", fixed = TRUE)))
julio2019SplitedDeudaInterna$V1 <- julio2019DeudaInterna$V1
 julio2019SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", julio2019SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", julio2019SplitedDeudaInterna$X1)))
 colnames(julio2019SplitedDeudaInterna) <- c("Comisiones", "Total", "Concepto", "Intereses")
julio2019DeudaInterna <- julio2019SplitedDeudaInterna
# Select only v2
julio2019DeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")
## Cambiar columna de "concepto"

##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
 julio2019DeudaExterna <- slice(julio2019DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
 julio2019SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(julio2019DeudaExterna$V3), " ", fixed = TRUE)))
 julio2019SplitedDeudaExterna$V1 <- julio2019DeudaExterna$V1

##Join
julio2019MergedDeudaExterna <- merge(julio2019DeudaExterna, julio2019SplitedDeudaExterna, by = "V1")

julio2019MergedDeudaExterna <- julio2019MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
julio2019DeudaExterna <- julio2019MergedDeudaExterna  
colnames(julio2019DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(julio2019Resumen, file = "julio2019Resumen.csv", fileEncoding = "UTF-8")
write.csv(julio2019SituacionFinanciera, file ="julio2019SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(julio2019DeudaExterna, file ="julio2019DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(julio2019DeudaInterna, file ="julio2019DeudaInterna.csv", fileEncoding = "UTF-8")

