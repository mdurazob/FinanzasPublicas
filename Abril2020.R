## Scrapear información financiera a través de un TSV obtenido en Python

abril2020Resumen <- read.table(file = 'abril2020Resumen.tsv', sep = '\t', header = TRUE)
abril2020Resumen <- slice(abril2020Resumen, -c(1, 2, 3, 4, 5))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



abril2020DeudaInterna <- read.table(file = 'abril2020DeudaInterna.tsv', sep = '\t', header = FALSE, fill = TRUE, na.strings=c(""," ","NA"))
abril2020DeudaExterna <- read.table(file = 'abril2020DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
abril2020SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(abril2020Resumen$Enero.abril), " ", fixed = TRUE)))
abril2020SplitedResumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
colnames(abril2020SplitedResumen) <- c("2019","2020", "Variacion", "Concepto")

abril2020Resumen <- abril2020SplitedResumen
# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


abril2020SituacionFinanciera <- read.table(file = 'abril2020SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"))

#Dropping Nas in second colu,n
abril2020SituacionFinanciera <- subset(abril2020SituacionFinanciera, V2!= "NA")
abril2020SituacionFinanciera <- subset(abril2020SituacionFinanciera, V2!= "Partidas informativas")

abril2020SituacionFinanciera <- abril2020SituacionFinanciera %>%
  select(V2, V5, V8, V11)


#Renombrar columnas
colnames(abril2020SituacionFinanciera) <- c("Concepto","2019", "2020", "Crecimiento real")
## llenar columna de concepto adecuadamente
abril2020SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna 
## Quitar filas innecesarias
## Split
abril2020DeudaInterna <- slice(abril2020DeudaInterna, -c(1, 2, 3, 4))
abril2020SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(abril2020DeudaInterna$V3), " ", fixed = TRUE)))
abril2020SplitedDeudaInterna <- abril2020SplitedDeudaInterna %>% select (X1, X2)
## remove nas and certain rows
abril2020SplitedDeudaInterna <- abril2020SplitedDeudaInterna %>% filter(!is.na(X1))
abril2020SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", abril2020SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", abril2020SplitedDeudaInterna$X1)))
abril2020SplitedDeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")


abril2020DeudaInterna <- abril2020SplitedDeudaInterna
colnames(abril2020DeudaInterna) <- c("Comisiones","Total", "Intereses", "Concepto")


##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
abril2020DeudaExterna <- slice(abril2020DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
abril2020SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(abril2020DeudaExterna$V3), " ", fixed = TRUE)))
abril2020SplitedDeudaExterna$V1 <- abril2020DeudaExterna$V1

##Join
abril2020MergedDeudaExterna <- merge(abril2020DeudaExterna, abril2020SplitedDeudaExterna, by = "V1")

abril2020MergedDeudaExterna <- abril2020MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
abril2020DeudaExterna <- abril2020MergedDeudaExterna  
colnames(abril2020DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")

## Exportar cada tabla como csv
write.csv(abril2020Resumen, file = "abril2020Resumen.csv", fileEncoding = "UTF-8")
write.csv(abril2020SituacionFinanciera, file ="abril2020SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(abril2020DeudaExterna, file ="abril2020DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(abril2020DeudaInterna, file ="abril2020DeudaInterna.csv", fileEncoding = "UTF-8")

