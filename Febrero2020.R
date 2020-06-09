## Scrapear información financiera a través de un TSV obtenido en Python

febrero2020Resumen <- read.table(file = 'febrero2020Resumen.tsv', sep = '\t', header = TRUE)
febrero2020Resumen <- slice(febrero2020Resumen, -c(1, 2))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



febrero2020DeudaInterna <- read.table(file = 'febrero2020DeudaInterna.tsv', sep = '\t', header = FALSE, fill = TRUE, na.strings=c(""," ","NA"))
febrero2020DeudaExterna <- read.table(file = 'febrero2020DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
febrero2020SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(febrero2020Resumen$Enero.febrero), " ", fixed = TRUE)))
febrero2020SplitedResumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
#Variacion
febrero2020SplitedResumen$Variacion <- as.numeric(as.character(gsub(",","", febrero2020SplitedResumen$X1))) / as.numeric(as.character(gsub(",","", febrero2020SplitedResumen$X2)))
colnames(febrero2020SplitedResumen) <- c("2019","2020", "Concepto", "Variacion")
febrero2020Resumen <- febrero2020SplitedResumen
# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


febrero2020SituacionFinanciera <- read.table(file = 'febrero2020SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"))

#Dropping Nas in second colu,n
febrero2020SituacionFinanciera <- subset(febrero2020SituacionFinanciera, V2!= "NA")
febrero2020SituacionFinanciera <- subset(febrero2020SituacionFinanciera, V2!= "Partidas informativas")

febrero2020SituacionFinanciera <- febrero2020SituacionFinanciera %>%
  select(V2, V5, V8, V11)


#Renombrar columnas
colnames(febrero2020SituacionFinanciera) <- c("Concepto","2019", "2020", "Crecimiento real")
## llenar columna de concepto adecuadamente
febrero2020SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna 
## Quitar filas innecesarias
## Split
febrero2020DeudaInterna <- slice(febrero2020DeudaInterna, -c(1, 2, 3, 4))
febrero2020SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(febrero2020DeudaInterna$V3), " ", fixed = TRUE)))
febrero2020SplitedDeudaInterna <- febrero2020SplitedDeudaInterna %>% select (X1, X2)
## remove nas and certain rows
febrero2020SplitedDeudaInterna <- febrero2020SplitedDeudaInterna %>% filter(!is.na(X1))
febrero2020SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", febrero2020SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", febrero2020SplitedDeudaInterna$X1)))
febrero2020SplitedDeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")


febrero2020DeudaInterna <- febrero2020SplitedDeudaInterna
colnames(febrero2020DeudaInterna) <- c("Comisiones","Total", "Intereses", "Concepto")


##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
febrero2020DeudaExterna <- slice(febrero2020DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
febrero2020SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(febrero2020DeudaExterna$V3), " ", fixed = TRUE)))
febrero2020SplitedDeudaExterna$V1 <- febrero2020DeudaExterna$V1

##Join
febrero2020MergedDeudaExterna <- merge(febrero2020DeudaExterna, febrero2020SplitedDeudaExterna, by = "V1")

febrero2020MergedDeudaExterna <- febrero2020MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
febrero2020DeudaExterna <- febrero2020MergedDeudaExterna  
colnames(febrero2020DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")

## Exportar cada tabla como csv
write.csv(febrero2020Resumen, file = "febrero2020Resumen.csv", fileEncoding = "UTF-8")
write.csv(febrero2020SituacionFinanciera, file ="febrero2020SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(febrero2020DeudaExterna, file ="febrero2020DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(febrero2020DeudaInterna, file ="febrero2020DeudaInterna.csv", fileEncoding = "UTF-8")

