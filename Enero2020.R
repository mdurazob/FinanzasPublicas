## Scrapear información financiera a través de un TSV obtenido en Python

enero2020Resumen <- read.table(file = 'enero2020Resumen.tsv', sep = '\t', header = TRUE)
enero2020Resumen <- slice(enero2020Resumen, -c(1, 2))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



enero2020DeudaInterna <- read.table(file = 'enero2020DeudaInterna.tsv', sep = '\t', header = FALSE, fill = TRUE, na.strings=c(""," ","NA"))
enero2020DeudaExterna <- read.table(file = 'enero2020DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
enero2020SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(enero2020Resumen$Enero), " ", fixed = TRUE)))
enero2020SplitedResumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
#Variacion
enero2020SplitedResumen$Variacion <- as.numeric(as.character(gsub(",","", enero2020SplitedResumen$X1))) / as.numeric(as.character(gsub(",","", enero2020SplitedResumen$X2)))
colnames(enero2020SplitedResumen) <- c("2019","2020", "Concepto", "Variacion")
enero2020Resumen <- enero2020SplitedResumen
# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


enero2020SituacionFinanciera <- read.table(file = 'enero2020SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"))

#Dropping Nas in second colu,n
enero2020SituacionFinanciera <- subset(enero2020SituacionFinanciera, Concepto!= "NA")
enero2020SituacionFinanciera <- subset(enero2020SituacionFinanciera, Concepto!= "artidas informativas")



## llenar columna de concepto adecuadamente
enero2020SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna 
## Quitar filas innecesarias
## Split
enero2020DeudaInterna <- slice(enero2020DeudaInterna, -c(1, 2, 3, 4, 5))
## enero2020SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(enero2020DeudaInterna$V3), " ", fixed = TRUE)))
## enero2020SplitedDeudaInterna <- enero2020SplitedDeudaInterna %>% select (X1, X2)
## remove nas and certain rows
## enero2020SplitedDeudaInterna <- enero2020SplitedDeudaInterna %>% filter(!is.na(X1))
## enero2020SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", enero2020SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", enero2020SplitedDeudaInterna$X1)))
## enero2020SplitedDeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")


## enero2020DeudaInterna <- enero2020SplitedDeudaInterna
colnames(enero2020DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")


##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
enero2020DeudaExterna <- slice(enero2020DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
enero2020SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(enero2020DeudaExterna$V3), " ", fixed = TRUE)))
enero2020SplitedDeudaExterna$V1 <- enero2020DeudaExterna$V1

##Join
enero2020MergedDeudaExterna <- merge(enero2020DeudaExterna, enero2020SplitedDeudaExterna, by = "V1")

enero2020MergedDeudaExterna <- enero2020MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
enero2020DeudaExterna <- enero2020MergedDeudaExterna  
colnames(enero2020DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")

## Exportar cada tabla como csv
write.csv(enero2020Resumen, file = "enero2020Resumen.csv", fileEncoding = "UTF-8")
write.csv(enero2020SituacionFinanciera, file ="enero2020SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(enero2020DeudaExterna, file ="enero2020DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(enero2020DeudaInterna, file ="enero2020DeudaInterna.csv", fileEncoding = "UTF-8")

