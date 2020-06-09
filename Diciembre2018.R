## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
diciembre2018Resumen <- read.table(file = 'diciembre2018Resumen.tsv', sep = '\t', header = FALSE, na.strings=c(""," ","NA"))
diciembre2018Resumen <- slice(diciembre2018Resumen, -c(1,2,3))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



diciembre2018DeudaInterna <- read.table(file = 'diciembre2018DeudaInterna.tsv', sep = '\t', header = FALSE)
diciembre2018DeudaExterna <- read.table(file = 'diciembre2018DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
diciembre2018Resumen <- subset(diciembre2018Resumen, V2!= "NA")
diciembre2018Resumen <- subset(diciembre2018Resumen, V4!= "NA")

## slice again
diciembre2018Resumen <- slice(diciembre2018Resumen, -c(28))

diciembre2018Resumen <- diciembre2018Resumen %>% select(V2,V3,V4,V5)
colnames(diciembre2018Resumen) <- c("Concepto", "2017", "V", "Crecimiento")
diciembre2018Resumen <- subset(diciembre2018Resumen, V!= "NA")
colnames(diciembre2018Resumen) <- c("Concepto", "2017", "2018", "Crecimiento")
diciembre2018Resumen <- subset(diciembre2018Resumen, Concepto!= "NA")
colnames(diciembre2018Resumen) <- c("Concepto", "V", "2018", "Crecimiento")
diciembre2018Resumen <- subset(diciembre2018Resumen, V!= "NA")
colnames(diciembre2018Resumen) <- c("Concepto", "2017", "2018", "Crecimiento")


diciembre2018Resumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
diciembre2018Resumen <- diciembre2018Resumen %>% select(Concepto, V2, V3)
## no viene splited
##diciembre2018SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(diciembre2018Resumen$V2), " ", fixed = TRUE)))
##diciembre2018SplitedResumen <- diciembre2018SplitedResumen %>% select(X1, X2)
##diciembre2018SplitedResumen$Concepto <- diciembre2018Resumen$Concepto

##colnames(diciembre2018SplitedResumen) <- c("2018", "2019", "Concepto")
diciembre2018Resumen$Variacion <- 100 * (as.numeric(as.character(gsub(",","", diciembre2018Resumen$V3))) - as.numeric(as.character(gsub(",","", diciembre2018Resumen$V2)))) / as.numeric(as.character(gsub(",","", diciembre2018Resumen$V3)))
colnames(diciembre2018Resumen) <- c("Concepto", "2017", "2018", "Variacion")

##diciembre2018Resumen <- diciembre2018SplitedResumen

# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


diciembre2018SituacionFinanciera <- read.table(file = 'diciembre2018SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), fill=TRUE, skip='1')

#Dropping Nas in second colu,n
diciembre2018SituacionFinanciera <- subset(diciembre2018SituacionFinanciera, V2!= "Partidas informativas")

diciembre2018SituacionFinanciera <- slice(diciembre2018SituacionFinanciera, -c(1, 2, 3, 4, 5))

diciembre2018SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(diciembre2018SituacionFinanciera$V2), " ", fixed = TRUE)))




#Renombrar columnas
diciembre2018SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

diciembre2018SituacionFinanciera <- diciembre2018SituacionFinanciera  %>%
  select(X1, X2, X3, Concepto)
colnames(diciembre2018SituacionFinanciera) <- c("2017","2018", "Crecimiento real", "Concepto")
##Split situacion financiera
#diciembre2018SplitedSituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(diciembre2018SituacionFinanciera$V2), " ", fixed = TRUE)))
#diciembre2018SplitedSituacionFinanciera$Concepto <- diciembre2018SituacionFinanciera$Concepto
#diciembre2018SplitedSituacionFinanciera <- diciembre2018SplitedSituacionFinanciera %>% select(X1,X2,Concepto)
#diciembre2018SplitedSituacionFinanciera$Variacion <- 100 * (as.numeric(as.character(gsub(",","", diciembre2018SplitedSituacionFinanciera$"X2"))) - as.numeric(as.character(gsub(",","", diciembre2018SplitedSituacionFinanciera$"X1")))) / as.numeric(as.character(gsub(",","", diciembre2018SplitedSituacionFinanciera$"X2")))
#colnames(diciembre2018SplitedSituacionFinanciera) <- c("2017","2018", "Concepto", "Variacion")

#diciembre2018SituacionFinanciera <- diciembre2018SplitedSituacionFinanciera

# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
##diciembre2018DeudaInterna <- slice(diciembre2018DeudaInterna, -c(1, 2, 3, 4))


#colnames(diciembre2018DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
#diciembre2018SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(diciembre2018DeudaInterna$V3), " ", fixed = TRUE)))
#diciembre2018SplitedDeudaInterna$V1 <- diciembre2018DeudaInterna$V1
#diciembre2018SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", diciembre2018SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", diciembre2018SplitedDeudaInterna$X1)))

colnames(diciembre2018DeudaInterna) <- c("Concepto", "Intereses", "Comisiones", "Total")

# Select only v2
diciembre2018DeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")

#Select and change colnames
#diciembre2018DeudaInterna <- diciembre2018DeudaInterna %>% select(Concepto,V2,V3,V4)
#colnames(diciembre2018DeudaInterna) <- c("Concepto", "Intereses", "Comisiones", "Total")

## Cambiar columna de "concepto"
#diciembre2018DeudaInterna <- diciembre2018SplitedDeudaInterna
##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
diciembre2018DeudaExterna <- slice(diciembre2018DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
diciembre2018SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(diciembre2018DeudaExterna$V3), " ", fixed = TRUE)))
diciembre2018SplitedDeudaExterna$V1 <- diciembre2018DeudaExterna$V1

##Join
diciembre2018MergedDeudaExterna <- merge(diciembre2018DeudaExterna, diciembre2018SplitedDeudaExterna, by = "V1")

diciembre2018MergedDeudaExterna <- diciembre2018MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
diciembre2018DeudaExterna <- diciembre2018MergedDeudaExterna  
colnames(diciembre2018DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(diciembre2018Resumen, file = "diciembre2018Resumen.csv", fileEncoding = "UTF-8")
write.csv(diciembre2018SituacionFinanciera, file ="diciembre2018SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(diciembre2018DeudaExterna, file ="diciembre2018DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(diciembre2018DeudaInterna, file ="diciembre2018DeudaInterna.csv", fileEncoding = "UTF-8")

