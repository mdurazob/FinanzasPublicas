## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
noviembre2018Resumen <- read.table(file = 'noviembre2018Resumen.tsv', sep = '\t', header = FALSE, na.strings=c(""," ","NA"), fill=TRUE)
noviembre2018Resumen <- slice(noviembre2018Resumen, -c(1,2,3))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



noviembre2018DeudaInterna <- read.table(file = 'noviembre2018DeudaInterna.tsv', sep = '\t', header = FALSE)
noviembre2018DeudaExterna <- read.table(file = 'noviembre2018DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
noviembre2018Resumen <- subset(noviembre2018Resumen, V2!= "NA")
noviembre2018Resumen <- subset(noviembre2018Resumen, V4!= "NA")

## slice again
noviembre2018Resumen <- slice(noviembre2018Resumen, -c(28))

noviembre2018Resumen <- noviembre2018Resumen %>% select(V2,V3,V4,V5)
colnames(noviembre2018Resumen) <- c("Concepto", "2017", "V", "Crecimiento")
noviembre2018Resumen <- subset(noviembre2018Resumen, V!= "NA")
colnames(noviembre2018Resumen) <- c("Concepto", "2017", "2018", "Crecimiento")
noviembre2018Resumen <- subset(noviembre2018Resumen, Concepto!= "NA")
colnames(noviembre2018Resumen) <- c("Concepto", "V", "2018", "Crecimiento")
noviembre2018Resumen <- subset(noviembre2018Resumen, V!= "NA")
colnames(noviembre2018Resumen) <- c("Concepto", "2017", "2018", "Crecimiento")


noviembre2018Resumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
##noviembre2018Resumen <- noviembre2018Resumen %>% select(Concepto, V2, V3, V4)
## no viene splited
##noviembre2018SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(noviembre2018Resumen$V2), " ", fixed = TRUE)))
##noviembre2018SplitedResumen <- noviembre2018SplitedResumen %>% select(X1, X2)
##noviembre2018SplitedResumen$Concepto <- noviembre2018Resumen$Concepto

##colnames(noviembre2018SplitedResumen) <- c("2018", "2019", "Concepto")
##noviembre2018SplitedResumen$Variacion <- 100 * (as.numeric(as.character(gsub(",","", noviembre2018SplitedResumen$"2019"))) - as.numeric(as.character(gsub(",","", noviembre2018SplitedResumen$"2018")))) / as.numeric(as.character(gsub(",","", noviembre2018SplitedResumen$"2019")))

##noviembre2018Resumen <- noviembre2018SplitedResumen

# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


noviembre2018SituacionFinanciera <- read.table(file = 'noviembre2018SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), fill=TRUE, skip='1')

#Dropping Nas in second colu,n
noviembre2018SituacionFinanciera <- subset(noviembre2018SituacionFinanciera, V2!= "Partidas informativas")

noviembre2018SituacionFinanciera <- slice(noviembre2018SituacionFinanciera, -c(1, 2, 3, 4, 5))

#noviembre2018SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(noviembre2018SituacionFinanciera$V2), " ", fixed = TRUE)))




#Renombrar columnas
noviembre2018SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

noviembre2018SituacionFinanciera <- noviembre2018SituacionFinanciera  %>%
  select(V5, V8, V11, Concepto)
colnames(noviembre2018SituacionFinanciera) <- c("2018","2019", "Crecimiento real", "Concepto")
##Split situacion financiera
noviembre2018SplitedSituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(noviembre2018SituacionFinanciera$V2), " ", fixed = TRUE)))
noviembre2018SplitedSituacionFinanciera$Concepto <- noviembre2018SituacionFinanciera$Concepto
noviembre2018SplitedSituacionFinanciera <- noviembre2018SplitedSituacionFinanciera %>% select(X1,X2,Concepto)
noviembre2018SplitedSituacionFinanciera$Variacion <- 100 * (as.numeric(as.character(gsub(",","", noviembre2018SplitedSituacionFinanciera$"X2"))) - as.numeric(as.character(gsub(",","", noviembre2018SplitedSituacionFinanciera$"X1")))) / as.numeric(as.character(gsub(",","", noviembre2018SplitedSituacionFinanciera$"X2")))
colnames(noviembre2018SplitedSituacionFinanciera) <- c("2017","2018", "Concepto", "Variacion")

noviembre2018SituacionFinanciera <- noviembre2018SplitedSituacionFinanciera

# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
##noviembre2018DeudaInterna <- slice(noviembre2018DeudaInterna, -c(1, 2, 3, 4))


#colnames(noviembre2018DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
#noviembre2018SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(noviembre2018DeudaInterna$V3), " ", fixed = TRUE)))
#noviembre2018SplitedDeudaInterna$V1 <- noviembre2018DeudaInterna$V1
#noviembre2018SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", noviembre2018SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", noviembre2018SplitedDeudaInterna$X1)))

colnames(noviembre2018DeudaInterna) <- c("Concepto", "Intereses", "Comisiones", "Total")

# Select only v2
noviembre2018DeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")

#Select and change colnames
noviembre2018DeudaInterna <- noviembre2018DeudaInterna %>% select(Concepto,V2,V3,V4)
colnames(noviembre2018DeudaInterna) <- c("Concepto", "Intereses", "Comisiones", "Total")

## Cambiar columna de "concepto"
#noviembre2018DeudaInterna <- noviembre2018SplitedDeudaInterna
##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
noviembre2018DeudaExterna <- slice(noviembre2018DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
noviembre2018SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(noviembre2018DeudaExterna$V3), " ", fixed = TRUE)))
noviembre2018SplitedDeudaExterna$V1 <- noviembre2018DeudaExterna$V1

##Join
noviembre2018MergedDeudaExterna <- merge(noviembre2018DeudaExterna, noviembre2018SplitedDeudaExterna, by = "V1")

noviembre2018MergedDeudaExterna <- noviembre2018MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
noviembre2018DeudaExterna <- noviembre2018MergedDeudaExterna  
colnames(noviembre2018DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(noviembre2018Resumen, file = "noviembre2018Resumen.csv", fileEncoding = "UTF-8")
write.csv(noviembre2018SituacionFinanciera, file ="noviembre2018SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(noviembre2018DeudaExterna, file ="noviembre2018DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(noviembre2018DeudaInterna, file ="noviembre2018DeudaInterna.csv", fileEncoding = "UTF-8")

