## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
octubre2018Resumen <- read.table(file = 'octubre2018Resumen.tsv', sep = '\t', header = FALSE, na.strings=c(""," ","NA"))
octubre2018Resumen <- slice(octubre2018Resumen, -c(1,2,3))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



octubre2018DeudaInterna <- read.table(file = 'octubre2018DeudaInterna.tsv', sep = '\t', header = FALSE)
octubre2018DeudaExterna <- read.table(file = 'octubre2018DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
octubre2018Resumen <- subset(octubre2018Resumen, V2!= "NA")
octubre2018Resumen <- subset(octubre2018Resumen, V4!= "NA")

## slice again
octubre2018Resumen <- slice(octubre2018Resumen, -c(28))

octubre2018Resumen <- octubre2018Resumen %>% select(V2,V3,V4,V5)
colnames(octubre2018Resumen) <- c("Concepto", "2017", "V", "Crecimiento")
octubre2018Resumen <- subset(octubre2018Resumen, V!= "NA")
colnames(octubre2018Resumen) <- c("Concepto", "2017", "2018", "Crecimiento")
octubre2018Resumen <- subset(octubre2018Resumen, Concepto!= "NA")
colnames(octubre2018Resumen) <- c("Concepto", "V", "2018", "Crecimiento")
octubre2018Resumen <- subset(octubre2018Resumen, V!= "NA")
colnames(octubre2018Resumen) <- c("Concepto", "2017", "2018", "Crecimiento")


octubre2018Resumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
octubre2018Resumen <- octubre2018Resumen %>% select(Concepto, V2, V3)
## no viene splited
##octubre2018SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(octubre2018Resumen$V2), " ", fixed = TRUE)))
##octubre2018SplitedResumen <- octubre2018SplitedResumen %>% select(X1, X2)
##octubre2018SplitedResumen$Concepto <- octubre2018Resumen$Concepto

##colnames(octubre2018SplitedResumen) <- c("2018", "2019", "Concepto")
octubre2018Resumen$Variacion <- 100 * (as.numeric(as.character(gsub(",","", octubre2018Resumen$V3))) - as.numeric(as.character(gsub(",","", octubre2018Resumen$V2)))) / as.numeric(as.character(gsub(",","", octubre2018Resumen$V3)))
colnames(octubre2018Resumen) <- c("Concepto", "2017", "2018", "Variacion")

##octubre2018Resumen <- octubre2018SplitedResumen

# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


octubre2018SituacionFinanciera <- read.table(file = 'octubre2018SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), fill=TRUE, skip='1')

#Dropping Nas in second colu,n
octubre2018SituacionFinanciera <- subset(octubre2018SituacionFinanciera, V2!= "Partidas informativas")

octubre2018SituacionFinanciera <- slice(octubre2018SituacionFinanciera, -c(1, 2, 3, 4, 5))

octubre2018SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(octubre2018SituacionFinanciera$V2), " ", fixed = TRUE)))




#Renombrar columnas
octubre2018SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

octubre2018SituacionFinanciera <- octubre2018SituacionFinanciera  %>%
  select(X1, X2, X3, Concepto)
colnames(octubre2018SituacionFinanciera) <- c("2017","2018", "Crecimiento real", "Concepto")
##Split situacion financiera
#octubre2018SplitedSituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(octubre2018SituacionFinanciera$V2), " ", fixed = TRUE)))
#octubre2018SplitedSituacionFinanciera$Concepto <- octubre2018SituacionFinanciera$Concepto
#octubre2018SplitedSituacionFinanciera <- octubre2018SplitedSituacionFinanciera %>% select(X1,X2,Concepto)
#octubre2018SplitedSituacionFinanciera$Variacion <- 100 * (as.numeric(as.character(gsub(",","", octubre2018SplitedSituacionFinanciera$"X2"))) - as.numeric(as.character(gsub(",","", octubre2018SplitedSituacionFinanciera$"X1")))) / as.numeric(as.character(gsub(",","", octubre2018SplitedSituacionFinanciera$"X2")))
#colnames(octubre2018SplitedSituacionFinanciera) <- c("2017","2018", "Concepto", "Variacion")

#octubre2018SituacionFinanciera <- octubre2018SplitedSituacionFinanciera

# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
##octubre2018DeudaInterna <- slice(octubre2018DeudaInterna, -c(1, 2, 3, 4))


#colnames(octubre2018DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
#octubre2018SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(octubre2018DeudaInterna$V3), " ", fixed = TRUE)))
#octubre2018SplitedDeudaInterna$V1 <- octubre2018DeudaInterna$V1
#octubre2018SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", octubre2018SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", octubre2018SplitedDeudaInterna$X1)))

colnames(octubre2018DeudaInterna) <- c("Concepto", "Intereses", "Comisiones", "Total")

# Select only v2
octubre2018DeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")

#Select and change colnames
#octubre2018DeudaInterna <- octubre2018DeudaInterna %>% select(Concepto,V2,V3,V4)
#colnames(octubre2018DeudaInterna) <- c("Concepto", "Intereses", "Comisiones", "Total")

## Cambiar columna de "concepto"
#octubre2018DeudaInterna <- octubre2018SplitedDeudaInterna
##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
octubre2018DeudaExterna <- slice(octubre2018DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
octubre2018SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(octubre2018DeudaExterna$V3), " ", fixed = TRUE)))
octubre2018SplitedDeudaExterna$V1 <- octubre2018DeudaExterna$V1

##Join
octubre2018MergedDeudaExterna <- merge(octubre2018DeudaExterna, octubre2018SplitedDeudaExterna, by = "V1")

octubre2018MergedDeudaExterna <- octubre2018MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
octubre2018DeudaExterna <- octubre2018MergedDeudaExterna  
colnames(octubre2018DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(octubre2018Resumen, file = "octubre2018Resumen.csv", fileEncoding = "UTF-8")
write.csv(octubre2018SituacionFinanciera, file ="octubre2018SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(octubre2018DeudaExterna, file ="octubre2018DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(octubre2018DeudaInterna, file ="octubre2018DeudaInterna.csv", fileEncoding = "UTF-8")

