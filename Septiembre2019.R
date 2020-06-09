## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
septiembre2019Resumen <- read.table(file = 'septiembre2019Resumen.tsv', sep = '\t', header = FALSE, na.strings=c(""," ","NA"))
septiembre2019Resumen <- slice(septiembre2019Resumen, -c(1,2,3,4,5,6,7))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



septiembre2019DeudaInterna <- read.table(file = 'septiembre2019DeudaInterna.tsv', sep = '\t', header = FALSE)
septiembre2019DeudaExterna <- read.table(file = 'septiembre2019DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
septiembre2019Resumen <- subset(septiembre2019Resumen, V2!= "NA")

septiembre2019Resumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
septiembre2019Resumen <- septiembre2019Resumen %>% select(Concepto, V2, V5)
##split
septiembre2019SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(septiembre2019Resumen$V2), " ", fixed = TRUE)))
septiembre2019SplitedResumen <- septiembre2019SplitedResumen %>% select(X1, X2)
septiembre2019SplitedResumen$Concepto <- septiembre2019Resumen$Concepto

colnames(septiembre2019SplitedResumen) <- c("2018", "2019", "Concepto")
septiembre2019SplitedResumen$Variacion <- 100 * (as.numeric(as.character(gsub(",","", septiembre2019SplitedResumen$"2019"))) - as.numeric(as.character(gsub(",","", septiembre2019SplitedResumen$"2018")))) / as.numeric(as.character(gsub(",","", septiembre2019SplitedResumen$"2019")))

septiembre2019Resumen <- septiembre2019SplitedResumen

# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


septiembre2019SituacionFinanciera <- read.table(file = 'septiembre2019SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), fill=TRUE, skip='1')

#Dropping Nas in second colu,n
septiembre2019SituacionFinanciera <- subset(septiembre2019SituacionFinanciera, V2!= "Partidas informativas")

septiembre2019SituacionFinanciera <- slice(septiembre2019SituacionFinanciera, -c(1, 2, 3, 4, 5))

septiembre2019SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(septiembre2019SituacionFinanciera$V2), " ", fixed = TRUE)))




#Renombrar columnas
septiembre2019SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

septiembre2019SituacionFinanciera <- septiembre2019SituacionFinanciera  %>%
  select(X1, X2, X3, Concepto)
colnames(septiembre2019SituacionFinanciera) <- c("2018","2019", "Crecimiento real", "Concepto")


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
septiembre2019DeudaInterna <- slice(septiembre2019DeudaInterna, -c(1, 2, 3, 4))
colnames(septiembre2019DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
 septiembre2019SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(septiembre2019DeudaInterna$V3), " ", fixed = TRUE)))
septiembre2019SplitedDeudaInterna$V1 <- septiembre2019DeudaInterna$V1
 septiembre2019SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", septiembre2019SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", septiembre2019SplitedDeudaInterna$X1)))
 colnames(septiembre2019SplitedDeudaInterna) <- c("Comisiones", "Total", "Concepto", "Intereses")
septiembre2019DeudaInterna <- septiembre2019SplitedDeudaInterna
# Select only v2
septiembre2019DeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")
## Cambiar columna de "concepto"

##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
 septiembre2019DeudaExterna <- slice(septiembre2019DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
 septiembre2019SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(septiembre2019DeudaExterna$V3), " ", fixed = TRUE)))
 septiembre2019SplitedDeudaExterna$V1 <- septiembre2019DeudaExterna$V1

##Join
septiembre2019MergedDeudaExterna <- merge(septiembre2019DeudaExterna, septiembre2019SplitedDeudaExterna, by = "V1")

septiembre2019MergedDeudaExterna <- septiembre2019MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
septiembre2019DeudaExterna <- septiembre2019MergedDeudaExterna  
colnames(septiembre2019DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(septiembre2019Resumen, file = "septiembre2019Resumen.csv", fileEncoding = "UTF-8")
write.csv(septiembre2019SituacionFinanciera, file ="septiembre2019SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(septiembre2019DeudaExterna, file ="septiembre2019DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(septiembre2019DeudaInterna, file ="septiembre2019DeudaInterna.csv", fileEncoding = "UTF-8")
