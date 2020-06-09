## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
noviembre2019Resumen <- read.table(file = 'noviembre2019Resumen.tsv', sep = '\t', header = TRUE)
noviembre2019Resumen <- slice(noviembre2019Resumen, -c(1, 2, 3, 4, 5, 6))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



noviembre2019DeudaInterna <- read.table(file = 'noviembre2019DeudaInterna.tsv', sep = '\t', header = FALSE)
noviembre2019DeudaExterna <- read.table(file = 'noviembre2019DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
noviembre2019SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(noviembre2019Resumen$Enero.noviembre), " ", fixed = TRUE)))
noviembre2019SplitedResumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
colnames(noviembre2019SplitedResumen) <- c("2018", "2019", "Concepto")

noviembre2019Resumen <- noviembre2019SplitedResumen
# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


noviembre2019SituacionFinanciera <- read.table(file = 'noviembre2019SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), skip = '4')

#Dropping Nas in second colu,n
noviembre2019SituacionFinanciera <- slice(noviembre2019SituacionFinanciera, -c(1, 2, 3))

noviembre2019SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(noviembre2019SituacionFinanciera$V2), " ", fixed = TRUE)))

noviembre2019SituacionFinanciera <- subset(noviembre2019SituacionFinanciera, X1!= "NA")



#Renombrar columnas
noviembre2019SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

noviembre2019SituacionFinanciera <- noviembre2019SituacionFinanciera  %>%
  select(X1, X2, X3, Concepto)
colnames(noviembre2019SituacionFinanciera) <- c("2018","2019", "Crecimiento real", "Concepto")


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
noviembre2019DeudaInterna <- slice(noviembre2019DeudaInterna, -c(1, 2, 3, 4))
colnames(noviembre2019DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
noviembre2019SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(noviembre2019DeudaInterna$V3), " ", fixed = TRUE)))
noviembre2019SplitedDeudaInterna$V1 <- noviembre2019DeudaInterna$V1
noviembre2019SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", noviembre2019SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", noviembre2019SplitedDeudaInterna$X1)))
colnames(noviembre2019SplitedDeudaInterna) <- c("Comisiones", "Total", "Concepto", "Intereses")
## noviembre2019DeudaInterna <- noviembre2019DeudaInterna %>% select(V1,V2)
# Select only v2
noviembre2019SplitedDeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")
noviembre2019DeudaInterna <- noviembre2019SplitedDeudaInterna 
## Cambiar columna de "concepto"

##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
noviembre2019DeudaExterna <- slice(noviembre2019DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
noviembre2019SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(noviembre2019DeudaExterna$V3), " ", fixed = TRUE)))
noviembre2019SplitedDeudaExterna$V1 <- noviembre2019DeudaExterna$V1

##Join
noviembre2019MergedDeudaExterna <- merge(noviembre2019DeudaExterna, noviembre2019SplitedDeudaExterna, by = "V1")

noviembre2019MergedDeudaExterna <- noviembre2019MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
noviembre2019DeudaExterna <- noviembre2019MergedDeudaExterna  
colnames(noviembre2019DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(noviembre2019Resumen, file = "noviembre2019Resumen.csv", fileEncoding = "UTF-8")
write.csv(noviembre2019SituacionFinanciera, file ="noviembre2019SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(noviembre2019DeudaExterna, file ="noviembre2019DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(noviembre2019DeudaInterna, file ="noviembre2019DeudaInterna.csv", fileEncoding = "UTF-8")
