## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
marzo2019Resumen <- read.table(file = 'marzo2019Resumen.tsv', sep = '\t', header = FALSE, na.strings=c(""," ","NA"))
## marzo2019Resumen <- slice(marzo2019Resumen, -c(1,2,3,4,5,6,7))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



marzo2019DeudaInterna <- read.table(file = 'marzo2019DeudaInterna.tsv', sep = '\t', header = FALSE)
marzo2019DeudaExterna <- read.table(file = 'marzo2019DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
marzo2019Resumen <- subset(marzo2019Resumen, V2!= "NA")

marzo2019Resumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
marzo2019Resumen <- marzo2019Resumen %>% select(Concepto, V2, V3, V4)
##ya viene splited
##marzo2019SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(marzo2019Resumen$V2), " ", fixed = TRUE)))
##marzo2019SplitedResumen <- marzo2019SplitedResumen %>% select(X1, X2)
##marzo2019SplitedResumen$Concepto <- marzo2019Resumen$Concepto

colnames(marzo2019Resumen) <- c("Concepto", "2018", "2019", "Variación")
## marzo2019SplitedResumen$Variacion <- 100 * (as.numeric(as.character(gsub(",","", marzo2019SplitedResumen$"2019"))) - as.numeric(as.character(gsub(",","", marzo2019SplitedResumen$"2018")))) / as.numeric(as.character(gsub(",","", marzo2019SplitedResumen$"2019")))

## marzo2019Resumen <- marzo2019SplitedResumen

# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


marzo2019SituacionFinanciera <- read.table(file = 'marzo2019SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), fill=TRUE, skip='1')

#Dropping Nas in second colu,n
marzo2019SituacionFinanciera <- subset(marzo2019SituacionFinanciera, V2!= "Partidas informativas")

#marzo2019SituacionFinanciera <- slice(marzo2019SituacionFinanciera, -c(1, 2, 3, 4, 5))

#marzo2019SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(marzo2019SituacionFinanciera$V2), " ", fixed = TRUE)))




#Renombrar columnas
marzo2019SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

marzo2019SituacionFinanciera <- marzo2019SituacionFinanciera  %>%
  select(V5, V8, V11, Concepto)
colnames(marzo2019SituacionFinanciera) <- c("2018","2019", "Crecimiento real", "Concepto")


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
marzo2019DeudaInterna <- slice(marzo2019DeudaInterna, -c(1, 2, 3, 4, 5))


## colnames(marzo2019DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
##marzo2019SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(marzo2019DeudaInterna$V3), " ", fixed = TRUE)))
##marzo2019SplitedDeudaInterna$V1 <- marzo2019DeudaInterna$V1
##marzo2019SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", marzo2019SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", marzo2019SplitedDeudaInterna$X1)))
#marzo2019DeudaInterna <- marzo2019SplitedDeudaInterna %>%
  ##select(Concepto, V2, V3, V4)

colnames(marzo2019DeudaInterna) <- c("Concepto", "Intereses", "Comisiones", "Total")

# Select only v2
marzo2019DeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")
## Cambiar columna de "concepto"

##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
 marzo2019DeudaExterna <- slice(marzo2019DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
 marzo2019SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(marzo2019DeudaExterna$V3), " ", fixed = TRUE)))
 marzo2019SplitedDeudaExterna$V1 <- marzo2019DeudaExterna$V1

##Join
marzo2019MergedDeudaExterna <- merge(marzo2019DeudaExterna, marzo2019SplitedDeudaExterna, by = "V1")

marzo2019MergedDeudaExterna <- marzo2019MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
marzo2019DeudaExterna <- marzo2019MergedDeudaExterna  
colnames(marzo2019DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(marzo2019Resumen, file = "marzo2019Resumen.csv", fileEncoding = "UTF-8")
write.csv(marzo2019SituacionFinanciera, file ="marzo2019SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(marzo2019DeudaExterna, file ="marzo2019DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(marzo2019DeudaInterna, file ="marzo2019DeudaInterna.csv", fileEncoding = "UTF-8")

