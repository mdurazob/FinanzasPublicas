## Scrapear información financiera a través de un TSV obtenido en Python
library(dplyr)
enero2019Resumen <- read.table(file = 'enero2019Resumen.tsv', sep = '\t', header = FALSE, na.strings=c(""," ","NA"))
enero2019Resumen <- slice(enero2019Resumen, -c(1,2,3))

## Se necesita remover las primeras filas, viene "defectuoso"
# también remover lo innecersario



enero2019DeudaInterna <- read.table(file = 'enero2019DeudaInterna.tsv', sep = '\t', header = FALSE)
enero2019DeudaExterna <- read.table(file = 'enero2019DeudaExterna.tsv', sep = '\t', header = FALSE)

## Split Deuda Externa, Resumen

library(tidyr)
library(tibble)

## todo lo que se necesita de la 1ra pag (resumen de los principales indicadores)
enero2019Resumen <- subset(enero2019Resumen, V2!= "NA")

enero2019Resumen$Concepto <- c("Ingresos Presupuestarios","Ingresos Tributarios","Ingresos sin IEPS de combuestibles","Gasto neto total sin erogaciones en inversiones financieras, pago de pensiones, participaciones y costo financiero", "Gasto neto total sin erogaciones pero con costo financiero", "Gasto neto sin erogaciones en inversiones financieras", "Gasto neto total", "Gasto corriente estructural", "Balance primario", "RFSP", "SHRFSP", "Deuda pública")
## Resumen sólo abarca período "mes1-mesn", tal periodo en 2019 vs tal periodo en 2020
enero2019Resumen <- enero2019Resumen %>% select(Concepto, V2, V3, V4)
## no viene splited
enero2019SplitedResumen <- data.frame(do.call("rbind", strsplit(as.character(enero2019Resumen$V2), " ", fixed = TRUE)))
enero2019SplitedResumen <- enero2019SplitedResumen %>% select(X1, X2)
enero2019SplitedResumen$Concepto <- enero2019Resumen$Concepto

colnames(enero2019SplitedResumen) <- c("2018", "2019", "Concepto")
enero2019SplitedResumen$Variacion <- 100 * (as.numeric(as.character(gsub(",","", enero2019SplitedResumen$"2019"))) - as.numeric(as.character(gsub(",","", enero2019SplitedResumen$"2018")))) / as.numeric(as.character(gsub(",","", enero2019SplitedResumen$"2019")))

enero2019Resumen <- enero2019SplitedResumen

# El resumen ya está listo y guardado como mesAñoScrapedResumen ahora vamos con la situación financiera 


enero2019SituacionFinanciera <- read.table(file = 'enero2019SituacionFinanciera.tsv', sep = '\t', header = FALSE, na.strings=c("", "NA"), fill=TRUE, skip='1')

#Dropping Nas in second colu,n
enero2019SituacionFinanciera <- subset(enero2019SituacionFinanciera, V2!= "artidas informativas")

#enero2019SituacionFinanciera <- slice(enero2019SituacionFinanciera, -c(1, 2, 3, 4, 5))

#enero2019SituacionFinanciera <- data.frame(do.call("rbind", strsplit(as.character(enero2019SituacionFinanciera$V2), " ", fixed = TRUE)))




#Renombrar columnas
enero2019SituacionFinanciera$Concepto <- c("Ingresos Presupuestarios","Petroleros", "Petroleros_GobFed","Petroleros_Pemex", "NoPetroleros", "NoPetroleros_GobFed", "NoPetroleros_GobFed_Tributarios", "NoPetroleros_GobFed_NoTributarios", "NoPetroleros_OrgDeControl", "NoPetroleros_OrgDeControl_IMSS", "NoPetroleros_OrgDeControl_ISSTE", "No_Petroleros_CFE", "Gasto neto pagado", "Programable", "Gasto de operación","Servicios Personales", "Otros gastos de operación","Pensiones y Jubilaciones", "Subsidios y transferencias", "Ayudas y otros gastos", "Inversión física", "Inversión_física_directa", "Inversión_física_indirecta", "Inversión financiera y otras", "No programable", "Costo financiero", "Intereses comisiones y gastos", "Intereses_gobFed", "Intereses_paraestatal", "Apoyo a ahorradores", "IPAB", "Otros", "Participaciones", "Adefas", "Balance (A-B)", "Balance no presupuestario", "Balance pùblico", "Balance público sin inversión", "Recursos externos", "Recursos internos", "Ingresos tributarios", "Ingresos no tributarios") 

enero2019SituacionFinanciera <- enero2019SituacionFinanciera  %>%
  select(V5, V8, V11, Concepto)
colnames(enero2019SituacionFinanciera) <- c("2018","2019", "Crecimiento real", "Concepto")


# ya lo tenemos en mesAñoSituacionFinanciera
## Ahora va el costo de la deuda interna
## slice it
enero2019DeudaInterna <- slice(enero2019DeudaInterna, -c(1, 2, 3, 4))


#colnames(enero2019DeudaInterna) <- c("Concepto","Intereses", "Comisiones", "Total")
## split
enero2019SplitedDeudaInterna <- data.frame(do.call("rbind", strsplit(as.character(enero2019DeudaInterna$V3), " ", fixed = TRUE)))
enero2019SplitedDeudaInterna$V1 <- enero2019DeudaInterna$V1
enero2019SplitedDeudaInterna$Intereses <- as.numeric(as.character(gsub(",","", enero2019SplitedDeudaInterna$X2))) - as.numeric(as.character(gsub(",","", enero2019SplitedDeudaInterna$X1)))

colnames(enero2019SplitedDeudaInterna) <- c("Comisiones", "Total", "Concepto", "Intereses")

# Select only v2
enero2019SplitedDeudaInterna$Concepto <- c("Suma de estructura por plazo","Largo plazo","Corto plazo","Suma de estructura por usuario", "Usuario_GobFederal", "Usuario_GobFederal_LargoPlazo", "Usuario_GobFederal_CortoPlazo", "Usuario_EmpresasProductivas", "Usuario_EmpresasProductivas_LargoPlazo", "Usuario_EmpresasProductivas_CortoPlazo", "Usuario_BancaDesarrollo", "Usuario_BancaDesarrollo_LargoPlazo", "Usuario_BancaDesarrollo_CortoPlazo", "Suma de estructuras por fuente de financiamiento", "Emision de valores", "Fondo de ahorro SAR", "Banca Comercial", "Obligaciones ISSTE", "Bonos de pensión PEMEX", "Bonos de pensión CFE", "Otros")
## Cambiar columna de "concepto"
enero2019DeudaInterna <- enero2019SplitedDeudaInterna
##Outer Join Splited y DeudaInternaGeneral porque una sí tiene el concepto
## Deuda externa
 enero2019DeudaExterna <- slice(enero2019DeudaExterna, -c(1, 2, 3, 4, 5, 6))
## substring
 enero2019SplitedDeudaExterna <- data.frame(do.call("rbind", strsplit(as.character(enero2019DeudaExterna$V3), " ", fixed = TRUE)))
 enero2019SplitedDeudaExterna$V1 <- enero2019DeudaExterna$V1

##Join
enero2019MergedDeudaExterna <- merge(enero2019DeudaExterna, enero2019SplitedDeudaExterna, by = "V1")

enero2019MergedDeudaExterna <- enero2019MergedDeudaExterna %>%
  select(V1, V2, V4, X1, X2)
  
enero2019DeudaExterna <- enero2019MergedDeudaExterna  
colnames(enero2019DeudaExterna) <- c("Concepto","Intereses", "Total", "Gastos de compromiso", "Otras comisiones")
  
## Exportar cada tabla como csv
write.csv(enero2019Resumen, file = "enero2019Resumen.csv", fileEncoding = "UTF-8")
write.csv(enero2019SituacionFinanciera, file ="enero2019SituacionFinanciera.csv", fileEncoding = "UTF-8")
write.csv(enero2019DeudaExterna, file ="enero2019DeudaExterna.csv", fileEncoding = "UTF-8")
write.csv(enero2019DeudaInterna, file ="enero2019DeudaInterna.csv", fileEncoding = "UTF-8")

