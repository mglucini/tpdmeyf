#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
setwd("/home/mauro/Escritorio/Facultad/Data Mining/DataMiningenEconomíayFinanzas" )
#cargo el dataset
dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")
#cargo el dataset donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")

# Corrección  variable internet
table(dataset[,'internet']) 
table(dapply[,'internet'])

dapply[,'internet'] <- replace(dapply$internet, dapply$internet>0, 1)
dapply[,'internet'] <- dapply[,'internet']*-1+1 

# Corrección  variable tmobile_app
unique(dataset[,'tmobile_app'])#Chequear valores unicos variable tmobile_app
unique(dapply[,'tmobile_app'])

dapply[,'tmobile_app'] <- dapply[,'tmobile_app']*-1+1 # Correccion


# Creación de variables

# Prestamo Personal Promedio

# Suma de cantidad de  Prestamos Prendarios , Personales e Hipotecarios
dataset[,"ctotal_prestamos"] <- dataset[,"cprestamos_personales"]+dataset[,"cprestamos_prendarios"]+dataset[,"cprestamos_hipotecarios"]
dapply[,"ctotal_prestamos"] <- dapply[,"cprestamos_personales"]+dapply[,"cprestamos_prendarios"]+dapply[,"cprestamos_hipotecarios"]

# Suma de monto de  Prestamos Prendarios , Personales e Hipotecarios
dataset[,"mtotal_prestamos"] <- dataset[,"mprestamos_personales"]+dataset[,"mprestamos_prendarios"]+dataset[,"mprestamos_hipotecarios"]
dapply[,"mtotal_prestamos"] <- dapply[,"mprestamos_personales"]+dapply[,"mprestamos_prendarios"]+dapply[,"mprestamos_hipotecarios"]

# Rate Total Prestamos
dataset[,"prestamos_promedio_total"] <- dataset[,"mtotal_prestamos"]/dataset[,"ctotal_prestamos"]
dataset[,'prestamos_promedio_total'] <- replace(dataset$prestamos_promedio_total, is.nan(dataset$prestamos_promedio_total), 0)

dapply[,"prestamos_promedio_total"] <- dapply[,"mtotal_prestamos"]/dapply[,"ctotal_prestamos"]
dapply[,'prestamos_promedio_total'] <- replace(dapply$prestamos_promedio_total, is.nan(dapply$prestamos_promedio_total), 0)



################
fwrite( dataset,
        file="./datasets/fe_paquete_premium_202009.csv",
        sep= "," )
fwrite( dapply,
        file="./datasets/fe_paquete_premium_202011.csv",
        sep= "," )

