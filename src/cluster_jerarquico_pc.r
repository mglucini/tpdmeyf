require("data.table")
require("randomForest")


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd( "~/Escritorio/Facultad/Data Mining/DataMiningenEconomíayFinanzas" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( "datasetsOri/paquete_premium.csv.gz", stringsAsFactors= TRUE)
gc()

#achico el dataset
dataset[  ,  azar := runif( nrow(dataset) ) ]
dataset  <-  dataset[  clase_ternaria =="BAJA+1"  & foto_mes>=202011  & foto_mes<=202011, ]
gc()


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )
gc()


campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
                     "mpagomiscuentas")



#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[ , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox = TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )


pdf( paste0( paste0("./work/cluster_jerarquico.pdf" ) ))
plot( hclust.rf )
dev.off()


h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)

  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]

  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

dataset[  , .N,  cliente_vip ]  #tamaño de los vips

#ahora a mano veo las variables
dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter

dataset[  , mean(cliente_edad),  cluster2 ]

dataset[  , mean(cliente_antiguedad),  cluster2 ]

dataset[  , mean(cproductos),  cluster2 ]

dataset[  , mean(mcuenta_corriente),  cluster2 ]

dataset[  , mean(mcuentas_saldo),  cluster2 ]

dataset[  , mean(mautoservicio),  cluster2 ]

dataset[  , mean(ctarjeta_visa+ctarjeta_master),  cluster2 ] # cantidad total de tarjetas

dataset[  , mean(cprestamos_personales),  cluster2 ]
dataset[  , mean(mprestamos_personales),  cluster2 ]

dataset[  , mean(cprestamos_prendarios),  cluster2 ]
dataset[  , mean(mprestamos_prendarios),  cluster2 ]

dataset[  , mean(cprestamos_hipotecarios),  cluster2 ]
dataset[  , mean(mprestamos_hipotecarios),  cluster2 ]

dataset[  , mean(cinversion1),  cluster2 ]
dataset[  , mean(cinversion2),  cluster2 ]


dataset[  , mean(minversion1_pesos),  cluster2 ]

dataset[  , mean(cpayroll_trx),  cluster2 ]

dataset[  , mean(cpayroll2_trx),  cluster2 ]

dataset[  , mean(cpagodeservicios),  cluster2 ]

dataset[  , mean(cpagomiscuentas),  cluster2 ]

dataset[  , mean(ccajeros_propios_descuentos),  cluster2 ]
dataset[  , mean(ctarjeta_visa_descuentos),  cluster2 ]
dataset[  , mean(ctarjeta_master_descuentos),  cluster2 ]

dataset[  , mean(mcomisiones_mantenimiento+mcomisiones_otras),  cluster2 ]

dataset[  , mean(cforex),  cluster2 ]
dataset[  , mean(cforex_buy),  cluster2 ]
dataset[  , mean(cforex_sell),  cluster2 ]

dataset[  , mean(ctransferencias_recibidas),  cluster2 ]
dataset[  , mean(ctransferencias_emitidas),  cluster2 ]

dataset[  , mean(chomebanking_transacciones),  cluster2 ]
dataset[  , mean(ccajas_transacciones),  cluster2 ]

dataset[  , mean(ccajas_consultas),  cluster2 ]

dataset[  , mean(ccajas_extracciones),  cluster2 ]

dataset[  , mean(catm_trx_other),  cluster2 ]

dataset[  , mean(cmobile_app_trx),  cluster2 ]

dataset[  , sum(Master_delinquency),  cluster2 ]

dataset[  , .N,  list(cluster2, Master_status)]

dataset[  , mean(Master_mlimitecompra),  cluster2 ]
dataset[  , mean(Master_fechaalta),  cluster2 ]
dataset[  , mean(Master_mconsumototal),  cluster2 ]
dataset[  , mean(Master_cconsumos),  cluster2 ]
dataset[  , mean(Master_cadelantosefectivo),  cluster2 ]


dataset[  , sum(Visa_delinquency),  cluster2 ]

dataset[  , .N,  list(cluster2, Visa_status)]

dataset[  , mean(Visa_mlimitecompra),  cluster2 ]
dataset[  , mean(Visa_fechaalta),  cluster2 ]
dataset[  , mean(Visa_mconsumototal),  cluster2 ]
dataset[  , mean(Visa_cconsumos),  cluster2 ]
dataset[  , mean(Visa_cadelantosefectivo),  cluster2 ]





