#Necesita para correr en Google Cloud
#256 GB de memoria RAM
#300 GB de espacio en el disco local
#8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("Rcpp")
require("rlist")
require("yaml")

require("lightgbm")


#defino la carpeta donde trabajo
directory.root  <-  "~/buckets/b1/"  #Google Cloud
setwd( directory.root )

palancas  <- list()  #variable con las palancas para activar/desactivar

palancas$version  <- "vidapropia_01"   #Muy importante, ir cambiando la version

palancas$variablesdrift  <- c("ccajas_transacciones","internet","tmobile_app")   #aqui van las columnas que se quieren eliminar

palancas$corregir <-  TRUE    # TRUE o FALSE

palancas$nuevasvars <-  TRUE  #si quiero hacer Feature Engineering manual

palancas$dummiesNA  <-  TRUE #La idea de Santiago Dellachiesa

palancas$lag1   <- TRUE    #lag de orden 1
palancas$delta1 <- TRUE    # campo -  lag de orden 1 
palancas$lag2   <- TRUE
palancas$delta2 <- TRUE
palancas$lag3   <- TRUE
palancas$delta3 <- TRUE
palancas$lag4   <- TRUE
palancas$delta4 <- TRUE
palancas$lag5   <- TRUE
palancas$delta5 <- TRUE
palancas$lag6   <- TRUE
palancas$delta6 <- TRUE


palancas$tendenciaYmuchomas  <- TRUE    #Great power comes with great responsability


palancas$canaritosimportancia  <- TRUE  #si me quedo solo con lo mas importante de canaritosimportancia


#escribo para saber cuales fueron los parametros
write_yaml(  palancas,  paste0( "./work/palanca_",  palancas$version  ,".yaml" ) )

#------------------------------------------------------------------------------

ReportarCampos  <- function( dataset )
{
  cat( "La cantidad de campos es ", ncol(dataset) , "\n" )
}
#------------------------------------------------------------------------------
#Agrega al dataset una variable que va de 1 a 12, el mes, para que el modelo aprenda estacionalidad

AgregarMes  <- function( dataset )
{
  gc()
  dataset[  , mes := foto_mes %% 100 ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Elimina las variables que uno supone hace Data Drifting

DriftEliminar  <- function( dataset, variables )
{
  gc()
  dataset[  , c(variables) := NULL ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#A las variables que tienen nulos, les agrega una nueva variable el dummy de is es nulo o no {0, 1}

DummiesNA  <- function( dataset )
{
  gc()
  nulos  <- colSums( is.na(dataset[foto_mes==202101]) )  #cuento la cantidad de nulos por columna
  colsconNA  <- names( which(  nulos > 0 ) )

  dataset[ , paste0( colsconNA, "_isNA") :=  lapply( .SD,  is.na ),
             .SDcols= colsconNA]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Corrige poniendo a NA las variables que en ese mes estan dañadas

Corregir  <- function( dataset )
{
  gc()
  #acomodo los errores del dataset

  dataset[ foto_mes==201801,  internet   := NA ]
  dataset[ foto_mes==201801,  thomebanking   := NA ]
  dataset[ foto_mes==201801,  chomebanking_transacciones   := NA ]
  dataset[ foto_mes==201801,  tcallcenter   := NA ]
  dataset[ foto_mes==201801,  ccallcenter_transacciones   := NA ]
  dataset[ foto_mes==201801,  cprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_hipotecarios  := NA ]
  dataset[ foto_mes==201801,  ccajas_transacciones   := NA ]
  dataset[ foto_mes==201801,  ccajas_consultas   := NA ]
  dataset[ foto_mes==201801,  ccajas_depositos   := NA ]
  dataset[ foto_mes==201801,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==201801,  ccajas_otras   := NA ]

  dataset[ foto_mes==201806,  tcallcenter   :=  NA ]
  dataset[ foto_mes==201806,  ccallcenter_transacciones   :=  NA ]

  dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
  dataset[ foto_mes==201904,  mttarjeta_visa_debitos_automaticos := NA ]
  dataset[ foto_mes==201904,  Visa_mfinanciacion_limite := NA ]

  dataset[ foto_mes==201905,  mrentabilidad     := NA ]
  dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
  dataset[ foto_mes==201905,  mcomisiones      := NA ]
  dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
  dataset[ foto_mes==201905,  mactivos_margen  := NA ]
  dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
  dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201905,  mcomisiones_otras := NA ]

  dataset[ foto_mes==201910,  mpasivos_margen   := NA ]
  dataset[ foto_mes==201910,  mactivos_margen   := NA ]
  dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones       := NA ]
  dataset[ foto_mes==201910,  mrentabilidad     := NA ]
  dataset[ foto_mes==201910,  mrentabilidad_annual        := NA ]
  dataset[ foto_mes==201910,  chomebanking_transacciones  := NA ]
  dataset[ foto_mes==201910,  ctarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  ctarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  mtarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  mtarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  ccajeros_propios_descuentos := NA ]
  dataset[ foto_mes==201910,  mcajeros_propios_descuentos := NA ]

  dataset[ foto_mes==202001,  cliente_vip   := NA ]

  dataset[ foto_mes==202006,  active_quarter   := NA ]
  dataset[ foto_mes==202006,  internet   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad_annual   := NA ]
  dataset[ foto_mes==202006,  mcomisiones   := NA ]
  dataset[ foto_mes==202006,  mactivos_margen   := NA ]
  dataset[ foto_mes==202006,  mpasivos_margen   := NA ]
  dataset[ foto_mes==202006,  mcuentas_saldo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_debito_transacciones   := NA ]
  dataset[ foto_mes==202006,  mautoservicio   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_visa_transacciones   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_visa_consumo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_master_transacciones   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_master_consumo   := NA ]
  dataset[ foto_mes==202006,  ccomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  mcomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  cextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  mextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  tcallcenter   := NA ]
  dataset[ foto_mes==202006,  ccallcenter_transacciones   := NA ]
  dataset[ foto_mes==202006,  thomebanking   := NA ]
  dataset[ foto_mes==202006,  chomebanking_transacciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_transacciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_consultas   := NA ]
  dataset[ foto_mes==202006,  ccajas_depositos   := NA ]
  dataset[ foto_mes==202006,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_otras   := NA ]
  dataset[ foto_mes==202006,  catm_trx   := NA ]
  dataset[ foto_mes==202006,  matm   := NA ]
  dataset[ foto_mes==202006,  catm_trx_other   := NA ]
  dataset[ foto_mes==202006,  matm_other   := NA ]
  dataset[ foto_mes==202006,  ctrx_quarter   := NA ]
  dataset[ foto_mes==202006,  tmobile_app   := NA ]
  dataset[ foto_mes==202006,  cmobile_app_trx   := NA ]


  dataset[ foto_mes==202010,  internet  := NA ]
  dataset[ foto_mes==202011,  internet  := NA ]
  dataset[ foto_mes==202012,  internet  := NA ]
  dataset[ foto_mes==202101,  internet  := NA ]

  dataset[ foto_mes==202009,  tmobile_app  := NA ]
  dataset[ foto_mes==202010,  tmobile_app  := NA ]
  dataset[ foto_mes==202011,  tmobile_app  := NA ]
  dataset[ foto_mes==202012,  tmobile_app  := NA ]
  dataset[ foto_mes==202101,  tmobile_app  := NA ]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset )
{
  gc()
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  #Aqui debe usted agregar sus propias nuevas variables
  # Suma de cantidad de  Prestamos Prendarios , Personales e Hipotecarios
  dataset[ , ctotal_prestamos          := rowSums( cbind( cprestamos_personales,  cprestamos_prendarios,cprestamos_hipotecarios) , na.rm=TRUE ) ]
  dataset[ , crateprestamosp         := cprestamos_personales  / ctotal_prestamos ]
  dataset[ , crateprestamospr         := cprestamos_prendarios  / ctotal_prestamos ]
  dataset[ , crateprestamosh         := cprestamos_hipotecarios  / ctotal_prestamos ]
  # Suma de monto de  Prestamos Prendarios , Personales e Hipotecarios
  dataset[ , mtotal_prestamos          := rowSums( cbind( mprestamos_personales,  mprestamos_prendarios,mprestamos_hipotecarios) , na.rm=TRUE ) ]
  dataset[ , mrateprestamosp         := mprestamos_personales  / mtotal_prestamos ]
  dataset[ , mrateprestamospr         := mprestamos_prendarios  / mtotal_prestamos ]
  dataset[ , mrateprestamosh         := mprestamos_hipotecarios  / mtotal_prestamos ]
  
  # Rate Total Prestamos
  dataset[ , prestamos_promedio_total         := mtotal_prestamos  / ctotal_prestamos ]
  
  ####
  
  dataset[ , prom_cuentacorriente         := mcuenta_corriente  / ccuenta_corriente ]
  dataset[ , prom_cajaahorro         := mcaja_ahorro  / ccaja_ahorro  ]
  dataset[ , ccorriente_ahorro          := rowSums( cbind( ccuenta_corriente,  ccaja_ahorro) , na.rm=TRUE ) ]
  dataset[ , mcorriente_ahorro          := rowSums( cbind( mcuenta_corriente,  mcaja_ahorro) , na.rm=TRUE ) ]
  dataset[ , prom_corriente_ahorro         := mcorriente_ahorro  / ccorriente_ahorro  ]
  
  dataset[ , rt_1_ctrx_quarter         := ctrx_quarter  / mtarjeta_visa_consumo  ]
  dataset[ , rt_2_ctrx_quarter         := ctrx_quarter  / mcaja_ahorro  ]
  dataset[ , rt_3_ctrx_quarter         := ctrx_quarter  / mtotal_prestamos  ]
  dataset[ , rt_4_ctrx_quarter         := ctrx_quarter  / cpayroll_trx  ]
  dataset[ , rt_5_ctrx_quarter         := ctrx_quarter  / mvr_msaldopesos  ]
  dataset[ , rt_6_ctrx_quarter         := ctrx_quarter  / mcuentas_saldo  ]
  dataset[ , rt_7_ctrx_quarter         := ctrx_quarter  / cpayroll_trx  ]
  dataset[ , rt_8_ctrx_quarter         := ctrx_quarter  / mpayroll  ]
  dataset[ , rt_9_ctrx_quarter         := ctrx_quarter  / mcuenta_corriente  ]
  dataset[ , rt_10_ctrx_quarter         := ctrx_quarter  / ctarjeta_visa_transacciones  ]
  dataset[ , rt_11_ctrx_quarter         := ctrx_quarter  / mprestamos_personales  ]
  dataset[ , rt_12_ctrx_quarter         := ctrx_quarter  / mactivos_margen  ]
  dataset[ , rt_13_ctrx_quarter         := ctrx_quarter  / mv_status01  ]
  dataset[ , rt_14_ctrx_quarter         := ctrx_quarter  / mcomisiones_mantenimiento  ]
  dataset[ , rt_15_ctrx_quarter         := ctrx_quarter  / ctarjeta_visa_transacciones  ]
  dataset[ , rt_16_ctrx_quarter         := ctrx_quarter  / mrentabilidad_annual  ]
  
  
  dataset[ , rt_1_mactivos_margen         := mactivos_margen  / mprestamos_personales  ]
  dataset[ , rt_1_mprestamos_personales         := mprestamos_personales  / mv_status01  ]
  dataset[ , rt_1_mactivos_margen         := mactivos_margen  / mv_status01  ]
  dataset[ , rt_14_mactivos_margen         := mactivos_margen  / mcomisiones_mantenimiento  ]
  dataset[ , rt_15_mactivos_margen        := mactivos_margen  / ctarjeta_visa_transacciones  ]
  dataset[ , rt_16_mactivos_margen         := mactivos_margen  / mrentabilidad_annual  ]
  
  dataset[ , rt_1_mtarjeta_visa_consumo         := mtarjeta_visa_consumo  / mcaja_ahorro  ]
  dataset[ , rt_2_mtarjeta_visa_consumo         := mtarjeta_visa_consumo  / mtotal_prestamos  ]
  dataset[ , rt_5_mtarjeta_visa_consumo         := mtarjeta_visa_consumo  / mvr_msaldopesos  ]
  dataset[ , rt_6_mtarjeta_visa_consumo         := mtarjeta_visa_consumo  / mcuentas_saldo  ]
  dataset[ , rt_7_mtarjeta_visa_consumo         := mtarjeta_visa_consumo  / cpayroll_trx  ]
  dataset[ , rt_8_mtarjeta_visa_consumo         := mtarjeta_visa_consumo  / mpayroll  ]
  dataset[ , rt_9_mtarjeta_visa_consumo         := mtarjeta_visa_consumo  / mv_msaldopesos  ]
  dataset[ , rt_14_mtarjeta_visa_consumo          := mtarjeta_visa_consumo   / mcomisiones_mantenimiento  ]
  dataset[ , rt_15_mtarjeta_visa_consumo        := mtarjeta_visa_consumo   / ctarjeta_visa_transacciones  ]
  dataset[ , rt_16_mtarjeta_visa_consumo          := mtarjeta_visa_consumo   / mrentabilidad_annual  ]
  
  dataset[ , rt_1_mcaja_ahorro         := mcaja_ahorro  / mtotal_prestamos  ]
  dataset[ , rt_4_mcaja_ahorro         := mcaja_ahorro  / cpayroll_trx  ]
  dataset[ , rt_5_mcaja_ahorro       := mcaja_ahorro  / mvr_msaldopesos  ]
  dataset[ , rt_6_mcaja_ahorro         := mcaja_ahorro  / mcuentas_saldo  ]
  dataset[ , rt_8_mcaja_ahorro         := mcaja_ahorro  / mpayroll  ]
  dataset[ , rt_2_mcaja_ahorro         := mcaja_ahorro  / mv_msaldopesos  ]
  dataset[ , rt_14_mcaja_ahorro         := mcaja_ahorro   / mcomisiones_mantenimiento  ]
  dataset[ , rt_15_mcaja_ahorro        := mcaja_ahorro   / ctarjeta_visa_transacciones  ]
  dataset[ , rt_16_mcaja_ahorro          := mcaja_ahorro   / mrentabilidad_annual  ]  
  
  dataset[ , rt_1_mtotal_prestamos        := mtotal_prestamos  / cpayroll_trx  ]
  dataset[ , rt_5_mtotal_prestamos      := mtotal_prestamos  / mvr_msaldopesos  ]
  dataset[ , rt_6_mtotal_prestamos         := mtotal_prestamos  / mcuentas_saldo  ]
  dataset[ , rt_8_mtotal_prestamos         := mtotal_prestamos  / mpayroll  ]
  dataset[ , rt_14_mtotal_prestamos        := mtotal_prestamos   / mcomisiones_mantenimiento  ]
  dataset[ , rt_15_mtotal_prestamos        := mtotal_prestamos   / ctarjeta_visa_transacciones  ]
  dataset[ , rt_16_mtotal_prestamos          := mtotal_prestamos   / mrentabilidad_annual  ]  
  
  dataset[ , rt_1_cpayroll_trx        := cpayroll_trx  / mvr_msaldopesos  ]
  dataset[ , rt_6_cpayroll_trx         := cpayroll_trx  / mcuentas_saldo  ]
  dataset[ , rt_8_cpayroll_trx         := cpayroll_trx  / mpayroll  ]
  dataset[ , rt_14_cpayroll_trx    := cpayroll_trx   / mcomisiones_mantenimiento  ]
  dataset[ , rt_15_cpayroll_trx     := cpayroll_trx   / ctarjeta_visa_transacciones  ]
  dataset[ , rt_13_cpayroll_trx          := cpayroll_trx   / mrentabilidad_annual  ]  
  
  dataset[ , rt_14_ctarjeta_visa_transacciones        := ctarjeta_visa_transacciones   / mcomisiones_mantenimiento  ]
  dataset[ , rt_15_mrentabilidad_annual      := mrentabilidad_annual   / ctarjeta_visa_transacciones  ]
  dataset[ , rt_16_mcomisiones_mantenimiento          := mcomisiones_mantenimiento   / mrentabilidad_annual  ]  
  
  dataset[ , rt_1_mvr_msaldopesos        := mvr_msaldopesos  / mcuentas_saldo  ]
  dataset[ , rt_8_mvr_msaldopesos         := mvr_msaldopesos  / mpayroll  ]
  dataset[ , rt_1_mcuentas_saldo        := mcuentas_saldo  / mpayroll  ]
  
  dataset[ , paste0("cliente_antiguedad","_rank")        := frank(cliente_antiguedad)]
  dataset[ , paste0('mrentabilidad',"_rank")        := frank(mrentabilidad)]
  dataset[ , paste0('mrentabilidad_annual',"_rank")        := frank(mrentabilidad_annual)]
  dataset[ , paste0('mcomisiones',"_rank")        := frank(mcomisiones)]
  dataset[ , paste0('mactivos_margen',"_rank")        := frank(mactivos_margen)]
  dataset[ , paste0('mpasivos_margen',"_rank")        := frank(mpasivos_margen)]
  dataset[ , paste0('cproductos',"_rank")        := frank(cproductos)]
  dataset[ , paste0('mcuenta_corriente',"_rank")        := frank(mcuenta_corriente)]
  dataset[ , paste0('mcaja_ahorro',"_rank")        := frank(mcaja_ahorro)]
  dataset[ , paste0('mcaja_ahorro_dolares',"_rank")        := frank(mcaja_ahorro_dolares)]
  dataset[ , paste0('mcaja_ahorro_adicional',"_rank")        := frank(mcaja_ahorro_adicional)]
  dataset[ , paste0('mcuentas_saldo',"_rank")        := frank(mcuentas_saldo)]
  dataset[ , paste0('ctarjeta_debito',"_rank")        := frank(ctarjeta_debito)]
  dataset[ , paste0('ctarjeta_debito_transacciones',"_rank")        := frank(ctarjeta_debito_transacciones)]
  dataset[ , paste0('mautoservicio',"_rank")        := frank(mautoservicio)]
  dataset[ , paste0('ctarjeta_visa_transacciones',"_rank")        := frank(ctarjeta_visa_transacciones)]
  dataset[ , paste0('mtarjeta_visa_consumo',"_rank")        := frank(mtarjeta_visa_consumo)]
  dataset[ , paste0('ctarjeta_master_transacciones',"_rank")        := frank(ctarjeta_master_transacciones)]
  dataset[ , paste0('mtarjeta_master_consumo',"_rank")        := frank(mtarjeta_master_consumo)]
  dataset[ , paste0('mprestamos_personales',"_rank")        := frank(mprestamos_personales)]
  dataset[ , paste0('mprestamos_prendarios',"_rank")        := frank(mprestamos_prendarios)]
  dataset[ , paste0('mtotal_prestamos',"_rank")        := frank(mtotal_prestamos)]
  dataset[ , paste0('mprestamos_hipotecarios',"_rank")        := frank(mprestamos_hipotecarios)]
  dataset[ , paste0('mplazo_fijo_dolares',"_rank")        := frank(mplazo_fijo_dolares)]
  dataset[ , paste0('mplazo_fijo_pesos',"_rank")        := frank(mplazo_fijo_pesos)]
  dataset[ , paste0('minversion1_pesos',"_rank")        := frank(minversion1_pesos)]
  dataset[ , paste0('minversion1_dolares',"_rank")        := frank(minversion1_dolares)]
  dataset[ , paste0('minversion2',"_rank")        := frank(minversion2)]
  dataset[ , paste0('mpayroll',"_rank")        := frank(mpayroll)]
  dataset[ , paste0('mcuenta_debitos_automaticos',"_rank")        := frank(mcuenta_debitos_automaticos)]
  dataset[ , paste0('mttarjeta_visa_debitos_automaticos',"_rank")        := frank(mttarjeta_visa_debitos_automaticos)]
  dataset[ , paste0('mttarjeta_master_debitos_automaticos',"_rank")        := frank(mttarjeta_master_debitos_automaticos)]
  dataset[ , paste0('mpagomiscuentas',"_rank")        := frank(mpagomiscuentas)]
  dataset[ , paste0('mtarjeta_visa_descuentos',"_rank")        := frank(mtarjeta_visa_descuentos)]
  dataset[ , paste0('mtarjeta_master_descuentos',"_rank")        := frank(mtarjeta_master_descuentos)]
  dataset[ , paste0('mcomisiones_mantenimiento',"_rank")        := frank(mcomisiones_mantenimiento)]
  dataset[ , paste0('mcomisiones_otras',"_rank")        := frank(mcomisiones_otras)]
  dataset[ , paste0('mtransferencias_recibidas',"_rank")        := frank(mtransferencias_recibidas)]
  dataset[ , paste0('mtransferencias_emitidas',"_rank")        := frank(mtransferencias_emitidas)]
  dataset[ , paste0('mextraccion_autoservicio',"_rank")        := frank(mextraccion_autoservicio)]
  dataset[ , paste0('mcheques_depositados',"_rank")        := frank(mcheques_depositados)]
  dataset[ , paste0('mcheques_emitidos',"_rank")        := frank(mcheques_emitidos)]
  dataset[ , paste0('ccallcenter_transacciones',"_rank")        := frank(ccallcenter_transacciones)]
  dataset[ , paste0('chomebanking_transacciones',"_rank")        := frank(chomebanking_transacciones)]
  dataset[ , paste0('matm',"_rank")        := frank(matm)]
  dataset[ , paste0('matm_other',"_rank")        := frank(matm_other)]
  dataset[ , paste0('ctrx_quarter',"_rank")        := frank(ctrx_quarter)]
  dataset[ , paste0('Master_mfinanciacion_limite',"_rank")        := frank(Master_mfinanciacion_limite)]
  dataset[ , paste0('Master_Fvencimiento',"_rank")        := frank(Master_Fvencimiento)]
  dataset[ , paste0('Master_msaldototal',"_rank")        := frank(Master_msaldototal)]
  dataset[ , paste0('Master_msaldopesos',"_rank")        := frank(Master_msaldopesos)]
  dataset[ , paste0('Master_msaldodolares',"_rank")        := frank(Master_msaldodolares)]
  dataset[ , paste0('Master_mconsumospesos',"_rank")        := frank(Master_mconsumospesos)]
  dataset[ , paste0('Master_mconsumosdolares',"_rank")        := frank(Master_mconsumosdolares)]
  dataset[ , paste0('Master_mlimitecompra',"_rank")        := frank(Master_mlimitecompra)]
  dataset[ , paste0('Master_mpagado',"_rank")        := frank(Master_mpagado)]
  dataset[ , paste0('Master_mpagospesos',"_rank")        := frank(Master_mpagospesos)]
  dataset[ , paste0('Master_mpagosdolares',"_rank")        := frank(Master_mpagosdolares)]
  dataset[ , paste0('Master_mconsumototal',"_rank")        := frank(Master_mconsumototal)]
  dataset[ , paste0('Master_cconsumos',"_rank")        := frank(Master_cconsumos)]
  dataset[ , paste0('Visa_mfinanciacion_limite',"_rank")        := frank(Visa_mfinanciacion_limite)]
  dataset[ , paste0('Visa_Fvencimiento',"_rank")        := frank(Visa_Fvencimiento)]
  dataset[ , paste0('Visa_msaldototal',"_rank")        := frank(Visa_msaldototal)]
  dataset[ , paste0('Visa_msaldopesos',"_rank")        := frank(Visa_msaldopesos)]
  dataset[ , paste0('Visa_msaldodolares',"_rank")        := frank(Visa_msaldodolares)]
  dataset[ , paste0('Visa_mconsumospesos',"_rank")        := frank(Visa_mconsumospesos)]
  dataset[ , paste0('Visa_mconsumosdolares',"_rank")        := frank(Visa_mconsumosdolares)]
  dataset[ , paste0('Visa_mlimitecompra',"_rank")        := frank(Visa_mlimitecompra)]
  dataset[ , paste0('Visa_mpagado',"_rank")        := frank(Visa_mpagado)]
  dataset[ , paste0('Visa_mpagospesos',"_rank")        := frank(Visa_mpagospesos)]
  dataset[ , paste0('Visa_mpagosdolares',"_rank")        := frank(Visa_mpagosdolares)]
  dataset[ , paste0('Visa_mconsumototal',"_rank")        := frank(Visa_mconsumototal)]
  dataset[ , paste0('Visa_cconsumos',"_rank")        := frank(Visa_cconsumos)]
  dataset[ , paste0('Visa_mpagominimo',"_rank")        := frank(Visa_mpagominimo)]
  
  
  
  
  
  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#esta funcion supone que dataset esta ordenado por   <numero_de_cliente, foto_mes>
#calcula el lag y el delta lag

Lags  <- function( cols, nlag, deltas )
{
  gc()
  sufijo  <- paste0( "_lag", nlag )

  dataset[ , paste0( cols, sufijo) := shift(.SD, nlag, NA, "lag"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  #agrego los deltas de los lags, con un "for" nada elegante
  if( deltas )
  {
    sufijodelta  <- paste0( "_delta", nlag )

    for( vcol in cols )
    {
     dataset[,  paste0(vcol, sufijodelta) := get( vcol)  - get(paste0( vcol, sufijo))]
    }
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formula de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C, y no en la porqueria de R o Python

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;


    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}')

#------------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos 6 meses
#la tendencia es la pendiente de la recta que ajusta por cuadrados minimos

TendenciaYmuchomas  <- function( dataset, cols, ventana=6, tendencia=TRUE, minimo=TRUE, maximo=TRUE, promedio=TRUE, 
                                 ratioavg=FALSE, ratiomax=FALSE)
{
  gc()
  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- ventana

  last  <- nrow( dataset )

  #creo el vector_desde que indica cada ventana
  #de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids   <- dataset$numero_de_cliente

  vector_desde  <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1

  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }

  for(  campo  in   cols )
  {
    nueva_col     <- fhistC( dataset[ , get(campo) ], vector_desde ) 

    if(tendencia)  dataset[ , paste0( campo, "_tend", ventana) := nueva_col[ (0*last +1):(1*last) ]  ]
    if(minimo)     dataset[ , paste0( campo, "_min", ventana)  := nueva_col[ (1*last +1):(2*last) ]  ]
    if(maximo)     dataset[ , paste0( campo, "_max", ventana)  := nueva_col[ (2*last +1):(3*last) ]  ]
    if(promedio)   dataset[ , paste0( campo, "_avg", ventana)  := nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratioavg)   dataset[ , paste0( campo, "_ratioavg", ventana)  := get(campo) /nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratiomax)   dataset[ , paste0( campo, "_ratiomax", ventana)  := get(campo) /nueva_col[ (2*last +1):(3*last) ]  ]
  }

}
#------------------------------------------------------------------------------
VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- getinfo(datos, "label")
  vpesos   <- getinfo(datos, "weight")

  #solo sumo 48750 si vpesos > 1, hackeo 
  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 48750, -1250 ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta

  gan  <- mean( tbl[ 1:500,  gan_acum] )  #meseta de tamaño 500

  pos_meseta  <- tbl[ 1:500,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#Elimina del dataset las variables que estan por debajo de la capa geologica de canaritos

CanaritosImportancia  <- function( canaritos_ratio=0.2 )
{
  gc()
  ReportarCampos( dataset )
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

  for( i  in 1:(ncol(dataset)*canaritos_ratio))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]

  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01" ) )

  azar  <- runif( nrow(dataset) )
  dataset[ , entrenamiento := foto_mes>= 202001 &  foto_mes<= 202010 &  foto_mes!=202006 & ( clase01==1 | azar < 0.10 ) ]

  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==202011, campos_buenos, with=FALSE]),
                          label=   dataset[ foto_mes==202011, clase01],
                          weight=  dataset[ foto_mes==202011, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
                          )


  param <- list( objective= "binary",
                 metric= "custom",
                 first_metric_only= TRUE,
                 boost_from_average= TRUE,
                 feature_pre_filter= FALSE,
                 verbosity= -100,
                 seed= 999983,
                 max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                 min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                 lambda_l1= 0.0,         #por ahora, lo dejo fijo
                 lambda_l2= 0.0,         #por ahora, lo dejo fijo
                 max_bin= 31,            #por ahora, lo dejo fijo
                 num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                 force_row_wise= TRUE,    #para que los alumnos no se atemoricen con tantos warning
                 learning_rate= 0.065, 
                 feature_fraction= 1.0,   #lo seteo en 1 para que las primeras variables del dataset no se vean opacadas
                 min_data_in_leaf= 260,
                 num_leaves= 60,
               # num_threads= 8,
                 early_stopping_rounds= 200 )

  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )

  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]

  fwrite( tb_importancia, file="./work/impo.txt",  , sep="\t" )

  umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) + sd(pos) ]  #Atencion corto en la mediana !!

  col_utiles  <- tb_importancia[ pos < umbral & !( Feature %like% "canarito"),  Feature ]
  col_utiles  <-  unique( c( col_utiles,  c("numero_de_cliente","foto_mes","clase_ternaria","mes") ) )
  col_inutiles  <- setdiff( colnames(dataset), col_utiles )

  dataset[  ,  (col_inutiles) := NULL ]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------

#Aqui empieza el programa

#cargo el dataset ORIGINAL
dataset  <- fread( "./datasetsOri/paquete_premium.csv.gz")
gc()


setorder(  dataset, numero_de_cliente, foto_mes )  #ordeno el dataset

AgregarMes( dataset )  #agrego el mes del año

if( palancas$dummiesNA )  DummiesNA( dataset )  #esta linea debe ir ANTES de Corregir  !!

if( palancas$corregir )  Corregir( dataset )  #esta linea debe ir DESPUES de  DummiesNA

if( palancas$nuevasvars )  AgregarVariables( dataset )


#--------------------------------------
#Esta primera parte es muuuy  artesanal  y discutible  ya que hay multiples formas de hacerlo

cols_analiticas  <- copy( setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") ) )
if( palancas$tendenciaYmuchomas )  TendenciaYmuchomas( dataset, cols_analiticas)

if( palancas$lag1 )   Lags(  cols_analiticas, 1, palancas$delta1 )

#hay una cantidad muy grande de variables, no soporto la presion
CanaritosImportancia( canaritos_ratio= 0.3 )
# at ease !  https://www.youtube.com/watch?v=QhCISxbO7rg
#--------------------------------------



#Agrego lags de orden 2
cols_analiticas  <- setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") )  
if( palancas$lag2 )   Lags( cols_analiticas, 2, palancas$delta2 )
CanaritosImportancia( canaritos_ratio= 0.1 )

#Agrego lags de orden 3
cols_analiticas  <- setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") )
if( palancas$lag3 )   Lags( cols_analiticas, 3, palancas$delta3 )
CanaritosImportancia( 0.3 )      # vuelvo a hacer lugar gracias a los canaritos

#Agrego lags de orden 4
cols_analiticas  <- setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") )
if( palancas$lag4 )   Lags(  cols_analiticas, 4, palancas$delta4 )
CanaritosImportancia( 0.3 )

#Agrego lags de orden 5
cols_analiticas  <- setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") )
if( palancas$lag5 )   Lags( cols_analiticas, 5, palancas$delta5 )
CanaritosImportancia( 0.3 )

#Agrego lags de orden 6
cols_analiticas  <- setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") )
if( palancas$lag6 )   Lags( cols_analiticas, 6, palancas$delta6 )
CanaritosImportancia( 0.3 )


#Ahora construyo patrones elaborados  https://www.youtube.com/watch?v=aLj8WCO-2QI

cols_analiticas  <- copy( setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") ) )
if( palancas$tendenciaYmuchomas )  TendenciaYmuchomas( dataset, cols_analiticas, minimo=FALSE, maximo=FALSE)
CanaritosImportancia( canaritos_ratio=0.1 )

cols_analiticas  <- copy( setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") ) )
if( palancas$tendenciaYmuchomas )  TendenciaYmuchomas( dataset, cols_analiticas, ventana=3, tendencia=FALSE, minimo=FALSE, maximo=FALSE)
CanaritosImportancia( canaritos_ratio=0.1 )


#dejo la clase como ultimo campo
nuevo_orden  <- c( setdiff( colnames( dataset ) , "clase_ternaria" ) , "clase_ternaria" )
setcolorder( dataset, nuevo_orden )


#Grabo el dataset    https://www.youtube.com/watch?v=66CP-pq7Cx0
fwrite( dataset,
        paste0( "./datasets/canaproxy_", palancas$version, ".csv.gz" ),
        logical01 = TRUE,
        sep= "," )



#quit( save="no" )


