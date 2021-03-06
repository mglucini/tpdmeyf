#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")


#defino la carpeta donde trabajo
directory.root  <-  "~/buckets/b1/"  #Google Cloud
setwd( directory.root )
ReportarCampos  <- function( dataset )
{
  cat( "La cantidad de campos es ", ncol(dataset) , "\n" )
}

#------------------------------------------------------------------------------

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

}
#------------------------------------------------------------------------------

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

}
#------------------------------------------------------------------------------
AgregarVariables  <- function( dataset )
{
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
    dataset[mapply(is.infinite, dataset)] <- NA
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
    dataset[mapply(is.nan, dataset)] <- 0
  }
  
  ReportarCampos( dataset )
}

AgregarMes  <- function( dataset )
{
  dataset[  , mes := foto_mes %% 100 ]
  ReportarCampos( dataset )
}

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
  
  gan  <- mean( tbl[ 1:500,  gan_acum] )  #meseta de tama??o 500
  
  pos_meseta  <- tbl[ 1:500,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )
  
  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
CanaritosImportancia  <- function( dataset )
{
  
  gc()
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]
  
  for( i  in 1:(ncol(dataset)/5))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]
  
  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01" ) )
  
  azar  <- runif( nrow(dataset) )
  entrenamiento  <-  dataset[ , foto_mes>= 202001 &  foto_mes<= 202010 &  foto_mes!=202006 & ( clase01==1 | azar < 0.10 ) ]
  
  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] )
  
  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==202011, campos_buenos, with=FALSE]),
                          label=   dataset[ foto_mes==202011, clase01],
                          weight=  dataset[ foto_mes==202011, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] )
  
  
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
                 learning_rate= 0.02, 
                 feature_fraction= 0.50,
                 min_data_in_leaf= 4000,
                 num_leaves= 600,
                 early_stopping_rounds= 200 )
  
  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )
  
  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]
  
  fwrite( tb_importancia, file="./work/impo.txt",  , sep="\t" )
  
  umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) - sd(pos) ]
  col_inutiles  <- tb_importancia[ pos >= umbral | Feature %like% "canarito",  Feature ]
  
  for( col in col_inutiles )
  {
    dataset[  ,  paste0(col) := NULL ]
  }
  
  rm( dtrain, dvalid )
  gc()
  
  ReportarCampos( dataset )
}
#cargo el dataset ORIGINAL
dataset  <- fread( "./datasetsOri/paquete_premium.csv.gz")


setorder(  dataset, numero_de_cliente, foto_mes )  #ordeno el dataset


Corregir( dataset )
AgregarVariables( dataset )


cols_analiticas  <- copy( setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") ) )

Lags(  cols_analiticas, 1, deltas=TRUE )

AgregarMes( dataset )

CanaritosImportancia( dataset )

#dejo la clase como ultimo campo
nuevo_orden  <- c( setdiff( colnames( dataset ) , "clase_ternaria" ) , "clase_ternaria" )
setcolorder( dataset, nuevo_orden )


#Grabo el dataset
fwrite( dataset,
        paste0( "./datasets/semillerio_dataset_lag_varpropias.csv.gz" ),
        logical01 = TRUE,
        sep= "," )

