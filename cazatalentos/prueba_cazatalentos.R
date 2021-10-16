#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 )/1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )

  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

Estrategia_B  <- function()
{
  #Estrategia
  #Se juegan varias rondas
  #En cada ronda, los jugadores que participan, tiran 70 tiros
  #De una ronda a la otra, solo pasan los que tuvieron igual o mayor aciertos a la mediana de aciertos de la ronda anterior
  #Se elige el mejor jugador de la sexta ronda

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )

  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100   70  tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  z1 <- 40
  planilla_cazatalentos[ ids_juegan1,  tiros1 := z1]  #registro en la planilla que tiran 70 tiros
  resultado1  <- gimnasio_tirar( ids_juegan1, z1)
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan1,  pr_1 := resultado1/z1 ] 

  #Ronda 2 -------------------------------------------------------
  #A la mitad mejor la hago tirar 70 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan1, median(pr_1) ]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ pr_1 >= mediana, id ]
  z2 <- 50
  planilla_cazatalentos[ ids_juegan2,  tiros2 := z2 ]  #registro en la planilla que tiran 70 tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, z2)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla
  
  
  planilla_cazatalentos[ ids_juegan2,  pr_2 := (resultado2 + 
                                                  planilla_cazatalentos[planilla_cazatalentos$id 
                                                                        %in% ids_juegan2]$aciertos1)/(z1+z2) ] 

  #Ronda 3 -------------------------------------------------------
  #A la mitad mejor la hago tirar 70 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan2, median(pr_2,na.rm = FALSE) ]
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][ pr_2 >= mediana, id ]
  z3 <- 60
 
  planilla_cazatalentos[ ids_juegan3,  tiros3 := z3 ]  #registro en la planilla que tiran 70 tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, z3)
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan3,  pr_3 := (resultado3 +planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan3]$aciertos1+planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan3]$aciertos2)/(z1+z2+z3)] 

  #Ronda 4 -------------------------------------------------------
  #A la mitad mejor la hago tirar 70 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan3, median(pr_3,na.rm = FALSE) ]
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][ pr_3 >= mediana, id ]
  z4 <- 70
  planilla_cazatalentos[ ids_juegan4,  tiros4 := z4 ]  #registro en la planilla que tiran 70 tiros
  resultado4  <- gimnasio_tirar( ids_juegan4, z4)
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan4,  pr_4 := (resultado4 +planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan4]$aciertos1+planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan4]$aciertos2+planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan4]$aciertos3)/(z1+z2+z3+z4)] 
  
  #Ronda 5 -------------------------------------------------------
  #A la mitad mejor la hago tirar 70 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan4, median(pr_4,na.rm = FALSE) ]
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][ pr_4 >= mediana, id ]
  z5 <- 80
  planilla_cazatalentos[ ids_juegan5,  tiros5 := z5 ]  #registro en la planilla que tiran 70 tiros
  resultado5  <- gimnasio_tirar( ids_juegan5, z5)
  planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan5,  pr_5 := (resultado5 +planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan5]$aciertos1+planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan5]$aciertos2+planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan5]$aciertos3+planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan5]$aciertos4)/(z1+z2+z3+z4+z5)] 
  

  #Ronda 6 -------------------------------------------------------
  #A la mitad mejor la hago tirar 200 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  
  
  mediana  <- planilla_cazatalentos[ ids_juegan5, median(pr_5,na.rm = FALSE) ]
  ids_juegan6  <- planilla_cazatalentos[ ids_juegan5 ][ pr_5 >= mediana, id ]
  if (length(ids_juegan6)==0){
  #medias <- planilla_cazatalentos[, .(Mean = rowMeans(.SD)), .SDcols=c('pr_1','pr_2','pr_3','pr_4','pr_5'),by = id]
  
  #planilla_cazatalentos <- cbind(planilla_cazatalentos,medias)
  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(pr_5) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
  
  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )}
  else{
  z6 <- 90
  
  planilla_cazatalentos[ ids_juegan6,  tiros6 := z6 ]  #registro en la planilla que tiran 200 tiros
  resultado6  <- gimnasio_tirar( ids_juegan6, z6)

  planilla_cazatalentos[ ids_juegan6,  aciertos6 := resultado6 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan6,  pr_6 := (resultado6 +planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan6]$aciertos1+planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan6]$aciertos2+planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan6]$aciertos3+planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan6]$aciertos4+planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan6]$aciertos5)/(z1+z2+z3+z4+z5+z6)] 
  
  #medias <- planilla_cazatalentos[, .(Mean = rowMeans(.SD)), .SDcols=c('pr_1','pr_2','pr_3','pr_4','pr_5','pr_6'),by = id]
  
  #planilla_cazatalentos <- cbind(planilla_cazatalentos,medias)
  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(pr_6) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )}
  
  return( veredicto)
}
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 102191 )  #debe ir una sola vez, ANTES de los experimentos

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:10000 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy

  veredicto  <- Estrategia_B()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]

tiros_total 
tasa_eleccion_correcta

#Es una sábana corta ...


