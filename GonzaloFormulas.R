#Gonzalo Alvarez Bedolla
#Programacion Anualidades vencidas
#A continuacion abordare las anualidades vencidas con dos enfoques, desde el valor final y desde el valor actual

#El uso de las formulas programadas requerira que el usuario inserte los datos necesarios y conforme al tiempo que lo ocupe, es decir, si trabajara en tasa mensual, el plazo y numero de periodos tendra que estar adecuado a meses

#Valor futuro, conociendo la anualidad, tasa de interés del periodo y el número (plazo) de anualidades.

ANLDvalorfinal=function(A,r,Tpers){
  VF=A*(((1+r)^Tpers-1)/r)
  return(VF)
         }
#Anualidad, conociendo valor futuro, tasa del periodo y número de pagos.

ANLDanualidadConVF=function(VF,r,Tpers){
  A=VF/(((1+r)^Tpers-1)/r)
  return(A)
}

#Número de pagos o plazo, conociendo valor futuro, número de pagos y tasa del periodo.

ANLDnperiodosConVF=function(VF,A,r){
  Tpers=log(((VF*r)/A)+1)/log(1+r)
  Tpers=round(Tpers,0)
  return(Tpers)
}


#Tasa del periodo, conociendo valor futuro, número de pagos y monto de la anualidad.
#Usaremos a formula creada por el profesor. Es importante cargar la funcion y despues cargar el source con el link, esto para que se cargen todas las funciones

ANLDtasaPeriodoConVF=function(VF,A,Tpers){
source("https://www.dropbox.com/s/s1nv2dypd2z0f3e/funcionesRMateFin.R?raw=1")
  r=tasaAnualidadVencidaVF(VF,A,Tpers,umbral=10*(10^-17))
  return(r)
}
ANLDtasaAnualConVF=function(r,aproxAño){
  i=r*aproxAño
  return(i)
}
#Valor actual, conociendo la anualidad, tasa de interés del periodo y el número (plazo) de anualidades.

ANLDvaloractual=function(A,r,Tpers){
  VA=A*((1-(1+r)^-Tpers)/r)
  return(VA)
}

#Anualidad, conociendo valor actual, tasa del periodo y número de pagos.

ANLDanualidadConVA=function(VA,r,Tpers){
  A=VA/((1-(1+r)^-Tpers)/r)
  return(A)
}

#Número de pagos o plazo, conociendo valor actual, número de pagos y tasa del periodo.

ANLDnperiodosConVA=function(VA,A,r){
  Tpers=-log(1-((VA*r)/A))/log(1+r)
  Tpers=round(Tpers,0)
  return(Tpers)
}

#Tasa del periodo, conociendo valor actual, número de pagos y monto de la anualidad.
#Usaremos a formula creada por el profesor. Es importante cargar la funcion y despues cargar el source con el link, esto para que se cargen todas las funciones

ANLDtasaPeriodoConVA=function(VA,A,Tpers){
  source("https://www.dropbox.com/s/s1nv2dypd2z0f3e/funcionesRMateFin.R?raw=1")
  r=tasaAnualidadVencidaVA(VA,A,Tpers,umbral=10*(10^-17))
  return(r)
}
ANLDtasaAnualconVA=function(r,aproxAño){
  i=r*aproxAño
  return(i)
}
