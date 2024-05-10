datos <- read.csv('datosB2.csv', sep= ";", dec= ",")
datos <- datos[!is.na(datos$TOTAL_SALA),]

datos["VP"] = ifelse(datos$PUNTUACION>2500, "POSITIVA", "NEGATIVA")
datos["SA"] = ifelse(datos$SALA=="A", "POSITIVA", "NEGATIVA")
datos["VH1"] = ifelse(datos$HABITACION1<13, "POSITIVA", "NEGATIVA")
datos["VT"] = ifelse(datos$TOTAL_SALA<50,"POSITIVA", "NEGATIVA")
datos["VMB"] = ifelse(datos$MIEMBROS>5 & datos$MIEMBROS<3, "NEGATIVA", "POSITIVA")
datos["VNP"] = ifelse(datos$PISTAS<3, "POSITIVA", "NEGATIVA")
datos["LE"] = ifelse(datos$LIDERAZGO=="SI", "POSITIVA", "NEGATIVA")
datos["PE"] = ifelse(datos$PRUEBA_EXTRA=="SI", "`POSITIVA", "NEGATIVA" )

#test independencia

"1. Comprobar si el tiempo total es independiente de las demás variables (usar datos
  agrupados de la práctica 2)."
df= datos[,c("VT","VP","SA", "VH1", "VMB", "VNP", "LE", "PE")]
VT= df[,"VT"]
VP= df[,"VP"]
SA= df[,"SA"]
VH1= df[,"VH1"]
VMB= df[,"VMB"]
VNP= df[,"VNP"]
LE= df[,"LE"]
PE= df[,"PE"]

# Las hipótesis del contraste son:
#
# - H0: las variables son independientes (hipótesis nula)
# - H1: las variable no son independientes
#
# nivel de significación alpha=0.05

tabla1=table(VT,VP)
t1 = chisq.test(tabla1)
t1$p.value

"Como el p-valor = 0.0000000864 < 0.05 = alpha, con los datos disponibles sí se puede
rechazar la hipótesis nula por lo que concluimos que no son independientes"

tabla2=table(VT,SA)
t2 = chisq.test(tabla2)
t2$p.value

"Como el p-valor = 0.4967805 > 0.05 = alpha, con los datos disponibles no se puede
rechazar la hipótesis nula por lo que concluimos que son independientes"

tabla3=table(VT,VH1)
t3 = chisq.test(tabla3)
t3$p.value

"Como el p-valor = 0.09826166 > 0.05 = alpha, con los datos disponibles no se puede
rechazar la hipótesis nula por lo que concluimos que son independientes"

tabla4=table(VT,VMB)
t4 = chisq.test(tabla4)
t4$p.value

"Como el p-valor = 0.05878172 > 0.05 = alpha, con los datos disponibles no se puede
rechazar la hipótesis nula por lo que concluimos que son independientes"

tabla5=table(VT,VNP)
t5 = chisq.test(tabla5)
t5$p.value

"Como el p-valor = 0.7201071 > 0.05 = alpha, con los datos disponibles no se puede
rechazar la hipótesis nula por lo que concluimos que son independientes"

tabla6=table(VT,LE)
t6 = chisq.test(tabla6)
t6$p.value

"Como el p-valor = 0.5328279 > 0.05 = alpha, con los datos disponibles no se puede
rechazar la hipótesis nula por lo que concluimos que son independientes"

tabla7=table(VT,PE)
t7 = chisq.test(tabla7)
t7$p.value

"Como el p-valor = 0.05878172 > 0.05 = alpha, con los datos disponibles no se puede
rechazar la hipótesis nula por lo que concluimos que son independientes"


"2. Datos estadísticos históricos arrojan que el porcentaje de grupos con valoración final
positiva solía estar en torno al 40 %. ¿Podemos confirmar en base a nuestros datos que
el número de grupos con valoración positiva ha aumentado?"

# Las hipótesis del contraste son:
#
# - H0: La valoracion final positiva es del 40%
# - H1: La valoracion final positiva es mayor 40%
#
# nivel de significación alpha=0.05

tabla_binomial = table(VP)
exitos = 34
total = 63
porcentaje = 0.4
b1 = binom.test(exitos, total, p = porcentaje)
b1$p.value

"Como el p-valor = 0.02830544 < 0.05 = alpha, con los datos disponibles se puede
rechazar la hipótesis nula y afirmamos la hipótesis 1, por lo que concluimos que el porcentaje de valoración
final positiva con los datos actuales es mayor al 40%"


"3. Comprobar si hay diferencias entre el tiempo de realización de la habitación 1 y de la
habitación 3."
