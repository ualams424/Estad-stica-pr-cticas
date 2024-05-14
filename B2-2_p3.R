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
tiempo_habitacion1 = datos$HABITACION1
tiempo_habitacion3 = datos$HABITACION3

# Las hipótesis del contraste son:
#
# - H0: tiempo_habitación1 = tiempo_habitación3
# - H1: tiempo_habitación1 != tiempo_habitación3
#
# nivel de significación alpha=0.05

t3 = t.test(tiempo_habitacion1, tiempo_habitacion3, paired = FALSE)
t3$p.value

"Como el p-valor = 3.677649e-06 < 0.05 = alpha, con los datos disponibles se puede
rechazar la hipótesis nula y afirmamos la hipótesis 1, por lo que concluimos que los tiempos
de ambas habitaciones son distintos"

"Hacemos otro test para la comparación de medias de dos distribuciones normales para muestras independientes"

# Las hipótesis son:
#
# - H0: tiempo_habitación1 = tiempo_habitación3
# - H1: tiempo_habitación1 > tiempo_habitación3
#
# nivel de significación alpha=0.05


t3_2 = t.test(tiempo_habitacion1, tiempo_habitacion3, paired = FALSE)
t3_2$p.value

"Como el p-valor = 3.677649e-06 < 0.05 = alpha, con los datos disponibles se puede
rechazar la hipótesis nula y afirmamos la hipótesis 1, por lo que concluimos que el tiempo
de la habitación 1 de media es menor al tiempo de la habitación 2"

"4. Comprobar si hay diferencias entre las salas A y B en lo que respecta a:
-Tiempo de realizaciÓn de cada una de las habitaciones (1, 2 y 3)."

# Comprobar diferencias entre salas A y B en diferentes aspectos

# Filtrar datos por sala A y B
datos_A <- datos[datos$SALA == "A", ]
datos_B <- datos[datos$SALA == "B", ]

# Tiempo de realización de cada habitación por sala
habitacion1_A <- datos_A$HABITACION1
habitacion1_B <- datos_B$HABITACION1

habitacion2_A <- datos_A$HABITACION2
habitacion2_B <- datos_B$HABITACION2

habitacion3_A <- datos_A$HABITACION3
habitacion3_B <- datos_B$HABITACION3

# Tiempo total de realización por sala
tiempo_total_A <- datos_A$TOTAL_SALA
tiempo_total_B <- datos_B$TOTAL_SALA

# Número de pistas pedidas por sala
pistas_A <- datos_A$PISTAS
pistas_B <- datos_B$PISTAS

# Test de comparación de medias para cada aspecto

# Habitación 1
t_habitacion1 <- t.test(habitacion1_A, habitacion1_B)
t_habitacion1$p.value

# Habitación 2
t_habitacion2 <- t.test(habitacion2_A, habitacion2_B)
t_habitacion2$p.value

# Habitación 3
t_habitacion3 <- t.test(habitacion3_A, habitacion3_B)
t_habitacion3$p.value

# Tiempo total de realización
t_tiempo_total <- t.test(tiempo_total_A, tiempo_total_B)
t_tiempo_total$p.value

# Número de pistas pedidas
t_pistas <- t.test(pistas_A, pistas_B)
t_pistas$p.value

# Número de miembros de los grupos por sala
miembros_A <- datos_A$MIEMBROS
miembros_B <- datos_B$MIEMBROS

# Test de comparación de medias para el número de miembros de los grupos
t_miembros <- t.test(miembros_A, miembros_B)
t_miembros$p.value


#En el caso de Habitación 1, el valor de p es mayor que 0.05, lo que
#sugiere que no hay una diferencia significativa en el tiempo de realización
#entre las salas A y B para esta habitación.
#Para Habitación 2 y Habitación 3, los valores de p son extremadamente pequeños 
#(cercanos a cero), lo que sugiere que hay diferencias significativas en el tiempo de 
#realización entre las salas A y B para estas habitaciones.
#Para el Tiempo total de realización y el Número de pistas pedidas, los valores de p
#son mayores que 0.05, lo que sugiere que no hay diferencias significativas entre las 
#salas A y B en estos aspectos.
#Estos resultados sugieren que hay diferencias significativas en el tiempo de realización
#entre las salas A y B para las Habitaciones 2 y 3, pero no para la Habitación 1, mientras
#que no hay diferencias significativas en el tiempo total de realización ni en el número de
#pistas pedidas entre las salas A y B.


"5. Plantear una hipótesis en relación a los datos que pueda resolverse mediante un contraste,
y hallar una resolución a la misma."
# Las hipótesis del contraste son:
#
# - H0: El número de líderes en ambas habitaciones es igual.
# - H1: El número de líderes en ambas habitaciones es diferente.
#
# nivel de significación alpha=0.05

# Crear una tabla de contingencia para el número de líderes en Sala A y Sala B
lideres_A <- table(datos_A$LIDERAZGO)
lideres_B <- table(datos_B$LIDERAZGO)

# Crear una tabla de contingencia combinada
tabla_lideres <- matrix(c(lideres_A["SI"], lideres_A["NO"], lideres_B["SI"], lideres_B["NO"]), 
                        nrow = 2, byrow = TRUE,
                        dimnames = list(c("Sala A", "Sala B"), c("Líder", "No Líder")))

# Realizar el test de chi-cuadrado para la tabla de contingencia
test_lideres <- chisq.test(tabla_lideres)
test_lideres$p.value

if (test_lideres$p.value < 0.05) {
  conclusion_lideres <- "Con los datos disponibles se puede rechazar la hipótesis nula y afirmar la hipótesis alternativa, concluyendo que hay una diferencia significativa en el número de líderes entre la Sala A y la Sala B."
} else {
  conclusion_lideres <- "Con los datos disponibles no se puede rechazar la hipótesis nula, concluyendo que no hay una diferencia significativa en el número de líderes entre la Sala A y la Sala B."
}

conclusion_lideres
