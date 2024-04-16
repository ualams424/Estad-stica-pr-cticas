install.packages("e1071")
datos <- read.csv('datosB2.csv', sep= ";", dec= ",")
datos <- datos[!is.na(datos$TOTAL_SALA),]
datos

#Crea nuevas variables categóricas

datos["VP"] = ifelse(datos$PUNTUACION>2500, "POSITIVA", "NEGATIVA")
datos["SA"] = ifelse(datos$SALA=="A", "POSITIVA", "NEGATIVA")
datos["VH1"] = ifelse(datos$HABITACION1<13, "POSITIVA", "NEGATIVA")
datos["VT"] = ifelse(datos$TOTAL_SALA<50,"POSITIVA", "NEGATIVA")
datos["VMB"] = ifelse(datos$MIEMBROS>5 & datos$MIEMBROS<3, "NEGATIVA", "POSITIVA")
datos["VNP"] = ifelse(datos$PISTAS<3, "POSITIVA", "NEGATIVA")
datos["LE"] = ifelse(datos$LIDERAZGO=="SI", "POSITIVA", "NEGATIVA")
datos["PE"] = ifelse(datos$PRUEBA_EXTRA=="SI", "`POSITIVA", "NEGATIVA" )

library(e1071)

#Utilizando solo información de sala.

# Seleciona las variables categóricas para el modelo
df_a = datos[,c("SA","VP")]
df_a
#Variable final
Y_a = df_a[,"VP"]
#Variable que se utiliza para predecir
X_a = df_a[,"SA"]

m_a = naiveBayes(X_a,Y_a)
# Extrae predicciones para evaluar el clasificador
pred_a = predict(m_a, df_a[,"SA"])
# Evaluar capacidad de prediccion del modelo
real_a=df_a$VP
nconjunta_a = table(pred_a, real_a) 
nVP_a = table(df_a$VP)

#a)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta_a["POSITIVA","POSITIVA"]/nVP_a["POSITIVA"]

#b)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta_a["NEGATIVA","NEGATIVA"]/nVP_a["NEGATIVA"]


#c)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser positiva.
n_a= nconjunta_a["POSITIVA", "POSITIVA"] + nconjunta_a["POSITIVA", "NEGATIVA"]
nconjunta_a["POSITIVA","POSITIVA"]/n_a

#d)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser negativa.
n_a2= nconjunta_a["NEGATIVA", "NEGATIVA"] + nconjunta_a["NEGATIVA", "POSITIVA"]
nconjunta_a["NEGATIVA","POSITIVA"]/n_a2

#e)Probabilidad de que el clasificador acierte en su predición.
(nconjunta_a["NEGATIVA","NEGATIVA"]+nconjunta_a["POSITIVA","POSITIVA"])/(nconjunta_a["NEGATIVA","NEGATIVA"]+nconjunta_a["POSITIVA","POSITIVA"]+nconjunta_a["NEGATIVA","POSITIVA"]+nconjunta_a["POSITIVA","NEGATIVA"])

#Utilizando información de sala, número de miembros y habitación 1.

df_b = datos[,c("SA","VMB","VH1","VP")]
df_b
Y_b = df_b[,"VP"]
X_b = df_b[,c("VH1","VMB","SA")]
m_b = naiveBayes(X_b,Y_b)
pred_b = predict(m_b, df_b[,c("VH1","VMB","SA")])
real_b=df$VP
nconjunta_b = table(pred_b, real_b) 
nVP_b = table(df$VP)

#a)Probabilidad de que acierte en la predicción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta_b["POSITIVA","POSITIVA"]/nVP_b["POSITIVA"]

#b)Probabilidad de que acierte en la predicción de valoración negativa dado que el grupo ha tenido una valoración negativa.
nconjunta_b["NEGATIVA","NEGATIVA"]/nVP_b["NEGATIVA"]

#c)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser positiva.
n_b= nconjunta_b["POSITIVA", "POSITIVA"] + nconjunta_b["POSITIVA", "NEGATIVA"]
nconjunta_b["POSITIVA","POSITIVA"]/n_b

#d)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser negativa.
n_b2= nconjunta_b["NEGATIVA", "NEGATIVA"] + nconjunta_b["NEGATIVA", "POSITIVA"]
nconjunta_b["NEGATIVA","POSITIVA"]/n_b2

#e)Probabilidad de que el clasificador acierte en su predición.
(nconjunta_b["NEGATIVA","NEGATIVA"]+nconjunta_b["POSITIVA","POSITIVA"])/(nconjunta_b["NEGATIVA","NEGATIVA"]+nconjunta_b["POSITIVA","POSITIVA"]+nconjunta_b["NEGATIVA","POSITIVA"]+nconjunta_b["POSITIVA","NEGATIVA"])


#Utilizando información de sala, número de miembros, habitación 1, y tiempo total.
df_c = datos[,c("SA","VMB","VH1","VT","VP")]
df_c
Y_c = df_c[,"VP"]
X_c = df_c[,c("VH1","VMB","VT","SA")]
m_c = naiveBayes(X_c,Y_c)
pred_c = predict(m_c, df_c[,c("VH1","VMB","VT","SA")])
real_c=df1$VP
nconjunta_c = table(pred_c, real_c) 
nVP_c = table(df_c$VP)

#a)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta_c["POSITIVA","POSITIVA"]/nVP_c["POSITIVA"]

#b)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta_c["NEGATIVA","NEGATIVA"]/nVP_c["NEGATIVA"]

#c)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser positiva.
n_c= nconjunta_c["POSITIVA", "POSITIVA"] + nconjunta_c["POSITIVA", "NEGATIVA"]
nconjunta_c["POSITIVA","POSITIVA"]/n_c

#d)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser negativa.
n_c2= nconjunta_c["NEGATIVA", "NEGATIVA"] + nconjunta_c["NEGATIVA", "POSITIVA"]
nconjunta_c["NEGATIVA","POSITIVA"]/n_c2

#e)Probabilidad de que el clasificador acierte en su predición.
(nconjunta_c["NEGATIVA","NEGATIVA"]+nconjunta_c["POSITIVA","POSITIVA"])/(nconjunta_c["NEGATIVA","NEGATIVA"]+nconjunta_c["POSITIVA","POSITIVA"]+nconjunta_c["NEGATIVA","POSITIVA"]+nconjunta_c["POSITIVA","NEGATIVA"])


#Utilizando toda la información disponible de sala, número de miembros, tiempos, número de pistas y liderazgo
df_d = datos[,c("SA","VMB","VH1","VT","VNP","LE","VP")]
df_d
Y_d = df_d[,"VP"]
X_d = df_d[,c("VH1","VMB","VT","VNP","LE","SA")]
m_d = naiveBayes(X_d,Y_d)
pred_d = predict(m_d, df_d[,c("VH1","VMB","VT","VNP","LE","SA")])
real_d=df2$VP
nconjunta_d = table(pred_d, real_d) 
nVP_d = table(df_d$VP)

#a)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta_d["POSITIVA","POSITIVA"]/nVP_d["POSITIVA"]

#b)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta_d["NEGATIVA","NEGATIVA"]/nVP_d["NEGATIVA"]

#c)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser positiva.
n_d= nconjunta_d["POSITIVA", "POSITIVA"] + nconjunta_d["POSITIVA", "NEGATIVA"]
nconjunta_d["POSITIVA","POSITIVA"]/n_d

#d)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser negativa.
n_d2= nconjunta_d["NEGATIVA", "NEGATIVA"] + nconjunta_d["NEGATIVA", "POSITIVA"]
nconjunta_d["NEGATIVA","POSITIVA"]/n_d2

#e)Probabilidad de que el clasificador acierte en su predición.
(nconjunta_d["NEGATIVA","NEGATIVA"]+nconjunta_d["POSITIVA","POSITIVA"])/(nconjunta_d["NEGATIVA","NEGATIVA"]+nconjunta_d["POSITIVA","POSITIVA"]+nconjunta_d["NEGATIVA","POSITIVA"]+nconjunta_d["POSITIVA","NEGATIVA"])
