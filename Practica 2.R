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
df_a = datos[,c("SA","VP")]
df_a
Y_a = df_a[,"VP"]
X_a = df_a[,"SA"]
m_a = naiveBayes(X_a,Y_a)
pred_a = predict(m_a, df_a[,"SA"])
real_a=df_a$VP
nconjunta_a = table(pred_a, real_a) 
nVP_a = table(df_a$VP)

#a)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta_a["POSITIVA","POSITIVA"]/nVP_a["POSITIVA"]

#b)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta_a["NEGATIVA","NEGATIVA"]/nVP_a["NEGATIVA"]

#d)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser negativa.
n_a= nconjunta_a["NEGATIVA", "NEGATIVA"] + nconjunta_a["NEGATIVA", "POSITIVA"]
nconjunta_a["NEGATIVA","POSITIVA"]/n_a

#c)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser positiva.
n_a2= nconjunta_a["POSITIVA", "POSITIVA"] + nconjunta_a["POSITIVA", "NEGATIVA"]
nconjunta_a["POSITIVA","POSITIVA"]/n_a2


# Seleciona las variables categóricas para el modelo
df = datos[,c("SA","VMB","VH1","VP")]
df
#Variable final
Y = df[,"VP"]
#Variable que se utiliza para predecir
X = df[,c("VH1","VMB","SA")]

m = naiveBayes(X,Y)

# Extrae predicciones para evaluar el clasificador
pred = predict(m, df[,c("VH1","VMB","SA")])

# Evaluar capacidad de prediccion del modelo
real=df$VP
nconjunta = table(pred, real) 
nVP = table(df$VP)

#a)Probabilidad de que acierte en la predicción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta["POSITIVA","POSITIVA"]/nVP["POSITIVA"]

#b)Probabilidad de que acierte en la predicción de valoración negativa dado que el grupo ha tenido una valoración negativa.
nconjunta["NEGATIVA","NEGATIVA"]/nVP["NEGATIVA"]

#d)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser negativa.
n= nconjunta["NEGATIVA", "NEGATIVA"] + nconjunta["NEGATIVA", "POSITIVA"]
nconjunta["NEGATIVA","POSITIVA"]/n

#c)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser positiva.
n_2= nconjunta["POSITIVA", "POSITIVA"] + nconjunta["POSITIVA", "NEGATIVA"]
nconjunta["POSITIVA","POSITIVA"]/n_2

#Utilizando información de sala, número de miembros, habitación 1, y tiempo total.
df1 = datos[,c("SA","VMB","VH1","VT","VP")]
df1
Y1 = df1[,"VP"]
X1 = df1[,c("VH1","VMB","VT","SA")]
m1 = naiveBayes(X1,Y1)
pred1 = predict(m1, df1[,c("VH1","VMB","VT","SA")])
real1=df1$VP
nconjunta1 = table(pred1, real1) 
nVP1 = table(df1$VP)

#a)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta1["POSITIVA","POSITIVA"]/nVP1["POSITIVA"]

#b)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta1["NEGATIVA","NEGATIVA"]/nVP1["NEGATIVA"]

#d)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser negativa.
n1= nconjunta1["NEGATIVA", "NEGATIVA"] + nconjunta1["NEGATIVA", "POSITIVA"]
nconjunta1["NEGATIVA","POSITIVA"]/n1

#c)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser positiva.
n1_2= nconjunta1["POSITIVA", "POSITIVA"] + nconjunta1["POSITIVA", "NEGATIVA"]
nconjunta1["POSITIVA","POSITIVA"]/n1_2


#Utilizando toda la información disponible de sala, número de miembros, tiempos, número de pistas y liderazgo
df2 = datos[,c("SA","VMB","VH1","VT","VNP","LE","VP")]
df2
Y2 = df2[,"VP"]
X2 = df2[,c("VH1","VMB","VT","VNP","LE","SA")]
m2 = naiveBayes(X2,Y2)
pred2 = predict(m2, df2[,c("VH1","VMB","VT","VNP","LE","SA")])
real2=df2$VP
nconjunta2 = table(pred2, real2) 
nVP2 = table(df2$VP)

#a)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta2["POSITIVA","POSITIVA"]/nVP2["POSITIVA"]

#b)Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta2["NEGATIVA","NEGATIVA"]/nVP2["NEGATIVA"]

#d)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser negativa.
n2= nconjunta2["NEGATIVA", "NEGATIVA"] + nconjunta2["NEGATIVA", "POSITIVA"]
nconjunta2["NEGATIVA","POSITIVA"]/n2

#c)Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser positiva.
n2_2= nconjunta2["POSITIVA", "POSITIVA"] + nconjunta2["POSITIVA", "NEGATIVA"]
nconjunta2["POSITIVA","POSITIVA"]/n2_2
