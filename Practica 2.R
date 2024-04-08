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

# Seleciona las variabels categóricas para el modelo
df = datos[,c("VH1","VMB","VP")]
df

#Variable final
Y = df[,"VP"]
#Variable que se utiliza para predecir
X = df[,c("VH1","VMB")]

m = naiveBayes(X,Y)

# Extrae predicciones para evaluar el clasificador
pred = predict(m, df[,c("VH1","VMB")])

# Evaluar capacidad de prediccion del modelo
real=df$VP
nconjunta = table(pred, real) 
nVP = table(df$VP)

#Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta["POSITIVA","POSITIVA"]/nVP["POSITIVA"]

#Probabilidad de que acierte en la predcción de valoración positiva dado que el grupo ha tenido una valoración positiva.
nconjunta["NEGATIVA","NEGATIVA"]/nVP["NEGATIVA"]

#Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser negativa.
n= nconjunta["NEGATIVA", "NEGATIVA"] + nconjunta["NEGATIVA", "POSITIVA"]
nconjunta["NEGATIVA","POSITIVA"]/n

#Probabilidad de que el grupo obtenga una valoración positiva dado que el clasificador predice que va a ser positiva.
n= nconjunta["POSITIVA", "POSITIVA"] + nconjunta["POSITIVA", "NEGATIVA"]
nconjunta["POSITIVA","POSITIVA"]/n