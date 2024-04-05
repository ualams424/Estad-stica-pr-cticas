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

df = datos[,c("VH1","VMB","VP")]
df

#Variable final
Y = df[,"VP"]
#Variable que se utiliza para predecir
X = df[,c("VH1","VMB")]

m = naiveBayes(X,Y)

pred = predict(m, df[,c("VH1","VMB")])
real=df$VP
nconjunta = table(pred, real) 
nVP = table(df$VP)
nconjunta["POSITIVA","POSITIVA"]/nVP["POSITIVA"]

nconjunta["NEGATIVA","NEGATIVA"]/nVP["NEGATIVA"]

n= nconjunta["NEGATIVA", "NEGATIVA"] + nconjunta["NEGATIVA", "POSITIVA"]
nconjunta["NEGATIVA","NEGATIVA"]/n
