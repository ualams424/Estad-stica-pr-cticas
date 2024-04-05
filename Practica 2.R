install.packages("e1071")
datos <- read.csv('datosB2.csv', sep= ";", dec= ",")
datos <- datos[!is.na(datos$TOTAL_SALA),]
datos

datos["VP"] = ifelse(datos$PUNTUACION>2500, "POSITIVA", "NEGATIVA")
datos["SA"] = ifelse(datos$SALA=="A", "POSITIVA", "NEGATIVA")
datos["VH1"] = ifelse(datos$HABITACION1<13, "POSITIVA", "NEGATIVA")
datos["VT"] = ifelse(datos$TOTAL_SALA<50,"POSITIVA", "NEGATIVA")
datos["VMB"] = ifelse(datos$MIEMBROS>5 & datos$MIEMBROS<3, "NO BALANCEADO", "BALANCEADO")
datos["VNP"] = ifelse(datos$PISTAS<3, "POSITIVA", "NEGATIVA")
datos["LE"] = ifelse(datos$LIDERAZGO=="SI", "LIDERAZGO", "NO LIDERAZGO")
datos["PE"] = ifelse(datos$PRUEBA_EXTRA=="SI", "`POSITIVA", "NEGATIVA" )