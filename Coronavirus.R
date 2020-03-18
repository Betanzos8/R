library("RCurl")
library("httr")
library("XML")
library("ggplot2")

#Lectura de página fuente
url<-"https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports"
doc<-htmlParse(as.character(GET(url)))

#Obtención de ligas a partir de xpath
archivos<-xpathSApply(doc,'//a[@class="js-navigation-open "]',xmlGetAttr,"href")

#Formato de ligas
archivos<-paste0("https://raw.githubusercontent.com",archivos)
archivos<-archivos[grepl(".csv",archivos)]
archivos<-gsub("/blob/","/",archivos)

#Extracción de datos
datos<-data.frame()
for (i in 1:length(archivos)){
  dummy<-read.csv(url(archivos[i]))
  names(dummy)<-c("State","Country","Last_Update","Confirmed","Deaths","Recovered")
  dummy<-dummy[,1:6]
  dummy$fecha<-gsub(".*/|.csv","",archivos[i])
  datos<-rbind(datos,dummy)
}

#Formato y ligera limpieza de datos importantes
datos$fecha<-as.Date(datos$fecha,format="%m-%d-%Y")
datos$Country<-gsub("Mainland China","China",datos$Country)
datos$Country<-gsub("Korea, South","South Korea",datos$Country)
datos$Country<-gsub("Taiwan\\*","Taiwan",datos$Country)

#Creación de agrupado por país por día de casos confirmados
agregados<-aggregate(datos$Confirmed,by=list(datos$fecha,datos$Country),sum,na.rm=T)
names(agregados)<-c("Fecha","Pais","Casos")

#Creación de variable indice con el fin de hacer comparaciones entre paises
minimos<-aggregate(agregados$Fecha[agregados$Casos>0],by=list(agregados$Pais[agregados$Casos>0]),min)
names(minimos)<-c("Pais","Min")
agregados<-merge(agregados,minimos,by="Pais",all.x=T)
agregados$indice<-as.numeric(agregados$Fecha-agregados$Min)

#Modelo exponencial por regresión lineal
conteos<-agregados$Casos[agregados$Pais=="Italy"][20:47]
tiempo<-1:length(conteos)

#Revisión de linealidad del logaritmo y generación del model
plot(log(conteos),log(tiempo+2),pch=20)
exponential.model <- lm(log(conteos)~ log(tiempo+2) )
summary(exponential.model)

#gráfica del modelo resultante
conteos_exponencial <- exp(predict(exponential.model,list(tiempo)))
plot(tiempo,conteos,pch=16)
lines(rango_tiempo, conteos_exponencial,lwd=2, col = "red", xlab = "tiempo", ylab = "conteos")


