library("RCurl")
library("httr")
library("XML")
library("ggplot2")
url<-"https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports"
doc<-htmlParse(as.character(GET(url)))

archivos<-xpathSApply(doc,'//a[@class="js-navigation-open "]',xmlGetAttr,"href")
archivos<-paste0("https://raw.githubusercontent.com",archivos)
archivos<-archivos[grepl(".csv",archivos)]
archivos<-gsub("/blob/","/",archivos)

datos<-data.frame()

for (i in 1:length(archivos)){
  dummy<-read.csv(url(archivos[i]))
  names(dummy)<-c("State","Country","Last_Update","Confirmed","Deaths","Recovered")
  dummy<-dummy[,1:6]
  dummy$fecha<-gsub(".*/|.csv","",archivos[i])
  datos<-rbind(datos,dummy)
}

datos$fecha<-as.Date(datos$fecha,format="%m-%d-%Y")
datos$Country<-gsub("Mainland China","China",datos$Country)
datos$Country<-gsub("Korea, South","South Korea",datos$Country)
datos$Country<-gsub("Taiwan\\*","Taiwan",datos$Country)


agregados<-aggregate(datos$Confirmed,by=list(datos$fecha,datos$Country),sum,na.rm=T)
names(agregados)<-c("Fecha","Pais","Casos")

minimos<-aggregate(agregados$Fecha[agregados$Casos>0],by=list(agregados$Pais[agregados$Casos>0]),min)
names(minimos)<-c("Pais","Min")
agregados<-merge(agregados,minimos,by="Pais",all.x=T)
agregados$indice<-as.numeric(agregados$Fecha-agregados$Min)
