library("RCurl")
library("httr")
library("XML")
library("ggplot2")
library("dplyr")
url<-"https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports"
doc<-htmlParse(as.character(GET(url)))
archivos<-xpathSApply(doc,'//a[@class="js-navigation-open "]',xmlGetAttr,"href")
archivos<-paste0("https://raw.githubusercontent.com",archivos)
archivos<-archivos[grepl(".csv",archivos)]
archivos<-gsub("/blob/","/",archivos)
datos<-data.frame()
for (i in 1:length(archivos)){
  dummy<-read.csv(url(archivos[i]),stringsAsFactors = F)
  names(dummy)<-gsub("ï..","",names(dummy))
  names(dummy)<-gsub("\\.","_",names(dummy))
  dummy<-dummy[,c("Province_State","Country_Region","Last_Update","Confirmed","Deaths","Recovered")]
  names(dummy)<-c("State","Country","Last_Update","Confirmed","Deaths","Recovered")
  dummy<-dummy[,1:6]
  dummy$Last_Update<-substr(dummy$Last_Update,1,16)
  dummy$Last_Update<-as.POSIXlt(dummy$Last_Update,tryFormats = c("%Y-%m-%dT%H:%M","%m/%d/%Y %H:%M","%Y-%m-%d %H:%M"),tz="GMT")
  dummy$fecha<-gsub(".*/|.csv","",archivos[i])
  datos<-rbind(datos,dummy)
  Sys.sleep(1)
}
datos$fecha<-as.Date(datos$fecha,format="%m-%d-%Y")
datos$Country<-gsub("Mainland China","China",datos$Country)
datos$Country<-gsub("Korea, South","South Korea",datos$Country)
datos$Country<-gsub("Taiwan\\*","Taiwan",datos$Country)
datos$Country<-gsub("UK","United Kingdom",datos$Country)
datos$Country<-gsub("Iran \\(Islamic Republic of\\)","Iran",datos$Country)
#datos$Country[!grepl("China",datos$Country)]<-"Mundo"

agregados<-aggregate(datos$Confirmed,by=list(datos$fecha,datos$Country),sum,na.rm=T)
names(agregados)<-c("Fecha","Pais","Confirmados")
agregados2<-aggregate(datos$Deaths,by=list(datos$fecha,datos$Country),sum,na.rm=T)
names(agregados2)<-c("Fecha","Pais","Muertes")
agregados3<-aggregate(datos$Recovered,by=list(datos$fecha,datos$Country),sum,na.rm=T)
names(agregados3)<-c("Fecha","Pais","Recuperados")
agregados$llave<-paste0(agregados$Fecha,agregados$Pais)
agregados2$llave<-paste0(agregados2$Fecha,agregados2$Pais)
agregados3$llave<-paste0(agregados3$Fecha,agregados3$Pais)

agregados<-merge(agregados,agregados2[,c("llave","Muertes")],by="llave",all=T)
agregados<-merge(agregados,agregados3[,c("llave","Recuperados")],by="llave",all=T)
agregados$Salientes<-agregados$Muertes+agregados$Recuperados
agregados$Letalidad<-agregados$Muertes/agregados$Salientes
agregados$llave<-NULL

ggplot() +
  geom_line(data = agregados[agregados$Pais %in% c("Switzerland","Belgium","France","Italy","Spain","Germany") & agregados$Recuperados>=10,], aes(x = Fecha, y = Letalidad, color = Pais), size = 1) +
  xlab("Fecha") +
  ylab("Letalidad")+
  labs(title="Letalidad en Europa")+
  theme(plot.title = element_text(color="red", size=14, face="bold.italic",hjust = 0.5))

