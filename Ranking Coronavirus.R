library("RCurl")
library("httr")
library("XML")
library("ggplot2")
library("dplyr")

#Lectura y limpieza
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
  dummy$Last_Update<-as.POSIXlt(dummy$Last_Update,tryFormats = c("%Y-%m-%dT%H:%M:%OS","%m/%d/%Y %H:%M","%Y-%m-%d %H:%M:%OS"))
  dummy$fecha<-gsub(".*/|.csv","",archivos[i])
  datos<-rbind(datos,dummy)
  Sys.sleep(1)
}
datos$fecha<-as.Date(datos$fecha,format="%m-%d-%Y")
datos$Country<-gsub("Mainland China","China",datos$Country)
datos$Country<-gsub("Korea, South","South Korea",datos$Country)
datos$Country<-gsub("Taiwan\\*","Taiwan",datos$Country)
datos$Country<-gsub("UK","United Kingdom",datos$Country)

#Agrupamiento de datos
agregados<-aggregate(datos$Confirmed,by=list(datos$fecha,datos$Country),sum,na.rm=T)
names(agregados)<-c("Fecha","Pais","Casos")
minimos<-aggregate(agregados$Fecha[agregados$Casos>0],by=list(agregados$Pais[agregados$Casos>0]),min)
names(minimos)<-c("Pais","Min")
agregados<-merge(agregados,minimos,by="Pais",all.x=T)
agregados$indice<-as.numeric(agregados$Fecha-agregados$Min)

#Funcion de ranking
obt_ordenada<-function(x){
  conteos<-agregados$Casos[agregados$Pais==x&agregados$Casos>0]
  fechas<-agregados$Fecha[agregados$Pais==x&agregados$Casos>0]
  if (length(conteos)>=3){
    inc<-(conteos[2:length(conteos)]-conteos[1:(length(conteos)-1)])>=8
    inc<-inc[1:(length(inc)-2)]+inc[2:(length(inc)-1)]+inc[3:length(inc)]
    if (max(inc,na.rm=T)==3& sum(min(conteos[2:length(conteos)]-conteos[1:(length(conteos)-1)])<0)<=1){
      conteos<-conteos[match(3,inc):length(conteos)]
      fechas<-fechas[match(3,inc):length(fechas)]
      inc<-(conteos[2:length(conteos)]-conteos[1:(length(conteos)-1)])<=0
      for (i in length(inc):1){
        if (inc[i]){
          conteos[i+1]<-floor(conteos[i]*sqrt(conteos[i+2]/conteos[i]))
        }
      }
      if (length(conteos)>=3){
        ordenadas<-numeric()
        for (i in 2:(length(conteos)-2)){
          ordenadas[i-1]<-as.numeric(lm(log(conteos[i:(i+2)]-conteos[i-1]+1)~ log(1:3))$coefficients[1])
        }
        data.frame(ordenadas,fecha=fechas[4:length(fechas)],pais=rep(x,length(ordenadas)),stringsAsFactors = F)    
      }else{
        data.frame()
      }
    }else{
      data.frame()
    }  
  }else{
    data.frame()
  }
}

#Aplicación de la función y variables adicionales
paises<-unique(agregados$Pais)
paises<-paises[paises!="Others"]
ordenadas<-bind_rows(lapply(paises,obt_ordenada))

hoy<-ordenadas[ordenadas$fecha==max(ordenadas$fecha),]
conteo_paises<-as.data.frame(table(ordenadas$pais))
names(conteo_paises)<-c("pais","conteo")
hoy<-merge(hoy,conteo_paises,by="pais",all.x=T)
hoy$conteo<-hoy$conteo+2
casos<-agregados[order(agregados$Casos,decreasing = T),]
casos<-casos[!duplicated(casos$Pais),c("Pais","Casos")]
names(casos)<-c("pais","casos")
hoy<-merge(hoy,casos,by="pais",all.x=T)
hoy<-hoy[order(hoy$ordenadas,decreasing = T),]

ggplot() +
  geom_line(data = ordenadas[ordenadas$pais %in% c("Mexico","Colombia","Ecuador","Brazil","Argentina","Peru","Uruguay","Chile","Poland"),], aes(x = fecha, y = ordenadas, color = pais), size = 1)+
  xlab("x axis") +
  ylab("y axis")

ggplot() +
  geom_line(data = ordenadas[ordenadas$pais %in% c("Australia","Brazil","Belgium"),], aes(x = fecha, y = ordenadas, color = pais), size = 1)+
  xlab("x axis") +
  ylab("y axis")

