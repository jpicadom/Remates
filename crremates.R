setwd("C:/Users/Jonathan/Dropbox/FamPicadoGomez")
paquetes <- c("XML","ggplot2","RJSONIO","stringr","jsonlite","dplyr","rvest","cluster","lubridate",
              "data.table","tidyr","filesstrings","gsubfn","httr","lattice","maps","RColorBrewer",
              "xlsx","plyr","tidyverse","caret","timeDate","formattable","RCurl","quantmod")
lapply(paquetes, require, character.only = TRUE) # Carga las librerias necesarias

links <- character()
enlaces<-character()
SUP<-150000000 # Monto Maximo de filtro de remates
INF<-1000000 # Monto Minimo de filtro de remates
lugares<-list("Upala","Sarapiquí","San Carlos","Guatuso","Pérez Zeledón","Jiménes","Orotina",
           "Valverde Vega","Puriscal","San Mateo","Los Chiles")

options(encoding="utf-8")

# ## Ingresar con el usuario y password -> necesita mejorarse
iniciaSesion <-"http://www.crremates.com/home/505.php" 
options(RDataCollectionLogin="jpicado3@costarricense.cr:john1051")
getURL(iniciaSesion,userpwd=getOption("RDataCollectionLogin"),followlocation=TRUE)

# Descargar las tablas por provincia del area metropolitana
# totProvincias<-4
# tablaResumen<-data.frame()
# ruta<-"http://www.crremates.com/home/141.php?prov="
# listaRemates<-list()
# for(i in 1:totProvincias){
#   url <- paste0(ruta,i)
#   download.file(url,destfile ="rematesResumen.xml")
#   tablas <- readHTMLTable("rematesResumen.xml",header = TRUE, stringsAsFactors=FALSE)
#   tablaResumen <- rbind(tablaResumen,tablas[[3]]) # 3ra tabla contiene los datos
#   tablaResumen<-tablaResumen[-1,] # remueve los nombres de la primera fila
#   enlaces<-url %>% read_html() %>% html_nodes("td") # Extrae atributos en td
#   urlRemate <- lapply(enlaces,function (x) {str_extract(x,"=\\d{5,7}")}) # Extrae el numero
#   urlRemate <- lapply(urlRemate,function (x) {str_extract(x,"\\d{5,7}")}) # Remueve el "="
#   urlRemate<- urlRemate[-which(is.na(urlRemate[]))] # Remueve los "na"
#   urlRemate<-unique(urlRemate)
#   listaRemates <- c(listaRemates,urlRemate)
# }
# listaRemates<-as.numeric(listaRemates)
# tablaResumen$V1<-listaRemates
# str(tablaResumen)
# head(tablaResumen)


# Extraccion DETALLADA de datos por numero de remate
# Lista de remates para las 4 provincias
totProvincias<-4
ruta<-"http://www.crremates.com/home/141.php?prov="
listaRemates<-list()
for(i in 1:totProvincias){
  url <- paste0(ruta,i)
  enlaces<-url %>% read_html() %>% html_nodes("td") # Extrae atributos en td
  urlRemate <- lapply(enlaces,function (x) {str_extract(x,"=\\d{5,7}")}) # Extrae el numero
  urlRemate <- lapply(urlRemate,function (x) {str_extract(x,"\\d{5,7}")}) # Remueve el "="
  urlRemate<- urlRemate[-which(is.na(urlRemate[]))] # Remueve los "na"
  urlRemate<-unique(urlRemate)
  listaRemates <- c(listaRemates,urlRemate)
}
listaRemates<-as.numeric(listaRemates)

#Extraer la informacion por numero de remate
getCurlOptionsConstants()[["connecttimeout"]]
myOpts <- curlOptions(connecttimeout=10000)
ruta<-"http://www.crremates.com/home/112.php?cod_remate="
tablaDetalle<-data.frame()
for(i in 1:length(listaRemates)){
  url <- paste0(ruta,listaRemates[i])
  download.file(url,destfile ="rematesDetalle.xml")
  tablas <- readHTMLTable("rematesDetalle.xml",header = TRUE, stringsAsFactors=FALSE)
  tablaPropiedad<-tablas[[1]]
  num<-listaRemates[i]
  tipo<-tablaPropiedad[9,2]
  fecha<-tablaPropiedad[6,2]
  hora<-tablaPropiedad[5,2]
  base<-tablaPropiedad[8,2]
  juzgado<-tablaPropiedad[7,2]
  provincia<-tablaPropiedad[13,2]
  canton<-tablaPropiedad[14,2]
  distrito<-tablaPropiedad[15,2]
  finca<-tablaPropiedad[10,2]
  terreno<-tablaPropiedad[17,2]
  linderos<-tablaPropiedad[12,2]
  acreedor<-tablaPropiedad[21,2]
  deudor<-tablaPropiedad[23,2]
  expediente<-tablaPropiedad[3,2]
  expediente<-str_extract(expediente,"\\d{2}-\\d{6}-\\d{4}-\\w{2}")
  consolidado<-cbind.data.frame(num,tipo,finca,fecha,hora,base,juzgado,provincia,canton,distrito,terreno,linderos,acreedor,
                    deudor,expediente)
  tablaDetalle <- rbind(tablaDetalle,consolidado) # 3ra tabla contiene los datos
}

# Limpieza de datos
# for(i in 2:9) {
#   names(tablaResumen) <- c("Remate","Fecha", "Provincia", "Canton", "Distrito","mCuadrados", "Monto", "Acreedor","Colones")
# }
tablaResumen<-tablaDetalle
tablaResumen$terreno <- lapply(tablaResumen$terreno,function (x) {gsub("[mÃ‚Â²]","",x)}) # Extrae el numero
tablaResumen$terreno<-as.numeric(gsub(",","",tablaResumen$terreno)) # convierte el dato a numero
tablaResumen$base<-lapply(tablaResumen$base, function (x) {gsub("\\Â","",x)})# quita caracteres especiales del monto
tablaResumen$fecha<-lapply(tablaResumen$fecha, function (x) {gsub("\\Â","",x)})# quita caracteres especiales de la fecha
tablaResumen$canton<-repair_encoding(tablaResumen$canton) # corrije las tildes
tablaResumen$distrito<-repair_encoding(tablaResumen$distrito) # corrije las tildes
tablaResumen$acreedor<-repair_encoding(tablaResumen$acreedor) # corrije las tildes
tablaResumen$deudor<-repair_encoding(tablaResumen$deudor) # corrije las tildes
tablaResumen$linderos<-repair_encoding(tablaResumen$linderos) # corrije las tildes
encoding(tablaResumen$linderos)<-"UTF-8"
tablaResumen$juzgado<-repair_encoding(tablaResumen$juzgado) # corrije las tildes
tail(tablaResumen)

last(getFX("CRC/USD",date=(Sys.Date())))[[1]] # Extrae el tipo de cambio del ultimo dia
tipoCambio<- last(CRCUSD)[[1]]
#tipoCambio<-1/572
convierteDolares<- function(x){
  options(scipen = 999)
  if(str_sub(x,start=1,end = 1) =="$" & !is.na(x)==T){
    m<-as.numeric(gsub("[$,]","",as.character(x))) / tipoCambio
  }
  else {
    m<-as.numeric(gsub("[¢,]","",as.character(x))) 
  }
}

# Precio en colones del remate
tablaResumen$colones <- lapply(unlist(tablaResumen$base),function(x) {
  ifelse((str_sub(x,start=1,end = 1) =="¢" & !is.na(x)==T),
         m<-as.numeric(gsub("[¢,]","",as.character(x))),       
         m<-as.numeric(gsub("[$,]","",as.character(x))) / tipoCambio)})
tablaResumen$colones <- formatC(unlist(tablaResumen$colones), digits = 2,format="f")
tablaResumen$colones<- as.numeric(tablaResumen$colones)
tablaResumen<- tablaResumen %>% filter(!is.na(colones)) # Remueve valores NA
tablaResumen$base<-as.character(tablaResumen$base) # Convierte a caracter el monto
tablaResumen$fecha<- gsub("  ","",tablaResumen$fecha) #Remueve los espacios en blanco
tablaResumen$fecha<- as.Date(tablaResumen$fecha,format="%d/%m/%Y") #Convierte a formato fecha

# Crear tabla de enfoque.  Filtrado de datos.
tablaFoco<- tablaResumen %>% filter(between(colones,INF,SUP)) %>% arrange(colones) # Tabla con montos<LIM
tablaFoco$distrito<-as.list(tablaFoco$distrito)
tablaFoco <- tablaFoco[!tablaFoco$canton %in% lugares, ] # Excluye los lugares alejados
tablaFoco$distrito<-as.character(tablaFoco$distrito)
hoy<-Sys.Date()
tablaFoco<- tablaFoco %>% filter(fecha>hoy) # Excluye remates antiguos
tablaFoco<- tablaFoco %>% arrange(fecha)
#http://www.crremates.com/home/112.php?cod_remate=64476
write.xlsx(tablaFoco,file = "C:\\Users\\Jonathan\\Dropbox\\FamPicadoGomez\\ResumenRemates2.xlsx",
           row.names = FALSE,col.names = TRUE)
library(xlsx)
