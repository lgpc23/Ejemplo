rm(list=ls())


directorio <- "C:/Users/guillermo/Desktop/Universidad/2020/2020-1/Macro y Finanzas/Taller II/Punto III"
setwd(directorio)





require(rvest)
require(magrittr)
## Loading required package: magrittr
url <- "http://www.banguat.gob.gt/participaciones/envolver.asp?karchivo=140119"
# we save in the variable url the website url.
pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
#We create a function with read_html to read the web page.
pagina %>%  
  html_nodes("table") %>% 
  #Here, we indicate that this is the table we want to extract.
  .[[5.5]] %>% 
  #Here we put of which table of the HTML is about, in our example it is the third table of the web.
  html_table(fill=T) -> t
#We save it in a CSV.
View(t)



x<- rbind(x,y)




library(readxl)
lista <- read_excel("C:/Users/guillermo/Desktop/Universidad/2020/2020-1/Macro y Finanzas/Taller II/Punto VIII/lista días.xlsx", 
                    col_names = FALSE)

lista$...2<- NULL

lista <-sub(".+?-", "", lista$...1)

lista <- na.omit(lista)
lista<- data.frame(lista)


lista$año<- lista$lista
lista$año <-sub(".+? ", "", lista$año)
lista$año <-sub(".+? ", "", lista$año)
lista$año <-sub(".+? ", "", lista$año)
lista$año <-sub(".+? ", "", lista$año)
lista$año <-sub(".+? ", "", lista$año)



  
  lista$lista <-sub(" 20\\S*", "", lista$lista)

  




lista$dia <- ifelse(grepl("01", lista$lista) ,"01", "" )



for(i in 1:31) {
 
  lista$dia[grepl(i, lista$lista)] <- i
  
  
}

lista$dia[grepl("01", lista$lista)] <- "01"
lista$dia[grepl("02", lista$lista)] <- "02"
lista$dia[grepl("03", lista$lista)] <- "03"
lista$dia[grepl("04", lista$lista)] <- "04"
lista$dia[grepl("05", lista$lista)] <- "05"
lista$dia[grepl("06", lista$lista)] <- "06"
lista$dia[grepl("07", lista$lista)] <- "07"
lista$dia[grepl("08", lista$lista)] <- "08"
lista$dia[grepl("09", lista$lista)] <- "09"

lista$dia[grepl(" 1 ", lista$lista)] <- "01"
lista$dia[grepl(" 2 ", lista$lista)] <- "02"
lista$dia[grepl(" 3 ", lista$lista)] <- "03"
lista$dia[grepl(" 4 ", lista$lista)] <- "04"
lista$dia[grepl(" 5 ", lista$lista)] <- "05"
lista$dia[grepl(" 6 ", lista$lista)] <- "06"
lista$dia[grepl(" 7 ", lista$lista)] <- "07"
lista$dia[grepl(" 8 ", lista$lista)] <- "08"
lista$dia[grepl(" 9 ", lista$lista)] <- "09"


str_length(lista$fecha)<6

meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
numero<- c(1:12)
for (i in meses) {
  
  lista$mes[grepl(i, lista$lista)] <- i
  
}


lista$mes[grepl("enero", lista$lista)] <- "01"
lista$mes[grepl("diciembre", lista$lista)] <- "12"
lista$mes[grepl("febrero", lista$lista)] <- "02"
lista$mes[grepl("marzo", lista$lista)] <- "03"
lista$mes[grepl("abril", lista$lista)] <- "04"
lista$mes[grepl("mayo", lista$lista)] <- "05"
lista$mes[grepl("junio", lista$lista)] <- "06"
lista$mes[grepl("julio", lista$lista)] <- "07"
lista$mes[grepl("agosto", lista$lista)] <- "08"
lista$mes[grepl("septiembre", lista$lista)] <- "09"
lista$mes[grepl("octubre", lista$lista)] <- "10"
lista$mes[grepl("noviembre", lista$lista)] <- "11"


lista$año <- as.numeric(lista$año)

lista$año <- lista$año-2000
lista$año <- as.factor(lista$año)


lista$mes <- as.factor(lista$mes)

lista$fecha<- paste(lista$dia, lista$mes, sep = "")

lista$fecha<- paste(lista$fecha, lista$año, sep = "")





fecha<- lista$fecha




require(rvest)
require(magrittr)
## Loading required package: magrittr
url <- "http://www.banguat.gob.gt/participaciones/envolver.asp?karchivo=030420"
# we save in the variable url the website url.
pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
#We create a function with read_html to read the web page.
pagina %>%  
  html_nodes("table") %>% 
  #Here, we indicate that this is the table we want to extract.
  .[[5]] %>% 
  #Here we put of which table of the HTML is about, in our example it is the third table of the web.
  html_table(fill=T) -> x
#We save it in a CSV.
View(x)

x<-NULL

for (i in fecha) {
  
  url<- "http://www.banguat.gob.gt/participaciones/envolver.asp?karchivo="
  url<- paste(url, i, sep = "")
  print(url)
  

require(rvest)
require(magrittr)

pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)

pagina %>%  
  html_nodes("table") %>% 
  
  .[[5]] %>% 
  
  html_table(fill=T) -> i



x<- dplyr::bind_rows(x, i)

i<- NULL


  
}


no_posturas <-sum(x$X3=="No se recibieron posturas" & x$X4=="No se recibieron posturas" )


# 2019 --------------------------------------------------------------------


listas<- rep(2019, 374)
o<- c(1:9)
b<-c(10:99)

o<- paste0("00", o)
b<-paste0("0", b)

c<- c(100:374)

b<-append (o, b)
b<-append (b, c)

View(b)

listas<- paste(b, listas, sep="-")

actual<- NULL
for (i in listas) {
  
  i<- x[grep(i, x$X2),] 
  actual<- dplyr::bind_rows(actual, i)
    
}



actual2<- actual[165:314,]
actual<- actual[-c(165:314),]

names(actual)[2] <- "Número de Subasta"
names(actual)[4] <- "Postura Monto"
names(actual)[5] <- "Postura Cambio Mínimo"
names(actual)[6] <- "Postura Cambio Máximo"
names(actual)[7] <- "Adjudicación Monto"
names(actual)[8] <- "Adjudicación Tipo de Cambio de Corte"
names(actual)[9] <- "Adjudicación Tipo de Cambio Ponderado"


actual<-actual[,-c(1,3,10)]


sum(actual$`Postura Cambio Mínimo`=="No se recibieron posturas")



names(actual2)[2] <- "Número de Subasta"
names(actual2)[3] <- "Postura Monto"
names(actual2)[4] <- "Postura Cambio Mínimo"
names(actual2)[5] <- "Postura Cambio Máximo"
names(actual2)[6] <- "Adjudicación Monto"
names(actual2)[7] <- "Adjudicación Tipo de Cambio de Corte"
names(actual2)[8] <- "Adjudicación Tipo de Cambio Ponderado"


actual2<-actual2[,-c(1,9,10)]

actual<- rbind(actual, actual2)

actual<- actual[-grep("No se recibieron posturas", actual$`Postura Cambio Mínimo`),]



actual[is.na(actual)] <- 0

actual<-actual[!duplicated(actual$`Número de Subasta`), ]

library(tidyverse)

str(actual)



# 2018 --------------------------------------------------------------------


listas<- rep(2018, 359)
o<- c(1:9)
b<-c(10:99)

o<- paste0("00", o)
b<-paste0("0", b)

c<- c(100:359)

b<-append (o, b)
b<-append (b, c)

View(b)

listas<- paste(b, listas, sep="-")

actual8<- NULL
for (i in listas) {
  
  i<- x[grep(i, x$X1),] 
  actual8<- dplyr::bind_rows(actual8, i)
  
}

names(actual8)[1] <- "Número de Subasta"
names(actual8)[2] <- "Postura Monto"
names(actual8)[3] <- "Postura Cambio Mínimo"
names(actual8)[4] <- "Postura Cambio Máximo"
names(actual8)[5] <- "Adjudicación Monto"
names(actual8)[6] <- "Adjudicación Tipo de Cambio de Corte"
names(actual8)[7] <- "Adjudicación Tipo de Cambio Ponderado"



actual8<-actual8[,-c(8:10)]


actual82<- NULL
for (i in listas) {
  
  i<- x[grep(i, x$X2),] 
  actual82<- dplyr::bind_rows(actual82, i)
  
}

actual82<-actual82[,-c(1,3,10)]

names(actual82)[1] <- "Número de Subasta"
names(actual82)[2] <- "Postura Monto"
names(actual82)[3] <- "Postura Cambio Mínimo"
names(actual82)[4] <- "Postura Cambio Máximo"
names(actual82)[5] <- "Adjudicación Monto"
names(actual82)[6] <- "Adjudicación Tipo de Cambio de Corte"
names(actual82)[7] <- "Adjudicación Tipo de Cambio Ponderado"


actual8<- rbind(actual8, actual82)

actual8<-actual8[!duplicated(actual8$`Número de Subasta`), ]


# 2016 --------------------------------------------------------------------



listas<- rep(2016, 510)
o<- c(1:9)
b<-c(10:99)

o<- paste0("", o)
b<-paste0("", b)

c<- c(100:468)

b<-append (o, b)
b<-append (b, c)


listas<- paste(b, listas, sep="-")


listas<- c()

actual62<- NULL
for (i in listas) {
  
  i<- x[grep(i, x$X1),] 
  actual62<- dplyr::bind_rows(actual62, i)
  
}



actual62<-actual62[,-c(8:10)]

names(actual62)[1] <- "Número de Subasta"
names(actual62)[2] <- "Postura Monto"
names(actual62)[3] <- "Postura Cambio Mínimo"
names(actual62)[4] <- "Postura Cambio Máximo"
names(actual62)[5] <- "Adjudicación Monto"
names(actual62)[6] <- "Adjudicación Tipo de Cambio de Corte"
names(actual62)[7] <- "Adjudicación Tipo de Cambio Ponderado"


actual62<-actual62[!duplicated(actual62$`Número de Subasta`), ]



actual62<-actual62[order(actual62$`Número de Subasta`),]

# 2017 --------------------------------------------------------------------



listas<- rep(2017, 468)
o<- c(1:9)
b<-c(10:99)

o<- paste0("00", o)
b<-paste0("0", b)

c<- c(100:468)

b<-append (o, b)
b<-append (b, c)

View(b)

listas<- paste(b, listas, sep="-")

actual7<- NULL
for (i in listas) {
  
  i<- x[grep(i, x$X1),] 
  actual7<- dplyr::bind_rows(actual7, i)
  
}



actual7<-actual7[,-c(8:10)]

names(actual7)[1] <- "Número de Subasta"
names(actual7)[2] <- "Postura Monto"
names(actual7)[3] <- "Postura Cambio Mínimo"
names(actual7)[4] <- "Postura Cambio Máximo"
names(actual7)[5] <- "Adjudicación Monto"
names(actual7)[6] <- "Adjudicación Tipo de Cambio de Corte"
names(actual7)[7] <- "Adjudicación Tipo de Cambio Ponderado"





# 2020 --------------------------------------------------------------------



listas<- rep(2020, 153)
o<- c(1:9)
b<-c(10:99)

o<- paste0("00", o)
b<-paste0("0", b)

c<- c(100:468)

b<-append (o, b)
b<-append (b, c)

View(b)

listas<- paste(b, listas, sep="-")

actual2<- NULL
for (i in listas) {
  
  i<- x[grep(i, x$X2),] 
  actual2<- dplyr::bind_rows(actual2, i)
  
}


actual2<-actual2[,-c(1,9,10)]

names(actual2)[1] <- "Número de Subasta"
names(actual2)[2] <- "Postura Monto"
names(actual2)[3] <- "Postura Cambio Mínimo"
names(actual2)[4] <- "Postura Cambio Máximo"
names(actual2)[5] <- "Adjudicación Monto"
names(actual2)[6] <- "Adjudicación Tipo de Cambio de Corte"
names(actual2)[7] <- "Adjudicación Tipo de Cambio Ponderado"


datos<- rbind(actual62, actual7)
datos<-rbind(datos, actual8)
datos<- rbind(datos, actual)
datos<- rbind(datos, actual2)


datos[is.na(datos)] <- 0

datos<-datos[!duplicated(datos$`Número de Subasta`), ]



actual2<-rbind(actual2, actual)
actual2<- rbind(actual2, actual8)
actual2<-rbind(actual2, actual7)
actual2<-rbind(actual2, actual62)

actual2<- actual2[-grep("No se recibieron posturas", actual2$`Postura Cambio Mínimo`),]


actual2[is.na(actual2)] <- 0

actual2<-actual2[!duplicated(actual2$`Número de Subasta`), ]

datos<- na.omit(datos)

# ff ----------------------------------------------------------------------

library(stringr)
fecha<- as.data.frame(fecha)

fecha$prueba<-str_sub(fecha$fecha,-2,-1)

subasta<- actual2$`Número de Subasta`

fecha<- fecha[fecha$prueba==20,]
fecha<- fecha$fecha



for (i in subasta){
  for (z in fecha){
    
    
    
      
      url<- "http://www.banguat.gob.gt/participaciones/envolver.asp?karchivo="
      url<- paste(url, z, sep = "")
      print(url)
      
      
      require(rvest)
      require(magrittr)
      
      pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
      
      pagina %>%  
        html_nodes("table") %>% 
        
        .[[5]] %>% 
        
        html_table(fill=T) -> i
      
      
      
      x<- dplyr::bind_rows(x, i)
      
      i<- NULL
    
    
    
  }
  
}





library("xlsx")


write.xlsx(datos, file="guat2020.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


library(readxl)
 guat2020 <- read_excel("guat2020.xlsx")


str(guat2020)
guat2020<- na.omit(guat2020)
guat2020$`Adjudicación Monto`<-as.numeric(guat2020$`Adjudicación Monto`)
guat2020$indice<- c(1:810)
guat2020$`Adjudicación Tipo de Cambio Ponderado`<-as.numeric(guat2020$`Adjudicación Tipo de Cambio Ponderado`)


# 2016 --------------------------------------------------------------------

library(readxl)
guat2020 <- read_excel("guat2020.xlsx")
guat2020$indice<- c(1:997)
guat2020<- filter(guat2020, guat2020$indice< 157)

guat2020$`Adjudicación Monto`<-as.numeric(guat2020$`Adjudicación Monto`)

guat2020$`Adjudicación Tipo de Cambio Ponderado`<-as.numeric(guat2020$`Adjudicación Tipo de Cambio Ponderado`)
guat2020<- na.omit(guat2020)

library(ggplot2)

p16<-ggplot(guat2020, aes(x=`indice`, y=`Adjudicación Tipo de Cambio Ponderado`)) +
  geom_line(color="#69b3a2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust=0.5))+
  labs(y = "Quetzales por Dólar",  x = "Número de Subasta", caption = "Fuente: Banco de Guatemala.  ", colour= "Serie")+
  ggtitle("Adjudicación Tipo de Cambio Ponderado \n2016")

# 2017 --------------------------------------------------------------------

library(readxl)
guat2020 <- read_excel("guat2020.xlsx")
guat2020$indice<- c(1:997)
guat2020<- filter(guat2020, guat2020$indice> 157 & guat2020$indice< 471 )
guat2020<- na.omit(guat2020)
guat2020$`Adjudicación Monto`<-as.numeric(guat2020$`Adjudicación Monto`)

guat2020$`Adjudicación Tipo de Cambio Ponderado`<-as.numeric(guat2020$`Adjudicación Tipo de Cambio Ponderado`)
guat2020<- na.omit(guat2020)

library(ggplot2)

p17<-ggplot(guat2020, aes(x=`indice`, y=`Adjudicación Tipo de Cambio Ponderado`)) +
  geom_line(color="#69b3a2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust=0.5))+
  labs(y = "Quetzales por Dólar",  x = "Número de Subasta", caption = "Fuente: Banco de Guatemala.  ", colour= "Serie")+
  ggtitle("Adjudicación Tipo de Cambio Ponderado \n2017")
# 2018 --------------------------------------------------------------------

library(readxl)
guat2020 <- read_excel("guat2020.xlsx")
guat2020$indice<- c(1:997)
guat2020<- filter(guat2020, guat2020$indice> 471 & guat2020$indice< 621 )
guat2020<- na.omit(guat2020)
guat2020$`Adjudicación Monto`<-as.numeric(guat2020$`Adjudicación Monto`)

guat2020$`Adjudicación Tipo de Cambio Ponderado`<-as.numeric(guat2020$`Adjudicación Tipo de Cambio Ponderado`)
guat2020<- na.omit(guat2020)

library(ggplot2)

p18<-ggplot(guat2020, aes(x=`indice`, y=`Adjudicación Tipo de Cambio Ponderado`)) +
  geom_line(color="#69b3a2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust=0.5))+
  labs(y = "Quetzales por Dólar",  x = "Número de Subasta", caption = "Fuente: Banco de Guatemala.  ", colour= "Serie")+
  ggtitle("Adjudicación Tipo de Cambio Ponderado \n2018")
# 2019 --------------------------------------------------------------------


library(readxl)
guat2020 <- read_excel("guat2020.xlsx")
guat2020$indice<- c(1:997)
guat2020<- filter(guat2020, guat2020$indice> 621 & guat2020$indice< 885 )
guat2020<- na.omit(guat2020)
guat2020$`Adjudicación Monto`<-as.numeric(guat2020$`Adjudicación Monto`)

guat2020$`Adjudicación Tipo de Cambio Ponderado`<-as.numeric(guat2020$`Adjudicación Tipo de Cambio Ponderado`)
guat2020<- na.omit(guat2020)

library(ggplot2)

p19<-ggplot(guat2020, aes(x=`indice`, y=`Adjudicación Tipo de Cambio Ponderado`)) +
  geom_line(color="#69b3a2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust=0.5))+
  labs(y = "Quetzales por Dólar",  x = "Número de Subasta", caption = "Fuente: Banco de Guatemala.  ", colour= "Serie")+
  ggtitle("Adjudicación Tipo de Cambio Ponderado \n2019")

# 2020 --------------------------------------------------------------------


library(readxl)
guat2020 <- read_excel("guat2020.xlsx")
guat2020$indice<- c(1:997)
guat2020<- filter(guat2020, guat2020$indice> 885 & guat2020$indice< 998 )
guat2020<- na.omit(guat2020)
guat2020$`Adjudicación Monto`<-as.numeric(guat2020$`Adjudicación Monto`)

guat2020$`Adjudicación Tipo de Cambio Ponderado`<-as.numeric(guat2020$`Adjudicación Tipo de Cambio Ponderado`)
guat2020<- na.omit(guat2020)

library(ggplot2)

p20<-ggplot(guat2020, aes(x=`indice`, y=`Adjudicación Tipo de Cambio Ponderado`)) +
  geom_line(color="#69b3a2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust=0.5))+
  labs(y = "Quetzales por Dólar",  x = "Número de Subasta", caption = "Fuente: Banco de Guatemala.  ", colour= "Serie")+
  ggtitle("Adjudicación Tipo de Cambio Ponderado \n2020")




# descriptivas ------------------------------------------------------------
library(gridExtra)
library(ggplot2)
library(grid)






ggdraw() +
  draw_plot(p16, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p17, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p18, x = 0, y = 0, width = .5, height = .5) +
  draw_plot(p19, x = .5, y = 0, width = .5, height = .5) 






