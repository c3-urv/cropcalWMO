library(shiny)
library(tidyverse)

iternormRCL=function(infolder='./data_qc',outfolder='./koppen/',statname='./data_qc/stations.txt',fyear=1900,lyear=2200,mindata=3650){
# OBJECTIVE: iterates from list the production of normals
# uses as infolder data_qc
# relies on a station list with the first column listing the code/name of the station. e.g. 650003 will read 650003.txt
# output: ./koppen	
  
  if(dir.exists((outfolder))){ unlink(outfolder, recursive = TRUE)} #removes previous existing versions
  if (!dir.exists(outfolder)) {dir.create(outfolder) }
  nyu<-nchar(outfolder)
  if(substring(outfolder,nyu,nyu)!='/'){outfolder<-paste0(outfolder,'/')} #genious
  
  if(!dir.exists(infolder)){return('infolder does not exist!')}
  nyu<-nchar(infolder)
  if(substring(infolder,nyu,nyu)!='/'){infolder<-paste0(infolder,'/')} #genious
  
  stats<-read.table(statname,header=TRUE)
  listo<-stats[,1]
	j<-nrow(stats)
	for (i in 1:j){
	  plotty<-listo[i]
		print(noquote(paste('Working with series ',listo[i],sep='')))
		nombre<-paste0(infolder,listo[i],'.txt')
		if(file.exists(nombre)){
		series<-read.table(nombre,na.strings=c('-99.9','-99.90'))		
		target<-which(series[,1] >=fyear & series[,1] <= lyear)
		series<-series[target,]
		good<-length(which(!is.na(series[,4]) & !is.na(series[,5]) & !is.na(series[,6])))
		if(good >= mindata){
		bocata<-normy(series,paste0(outfolder,plotty,'.jpg'),paste0(outfolder,'normals_',listo[i],'.txt'))
		}
		}
}
}

normy<-function(series,filena='climograma.jpg',outnorm=climograma.txt){

# OBJECTIVE: uses an RClimdex formatted series to compute a climogram and produce a climograph. 
# PARAMETERS:
# $series: an RClidmex formatted series
# OUTPUT
# $salida: the climate normals (tx,tn,tm,pc). Each variable is preceeded by another column with 1:12,
# as needed by the function preparing the climogram
# CALLS: 
# Calls daytomonth, climograma (and produces all its objects) and koppen (and produces all its objects). 

   series<-series %>% rename(Year = 1, Month = 2, Day = 3, Rr = 4, Tx = 5, Tn = 6)  #%>% filter(Year >= input$fyear & Year <= input$lyear)
  
  
  rrnormal <- series %>% dplyr::select(Year,Month,Rr) %>% 
    group_by(Year,Month)%>% summarise(Rr = sum(Rr,na.rm = TRUE), n=n()) %>% mutate(Rr= ifelse(n>25, Rr, NA)) %>% dplyr::select(-n) %>%
    ungroup() %>% group_by(Month) %>% summarise(Rr = mean(Rr,na.rm=TRUE),n=n()) %>% dplyr::select(-n) #### perhaps add an ifelse based on number of years!
  
  
  txnormal <- series %>% dplyr::select(Year,Month,Tx) %>% 
    group_by(Year,Month)%>% summarise(Tx = mean(Tx,na.rm = TRUE), n=n()) %>% mutate(Tx= ifelse(n>25, Tx, NA)) %>% dplyr::select(-n) %>%
    ungroup() %>% group_by(Month) %>% summarise(Tx = mean(Tx,na.rm=TRUE),n=n()) %>% dplyr::select(-n) #### perhaps add an ifelse based on number of years!
  
  tnnormal <- series %>% dplyr::select(Year,Month,Tn) %>% 
    group_by(Year,Month)%>% summarise(Tn = mean(Tn,na.rm = TRUE), n=n()) %>% mutate(Tn= ifelse(n>25, Tn, NA)) %>% dplyr::select(-n) %>%
    ungroup() %>% group_by(Month) %>% summarise(Tn = mean(Tn,na.rm=TRUE),n=n()) %>% dplyr::select(-n) #### perhaps add an ifelse based on number of years!
  
  salida<-full_join(rrnormal,txnormal,by='Month') %>% full_join(tnnormal,by='Month') %>% arrange(Month) %>% 
    mutate(Tm = rowMeans(dplyr::select(., c(Tx, Tn))))
  
pepe<-climograma(salida$Tm,salida$Rr,fitxer=filena,titol=filena)
pepito<-koppen(pepe)
write.table(salida,outnorm,sep='\t',quote=FALSE,col.names=TRUE,row.names = FALSE)
}

koppen=function(tmnormal,pcnormal,ccut=-3){
	# Objectiu: determinar el tipus climatic de KOPPEN a partir d'un objecte generat per climograma
	# PARAMETRES:
	# ccut: Gabler situa el tall C/D en 0; Ahrens en -3
	# climodata: objecte generat per climograma
	if(length(which(!is.na(tmnormal)))!=12){return('Manquen dades per classificar')}
	if(length(which(!is.na(pcnormal)))!=12){return('Manquen dades per classificar')}
	pacum<-sum(pcnormal)
	anual<-mean(tmnormal)
	maxanual<-max(tmnormal)
	minanual<-min(tmnormal)	
	
# Es polar? 
	if(maxanual<0){return('EF')} # Casquet
	if(maxanual<10 & maxanual>0){return('ET')} # Tundra
# No polar? Determinem "p" per saber si es un clima "B"
	p<-2*anual+14 ## precipitacio repartida tot l'any
	orden<-order(tmnormal,decreasing=T) ## busca els sis mesos mes calids (1 a 6 d'orden)
	warmrain<-sum(pcnormal[orden[1:6]])/pacum*100	
	if(warmrain >= 70){ # 70 per cent o mes de la precipitacio cau en els mesos d'estiu
		p<-2*anual+28
	}
	coolrain<-sum(pcnormal[orden[7:12]])/pacum*100
	if(coolrain >= 70){ # 70 per cent o mes de la precipitacio cau en els mesos d'hivern 
		p<-2*anual
	}

	if( p/2>pacum/10){ # Desert
		# Es un clima tipus BW, necessitem determinar el subtipus
		if(anual >=18){return('BWh')}else{return('BWk')}
		
	}
	if( p>pacum/10){ # Estepa
		# Es un clima tipus BS, necessitem determinar el subtipus
		if(anual >=18){return('BSh')}else{return('BSk')}
	}
# Si no polar i no sec, es tropical?

	if(minanual>=18){ # Tropical
		if(min(pcnormal)>=60){return('Af')}
	  pocum<-pacum/10 # en cm
		cutpoint<-10-(pocum/25)
		if(min(pcnormal)<60 & min(pcnormal/10)< cutpoint){return('Aw')}	
		if(min(pcnormal)<60 & min(pcnormal/10)>= cutpoint){return('Am')}	
	}

### Preparing Seasonality of the precipitation (second letter of middle latitudes)

		if(orden[1]>5 & orden[1]<10){summer=c(6,7,8);winter=c(1,2,12)}else{summer=c(1,2,12);winter=c(6,7,8)}
		wetsu<-max(pcnormal[summer])		
		drywi<-min(pcnormal[winter])
		second<-'f' # if the next conditions are not met, rains all year
		if(wetsu/drywi>=10){second='w'} # Dry winters
		if(min(pcnormal[summer]/10<4) & max(pcnormal[winter])>min(pcnormal[summer]*3)){second='s'} # Dry summers

### Preparing summer qualification (third letter)

		mas10<-length(which(tmnormal>10))
		if(maxanual >= 22 & mas10 >=4){third='a'}		
		if(maxanual < 22 & mas10 >=4){third='b'}			
		if(maxanual < 22 & mas10 <4 & mas10 > 0){third='c'}
		if(minanual < -38){third='d'}		
# Si no es polar, ni sec, ni tropical, es mesotermic?
	if(minanual<18 & minanual >= ccut){ # Mesotermic # Achtuuung! Ahrens Says -3
		elregreso<-paste('C',second,third,sep='')
		return(elregreso)
	}

	if(minanual < ccut & maxanual > 10){ # Microtermic
		elregreso<-paste('D',second,third,sep='')
		return(elregreso)
return('No Classificable')	
	}
}

##################### KOPPEN ZONE

climograma<-function(tmnormal,pcnormal,titol='Climograma',position='topleft',fitxer='climograma.jpg') # realitza un climograma 
{
  # PARAMETRES
  # tmnormal serie de tempeartures mitjanes normals
  # pcnormal: serie de precipitació normal
  # titol: titol de la serie
  # position: posicio de la llegenda. Un de "bottomright", "bottom", "bottomleft","left", "topleft", "top", "topright", "right" o "center"
  # fitxer: nom del fitxer de sortida
  jpeg(fitxer) # REDIRIGEIX OUTPUT A JPG
  plot.new= TRUE # COMENCEM UNA GRAFICA NOVA
  
  meses <- c("J","F","M","A","M","J","J","A","S","O","N","D")
  
  titulo <- titol
  anual<-round(mean(tmnormal,na.rm=TRUE),1)
  range<-round(max(tmnormal,na.rm=TRUE)-min(tmnormal,na.rm=TRUE),1)
  pacum<-round(sum(pcnormal,na.rm=TRUE),1)
  
  subtitol=paste('Tm Year: ',round(anual,1),' C. Range: ',round(range,1), ' C. Annual Acum Precip: ', pacum,' mm.', sep=' ')
  mintemp<-round(min(tmnormal,na.rm=TRUE)-1)*2
  maxtemp<-round(max(tmnormal,na.rm=TRUE)+1)*2
  criterio=max(maxtemp,max(pcnormal,na.rm=TRUE))
  valores<-seq(mintemp,criterio,length.out=10)
  if(valores[1]!=0){valores[1]=valores[1]-valores[1]%%10-10}
  if(valores[10]!=0){valores[10]=valores[10]-valores[10]%%10+10}
  valores<-seq(valores[1],valores[10],by=10)
  sortida<-list(tmnormal=tmnormal,pcnormal=pcnormal,anual=anual,range=range,pacum=pacum)
  tipo<-koppen(tmnormal,pcnormal)
  dev.off
  x<-barplot(pcnormal, axes = FALSE, main = titulo , sub=paste(subtitol,'.  Tipo: ',tipo,sep=''),
             ylim=c(min(valores[1],0),valores[length(valores)]),cex.main=0.7,cex.sub=0.8,adj=0,col='skyblue2')
  
  ## This was the old one: having ylim to valores[1] resulted in wierd behavior when climate is warm and precip low, i.e. in Dry climates
  #x<-barplot(pcnormal, axes = FALSE, main = titulo , sub=paste(subtitol,'.  Tipo: ',tipo,sep=''),
  #           ylim=c(valores[1],valores[length(valores)]),cex.main=0.7,cex.sub=0.8,adj=0,col='skyblue2')
  
  
  precval<-subset(valores,valores >= 0)
  
  ### new to try to avoid weird axis when precip is low (does not work)
  if(precval[1]!=0){precval=seq(0,max(precval),10)}
  
  axis(side=4, at = precval, lab=round(precval))
  axis(side=2, at = valores, lab = round(valores/2))
  axis(side=1, at = x, lab = meses)	
  
  lines(x,tmnormal*2,col="red",lwd=3)
  points(x,tmnormal*2,col="red",cex=2,pch=19)
  
  legend(x=position,legend=c("TM","PC"),lty=c(1,1),lwd=c(2,4),col=c("red","skyblue2"),cex=0.75)
  
  # sortida<-list(txnormal=txnormal,tnnormal=tnnormal,tmnormal=tmnormal,pcnormal=pcnormal,anual=anual,range=range,pacum=pacum)
  
  
  dev.off()
  #return(sortida)
}





daytomonth=function(x,y,z,feos='NA',allow=5,compile=1,write=0,filename='monthly.txt'){
# OBJECTIVE: computes monthly series from rclimdex-style data
# and places the result in nx13 grid
# PARAMETERS: 
# x: vector with year values
# y: vector with month values
# z: vector with data
# feos: missing value
# allow: percentage of missing tolerated to compute the mean
# compile: how will you compile the monthly vals: 1 = mean; 2 = sum
# write: 1 will produce a data file; other will do nothing
# filename: filename to be used if write is 1
# RETURNS:
# t: nx13 monthly means grid
datos<-cbind(x,y,z)
diasmes<-c(31,28,31,30,31,30,31,31,30,31,30,31)
x1<-min(x,na.rm=TRUE) # first year
x2<-max(x,na.rm=TRUE) # second year
x3<-x2-x1+1 # number of years
t<-array(dim=c(x3,13))
t[,1]<-x1:x2	

	for(i in x1:x2){
#		print(i)
		for(j in 1:12){
#			print(j)
			validos<-subset(datos[,3],datos[,1]==i & datos[,2]==j & datos[,3]!=feos)
			if(j==2 & i%%4==0 & i!=1900){bisiesto=1}else{bisiesto=0}
			contraste<-diasmes[j]+bisiesto
			permiso<-contraste-round(allow*contraste/100,0)
			if(length(validos)>0){	
			if(permiso<=length(validos)){
					if(compile==1){valor<-mean(validos,na.rm=TRUE)}
					if(compile==2){valor<-sum(validos,na.rm=TRUE)}
					}else{valor<-NA}
					}else{valor<-NA}
			fila<-i-x1+1
			columna<-j+1
			if(!is.na(valor)){t[fila,columna]<-round(valor,1)}else{t[fila,columna]<-NA} # not necessary. Learned NA rounds to NA!!!
		}
	
	}

if(write==1){write.table(t,file=filename,quote = FALSE, sep = '\t',row.names = FALSE,col.names = FALSE,na='-999.9')}
return(t)
}



calnorm<-function(series,pany=1971,fany=2000,perdua=5,fitxer='normals.txt'){ # calcula normals a partir de series mensuals
	
	# PARAMETRES
	# series: serie: serie de dades en format grid (13 files x n columnes, primera columna any
	# fitxer: nom del fitxer per escritura de dades. Defecte: normals.txt	
	# pany: primer any pel calcul de normals (passat). Per defecte = 1971
	# fany: darrer any pel calcul de normals (present). Per defecte = 2000
	# perdua: percentatge de valors perduts per comput de normal. Defecte, 5.

	normals<-apply(series[,2:13],2,mean,na.rm=T) 
	for(k in 2:13){ 
		dummy<-series[,k]
		presents<-which(!is.na(dummy))  
		presents<-length(presents)     
		potencials<-fany-pany+1	       
		perduts<-potencials-presents	

		tall<-perduts/potencials	 
		tall<-tall*100			
		if(tall > perdua){normals[(k-1)]<-NA}			
	}
	sortida<-cbind(1:12,round(normals,1)) 
	write.table(sortida,fitxer,quote=F,sep='\t',row.names=F,col.names=F)  
	return(sortida)
}





# Define UI
ui <- fluidPage(
  titlePanel("Climate Normals and Köppen Classification"),
  fluidRow(
    column(width = 3, 
       textInput("statname", "Station List", value = "../assessed/stations.txt"),
      textInput("infolder", "Input Folder", value = "../assessed"),
      textInput("outfolder", "Output Folder", value = "../koppen"),
      numericInput("fyear", "Start Year", value = 1900),
      numericInput("lyear", "End Year", value = 2200),
      numericInput("mindata", "Minimum Data", value = 3650),
      actionButton("runButton", "Run All Stations to Disk"),
      actionButton("readStationsButton", "Load Normals and Plots to Screen"),
      textOutput("outputText"),
      actionButton('kietoparao', 'Stop App'),
    ),
    
    column(width =2,
      radioButtons("selectedStations", "Select Station To Show", choices="",selected = character(0)),
    ),
    column(width =3,
           tableOutput("table"),
    ),
    column(width =3,
           imageOutput("image"),
    )
    
  )
)

# Define server
server <- function(input, output, session) {
  # Read and display stations file
  stations_data <- reactiveVal(NULL)
  
  observeEvent(input$kietoparao, {
    stopApp()
  })
  
  observeEvent(input$runButton, {
    outputText <- isolate({
     iternormRCL(
       infolder = input$infolder,
       outfolder = input$outfolder,
       statname = input$statname,
       fyear = input$fyear,
       lyear = input$lyear,
       mindata = input$mindata
     )
    })
    
    output$outputText <- renderText({
      paste("Analysis completed. Check the output in", input$outfolder)
    })
  })
  
  observeEvent(input$readStationsButton, {
    req(input$statname)
    stations_data(read.table(input$statname, header = TRUE)$ID)
    updateRadioButtons(session, "selectedStations", choices = stations_data())
  })

  observeEvent(input$selectedStations, {
    selectedStation <- input$selectedStations
      nombre <- paste0(input$outfolder, '/normals_', selectedStation, '.txt')
      x<-read_table(nombre) 
      meses<-c('J', 'F','M','A','M','J','J','A','S','O','N','D')
      x<- x%>% mutate(Month = meses)
      output$table <- renderTable({
        x
      })

      
      img_path <- paste0(input$outfolder, '/', selectedStation, '.jpg')
      output$image <- renderImage({
        list(src = img_path, alt = "Climograph")
      }, deleteFile = FALSE)
          
  })

  
 
  }



# Run the Shiny app
shinyApp(ui, server)
