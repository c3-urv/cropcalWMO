library(shiny)
library(tidyverse)
library(zoo)
library(lubridate)
library(circular)
library(ggplot2)
library(fitdistrplus)
library(kableExtra)
library(gridExtra)
library(rhandsontable)
library(ggthemes)




# auxiliares




max_racha_posterior <- function(x) {
  rle_values <- rle(x)
  max_racha <- max(rle_values$lengths[rle_values$values == 1], default = 0)
  return(max_racha)
}

# Calcula la racha máxima para cada fecha en los próximos 30 días



max_racha <- function(x) {
  rle_values <- rle(x)
  max_racha <- max(rle_values$lengths[rle_values$values == 1], default = 0)
  return(max_racha)
}


tablones<-function(julian_days,nametag,estacion, crop, serie,outfolder, fase){
  julian_days<-as.vector(na.omit(julian_days))
  # Assuming 'season$FJULIAN' is your circular data (ranging from 0 to 365)
  #browser()
  circ_data <- circular((julian_days / 365) * 2 * pi)
  
  
  # Fit the von Mises distribution
  fit_vonmises <- mle.vonmises(circ_data)
  #print(fit_vonmises)
  
  # Convert angles to Julian days for data points
  data_points_df <- data.frame(Julian = julian_days)
  
  # Calculate the cumulative distribution function (CDF) for von Mises
  #### The kappa parameter goes ballistic if there is little dispersion in the data; then, 
  ### it refuses to provide quantiles. I am limitting it to 100, bad fit, but the gaussian does
  ## not any good either
  cdf_values <- pvonmises(circ_data, mu = fit_vonmises$mu, kappa = min(fit_vonmises$kappa,100))
  ## take the julian for plotting purposes
  cdf_df <- data.frame(Julian = julian_days, CDF_VonMises = cdf_values)
  cdf_df <- cdf_df[order(cdf_df$Julian),]
  
  # Extract quantile values for von Mises in Julian days
  quantiles_vonmises <- qvonmises(c(0.33, 0.66), mu = fit_vonmises$mu, kappa = fit_vonmises$kappa)
  quantile_df_vonmises <- data.frame(Quantile = c(0.33, 0.66), Julian = (quantiles_vonmises / (2 * pi)) * 365)
  
  # Fit the Gaussian distribution using fitdistrplus
  fit_gaussian <- fitdist(julian_days, 'norm')
  
  # Calculate the cumulative distribution function (CDF) for Gaussian
  cdf_values_gaussian <- pnorm(sort(julian_days), mean = fit_gaussian$estimate[1], sd = fit_gaussian$estimate[2])
  #cdf_df_gaussian <- data.frame(Julian = sort((as.numeric(circ_data) / (2 * pi)) * 365), CDF_Gaussian = cdf_values_gaussian)
  cdf_df_gaussian <- data.frame(Julian = sort(julian_days), CDF_Gaussian = cdf_values_gaussian)
  
  
  # Extract quantile values for Gaussian in Julian days
  quantiles_gaussian <- qnorm(c(0.33, 0.66), mean = fit_gaussian$estimate[1], sd = fit_gaussian$estimate[2])
  quantile_df_gaussian <- data.frame(Quantile = c(0.33, 0.66), Julian = quantiles_gaussian)
  
  ecdf_values <-ecdf(julian_days)(sort(julian_days))
  ecdf_df <- data.frame(Julian = sort(julian_days), ECDF = ecdf_values)
  
  
  p<-ggplot() +
  geom_line(data = cdf_df_gaussian, aes(x = Julian, y = CDF_Gaussian), color = "blue", size = 1.5, linetype = "solid") +
    geom_line(data = cdf_df, aes(x = Julian, y = CDF_VonMises), color = "grey", size = 1.5, linetype = "solid") +
    geom_line(data = ecdf_df, aes(x = Julian, y = ECDF), color = "red", size = 1) +
    geom_point(data = data_points_df, aes(x = Julian, y = 0), color = "black", size = 2) +
    geom_point(data = quantile_df_vonmises, aes(x = Julian, y = Quantile), color = "red", size = 3, shape = 17) +
    # geom_point(data = quantile_df_gaussian, aes(x = Julian, y = Quantile), color = "blue", size = 3, shape = 17) +
    #geom_text(data = quantile_df_vonmises, aes(x = Julian, y = Quantile, label = round(Julian, 2)),
    #          vjust = -0.5, hjust = 0) +
    geom_text(data = quantile_df_gaussian, aes(x = Julian, y = Quantile, label = round(Julian, 2)),
              vjust = -0.5, hjust = 0) +
    geom_hline(yintercept = c(0.33, 0.66), linetype = "dashed", color = "red") +
    theme_minimal() +
    #theme(legend.position = 'bottom', legend.text = element_text(size=12))+
    theme(axis.text.x = element_text(size= 12, hjust = 1), axis.title = element_blank())+
    theme(axis.text.y = element_text(size= 12, hjust = 1), axis.title = element_blank())+
    theme(plot.title = element_text(hjust = 0.5, size = 14))+
    ggtitle(paste0(estacion,'. ', crop,'. ', fase))
  if(fase == 'Cessation'){fuse = 'cessation'};if(fase == 'Onset'){fuse = 'Onset'}
  ggsave(paste0(outfolder,'/',serie,'_',crop,'_',fase,'.jpg'),p,width = 4, height = 5)  
  
  
  # Goodness-of-fit plot using ggplot
 
  
  Empirical<-as_tibble(t(round(quantile(julian_days,c(0.05,0.33,0.66,0.95)))))
  names(Empirical)<-c('q0.05','q0.33','q0.66','q0.95')
  Normal<-as_tibble(round(quantile(fit_gaussian,c(0.05,0.33,0.66,0.95))$quantiles)) 
  names(Normal)<-c('q0.05','q0.33','q0.66','q0.95')
  Vonmises <- qvonmises(c(0.05, 0.33, 0.66, 0.95), mu = fit_vonmises$mu, kappa = fit_vonmises$kappa)
  Vonmises<-as_tibble(t(round(as.numeric(Vonmises / (2 * pi)* 366)))) 
    
  
  names(Vonmises)<-c('q0.05','q0.33','q0.66','q0.95')
  tablon<-bind_rows(
    mutate(Empirical, Distribution = paste("Empirical",fase)),
    mutate(Normal, Distribution = paste("Normal",fase)),
    mutate(Vonmises, Distribution = paste("Von Mises",fase))) %>% 
    dplyr::select(Distribution, 1:length(.)-1) 
  
  tablon<-tablon %>% mutate_at(vars(-1), ~ ifelse(. > 366, . - 366, .)) %>% mutate(across(-1, as.integer))

 
  write.table(tablon,paste0(outfolder,'/',serie,'_',crop,'_',fuse,'.txt'))
  kaka<-kbl(tablon) %>% kable_minimal()
  #print(kaka)
  return(list(tablon=tablon,grafico=p,tablapija=kaka))
}





calendator<-function(
    
  
  #mestart = 1, # mes que inicia el año
  seche=1,  #definicion día seco
  xmm =20,  # cantidad en mm para determinar onset
  
  monset = 4, # mes buscar onset
  donset = 5, # day busar onset
  yjours =  3,  # días para acumular xmm
  mjours = 10 , # máximo número de días secos post-onset
  pendant = 30 , # total de días en los que mirar
  
  moces =10,
  doces = 31,
  ymm = 10,  # candidad máxima acumulada para fin
  endjours = 7,  # número de días cantidad fin
  
  estacion = 'LOME',  #nombre estación  cosmético para outpout
  crop = 'RICE',  # nombre cultivo  cosmético  para output
  ciclo=90,  # duración del ciclo de cultivo
  serie='653410',  # serie  nombre exacti
  infolder='../assessed',  # directorio en el que buscarla
  outfolder='../cropcal', # directorio en el que almacenaer resultados
  calendaryear = 2024, #año para el que saldrá el calendario
  distro = 3 # 3 for von mises, 2 for Gaussian, 1 for empirical
){
  
  
  if (!dir.exists(outfolder)) {dir.create(outfolder) }
  
  station<-paste0(infolder,'/',serie,'.txt')
  
 
  
  k<-read_table(station,col_names = FALSE, na ='-99.9') %>% 
    rename('Year'=1,'Month'=2,'Day'=3,'RR'=4,'TX'=5,'TN'=6) %>% dplyr::select(-TX,-TN) %>%
    mutate(RRS = zoo::rollsum(RR, mjours, align = "right", fill = 0)) %>% mutate(Seche = ifelse(RR <= seche, 1,0)) %>% 
    mutate(row_id = row_number()) %>% mutate(Date = ymd(paste(Year,Month,Day,sep='-'))) %>% filter(!is.na(Date)) %>%
    mutate(Pendant = rollapply(Seche, width = pendant, FUN = max_racha_posterior, align = "left", partial = TRUE)) %>%
    mutate(Starts = ifelse(RRS > xmm & Pendant <= mjours & (Month > monset | Month == monset & Day >= donset) ,1,0)) %>%
    mutate(RRE = zoo::rollsum(RR, endjours,align = 'right',fill=0)) %>% 
    mutate(Ends = ifelse(RRE <= ymm & (Month > moces | Month == moces & Day >= doces),1,0))
  
  onsets<-k%>% filter(Starts == 1) %>% group_by(Year) %>% summarise(Onset = first(Date)) %>% mutate(JulianO = yday(Onset))
  cessations <- k%>% filter(Ends == 1) %>% group_by(Year) %>% summarise(Cessation = first(Date)) %>% mutate(JulianC = yday(Cessation))
  
  
  season<-full_join(onsets,cessations,by='Year') %>% arrange(Year)
  
  
  #julian_days,nametag,estacion, crop, serie,outfolder, fase
    ajuste<-tablones(season$JulianO,estacion = estacion, crop = crop, serie = serie, outfolder = outfolder, fase = 'Onset')
    inicio<-ajuste$tablon
    
    fitonset<-ajuste$tablon  %>% slice(distro) %>% 
    dplyr::select(-Distribution) %>% unlist(use.names = FALSE)
  
 # ggsave(paste0(outfolder,'/',serie,'_onset.jpg'),ajuste$grafico,width = 3, height = 4)  
    
 
    ajuste<-tablones(season$JulianC,estacion = estacion, crop = crop, serie = serie, outfolder = outfolder, fase = 'Cessation')
    cese<-ajuste$tablon
   
  fitcesation<-ajuste$tablon %>% slice(distro) %>% 
    dplyr::select(-Distribution) %>% unlist(use.names = FALSE)
  
#  ggsave(paste0(outfolder,'/',serie,'_cessation.jpg'),ajuste$grafico)
  calendaryear = 2024
  fonset<-paste(calendaryear,monset,donset,sep='-')
  fcesat<-paste(calendaryear,moces,doces,sep='-')
  
 # the interanual issue (>366) is corrected at tablon now, so the ifelse is redundant, but does not hurt, just in case! 
fitonset<-fitonset %>% as_tibble() %>% mutate(value = ifelse(value > 366, value-366,value)) %>% pull(value)
fitcesation<-fitcesation %>% as_tibble() %>% mutate(value = ifelse(value > 366, value-366,value)) %>% pull(value)

  imp.dates<- rep(NA, 366)
  if(fitonset[1] <= fitonset[2]){
  imp.dates[fitonset[1]:fitonset[2]]<-'Sowing Early'}
  else{
    imp.dates[fitonset[1]:366]<-'Sowing Early'
    imp.dates[1:fitonset[2]]<-'Sowing Early'} 


  if(fitonset[2] <= fitonset[3]){
  imp.dates[fitonset[2]:fitonset[3]]<-'Sowing Normal'}
  else{
  imp.dates[fitonset[2]]:366 <- 'Sowing Normal'
  imp.dates[1:fitonset[3]] <- 'Sowing Normal'
    }
  
  if(fitonset[3] <= fitonset[4]){
  imp.dates[fitonset[3]:fitonset[4]]<-'Sowing Late'}
  else {
    imp.dates[fitonset[3]:366] <-'Sowing Late'
    imp.dates[1:fitonset[4]] <-'Sowing Late'
  }
  

 
  harvest1<-fitonset[1]+ciclo; if(harvest1 > 366){harvest1<-harvest1-366}
  harvest2<-fitonset[2]+ciclo; if(harvest2> 366){harvest2<-harvest2-366}
  harvest3<-fitonset[3]+ciclo; if(harvest3 > 366){harvest3<-harvest3-366}
  harvest4<-fitonset[4]+ciclo; if(harvest4 > 366){harvest4<-harvest4-366}
  
  if(harvest1 < harvest2){
    imp.dates[harvest1:harvest2]<-'Harvest Early'
  } else{
   imp.dates[harvest1:366]<-'Harvest Early'
   imp.dates[1:harvest2]<-'Harvest Early'
  }
  
  
  if(harvest2 < harvest3){
    imp.dates[harvest2:harvest3]<-'Harvest Normal'
  } else{
    imp.dates[harvest2:366]<-'Harvest Normal'
    imp.dates[1:harvest3]<-'Harvest Normal'
  }
  
  
  if(harvest3 < harvest4){
    imp.dates[harvest3:harvest4]<-'Harvest Late'
  } else{
    imp.dates[harvest3:366]<-'Harvest Late'
    imp.dates[1:harvest4]<-'Harvest Late'
  }
  
  
  fonsi<-yday(format(as.Date(paste0(fonset, "%y-%m-%d"))))
  imp.dates[fonsi]<-'Onset Starter'
  consi<-yday(format(as.Date(paste0(fcesat, "%y-%m-%d"))))
  imp.dates[consi]<-'Cessation Starter'
  imp.dates[max(fonsi+1,fitonset[1])]<-'First Onset'
  imp.dates[fitonset[2]]<-'Normal Onset'
  imp.dates[fitonset[3]]<-'Late Onset'
  imp.dates[fitonset[4]]<-'Last Onset'
  imp.dates[fitcesation[1]]<-'First Cessation'
  imp.dates[fitcesation[2]]<-'Normal Cessation'
  imp.dates[fitcesation[3]]<-'Late Cessation'
  imp.dates[fitcesation[4]]<-'Last Cessation'
  

  print(imp.dates)
  
  desired_order<-c('Onset Starter','Sowing Early','Sowing Normal','Sowing Late',
                   'First Onset','Normal Onset', 'Late Onset', 'Last Onset',
                   'Harvest Early',  'Harvest Normal', 'Harvest Late',
                   'Cessation Starter','First Cessation','Normal Cessation', 'Late Cessation','Last Cessation')
   
 
 
  
   ordered_colors<-c('red','cyan','azure3','cadetblue2',
                     'darkolivegreen1','darkolivegreen2','darkolivegreen3','green',
                    'cornsilk1','cornsilk2','cornsilk4',
                    'magenta','darkorange1','gold', 'plum', 'plum4')
  
  

  kommando<-paste0(outfolder,'/',serie,'_',crop,'.jpg')
  start_date <- as.Date("2024-01-01")
  end_date <- as.Date("2024-12-31")
  date_seq <- seq(start_date, end_date, by = "1 day")
  ### cagontoloquesemenea
  # browser()
  # data <- tibble(date = date_seq) %>% mutate(month = month(date, label = TRUE, abbr = TRUE),day = day(date), month = factor(month, levels = rev(month.abb))) %>%
  #   mutate(Event = imp.dates)
  #browser()
  data <- tibble(date = date_seq) %>% mutate(month = month(date, label = TRUE, abbr = TRUE),day = day(date))  %>%
        mutate(Event = imp.dates) %>% mutate(month = as.factor(month))
  keywest <- tibble(Event=desired_order,color=ordered_colors)
  data<-left_join(data,keywest,by='Event') %>% arrange(date) %>% mutate(color=ifelse(is.na(color),'white',color)) 
  
  
print(data,n=366)
print(keywest,n=366)  
  
  
  titola = paste0(estacion, '. ', crop, '.')
  # browser()
  p<-ggplot(data, aes(x = factor(day), y = month, fill = color)) +
    geom_tile(color = "black",width = 0.8, height = 0.8, linewidth = 0.8) +
    scale_fill_identity("",guide = 'legend',labels = keywest$Event, breaks = keywest$color) +
    theme_minimal() +
    theme(legend.position = 'bottom', legend.text = element_text(size=12))+
    theme(axis.text.x = element_text(size= 12, hjust = 1), axis.title = element_blank())+
    theme(axis.text.y = element_text(size= 14, hjust = 1), axis.title = element_blank())+
    theme(plot.title = element_text(hjust = 0.5, size = 18))+
    scale_y_discrete(limits = rev(levels(data$month)))+
    ggtitle(titola)
    
  ggsave(kommando, p,width = 8.5, height = 5)


    
  
  return(list(inicio=inicio,cese=cese))
  
  
}












ui <- fluidPage(
  titlePanel("Crop Calendar Generator"),
  fluidRow(
    column(width = 2,
           textInput("serie", "Code:", value = "653550", width = "60%"),
           textInput("estacion", "Station Name", value = 'LOME', width = "60%"),
           textInput("crop", "Crop:", value = "CORN", width = "60%"),             
           textInput("infolder", "Input Folder:", value = "../assessed", width = "60%"),
           textInput("outfolder", "Output Folder:", value = "../cropcal", width = "60%"),
           numericInput("ciclo", "Crop Cycle Duration:", value = 120, width = "60%"),
#           numericInput("mestart", "Start Month:", value = 1, width = "60%"),
           numericInput("monset", "Onset Month:", value = 2, width = "60%"),
           numericInput("donset", "Onset Day:", value = 1, width = "60%"),
           
           selectInput("distro", "Distribution Type:",
                       choices = c("Empirical" = 1, "Gaussian" = 2, "Von Mises" = 3),
                       selected = 3, width = "60%"),
           actionButton("generate", "Run", width = "80%"),
           br(), 
           actionButton("cerrar", "Exit App.", width = "80%"),
           # numericInput("calendaryear", "Calendar Year:", value = 2024, width = "60%"),
    ),
    column(width = 2,
           numericInput("seche", "Dry Day Threshold:", value = 1, width = "60%"),
           numericInput("xmm", "Rainfall Threshold:", value = 20, width = "60%"),
           numericInput("yjours", "Days to Accumulate Rainfall:", value = 3, width = "60%"),
           numericInput("mjours", "Max CDD Post-Onset:", value = 10, width = "60%"),
           numericInput("pendant", "In X days", value = 30, width = "60%"),
           numericInput("moces", "Cessation Month:", value = 7, width = "60%"),
           numericInput("doces", "Cessation Day:", value = 1, width = "60%"),
           numericInput("ymm", "Max Accumulated Rainfall for End:", value = 15, width = "60%"),
           numericInput("endjours", "Number of Days for End:", value = 17, width = "60%"),
          
           
           
    ),
    column(width = 3,
           imageOutput("image2"),
           imageOutput("image3")
    ),
    column(width = 3,
           imageOutput("image"),
           div(style = "height:100px;"), 
           rHandsontableOutput("table"),
           div(style = "height:100px;"),
           rHandsontableOutput("table2"),
           
    )
  ),
  plotOutput("calendarPlot"),
  tableOutput("calendarTable")
)
server <- function(input, output, session) {
  
  
  observeEvent(input$cerrar, {
    stopApp("App Closed.")
  })
  
  
  
  observeEvent(input$generate, {
    outputText <- isolate({
     kaka<- calendator(
#        mestart = input$mestart,
        seche = input$seche,
        xmm = input$xmm,
        monset = input$monset,
        donset = input$donset,
        yjours = input$yjours,
        mjours = input$mjours,
        pendant = input$pendant,
        moces = input$moces,
        doces = input$doces,
        ymm = input$ymm,
        endjours = input$endjours,
        ciclo = input$ciclo,
        serie = input$serie,
        infolder = input$infolder,
        outfolder = input$outfolder,
        calendaryear = input$calendaryear,
        distro = as.numeric(input$distro),
        crop = input$crop,
        estacion = input$estacion
      )
      
      img_path <- paste0(input$outfolder,'/',input$serie,'_',input$crop,'.jpg')
      print(img_path)
      output$image <- renderImage({
        list(src = img_path, alt = "Calendar", width = 600)
      }, deleteFile = FALSE)
      
      
      img_path2 <- paste0(input$outfolder,'/',input$serie,'_',input$crop,'_','onset.jpg')
      print(img_path)
      output$image2 <- renderImage({
        list(src = img_path2, alt = "Onset", width = 375,height = "97%")
      }, deleteFile = FALSE)
      
      img_path3 <- paste0(input$outfolder,'/',input$serie,'_',input$crop,'_','cessation.jpg')
      print(img_path)
      output$image3 <- renderImage({
        list(src = img_path3, alt = "Cessation", width = 375, heigth = "97%")
      }, deleteFile = FALSE)
      
      output$table <- renderRHandsontable({
        data <- kaka$inicio
        nombresitos<-colnames(data)
        rhandsontable <- rhandsontable(data, rowHeaders=F, colHeaders=nombresitos) %>%
          return(rhandsontable)
      })
      
      output$table2 <- renderRHandsontable({
        data <- kaka$cese
        nombresitos<-colnames(data)
        rhandsontable <- rhandsontable(data, rowHeaders=F, colHeaders=nombresitos) %>%
        return(rhandsontable)
      })
      
    })
  })
  
  # 
  # output$calendarTable <- renderTable({
  #   # Assuming that the table data is saved in a file (modify accordingly)
  #   tableFile <- paste(input$outfolder, "/calendar_table.txt", sep = "")
  #   if (file.exists(tableFile)) {
  #     # Display the saved table
  #     read.table(tableFile, header = TRUE)
  #   }
  # })
}

shinyApp(ui, server)
