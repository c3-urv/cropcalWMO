library(shiny)
library(shinyjs)
library(lubridate)
library(INQC)
library(DT)
library(crosstalk)
library(gridExtra)
library(ggthemes)
library(ggpubr)
library(shinyWidgets)
library(rhandsontable)
library(tidyverse)



drywetseq <-function (x, chseco=200, chhumedo = 200, sueco = 0, dry = TRUE, wet = TRUE){
  todo <- NULL
  seco <- which(x <= sueco)
  mojao <- which(x > sueco)
  nohay <- which(is.na(x))
  y <- x
  y[seco] <- 0
  y[nohay] <- 9
  y[mojao] <- 1
  nyu <- rle(y)
  wetspell <- nyu$lengths[which(nyu$values == 1)]
  
  if (length(wetspell) > 0) {
    wetlim = chhumedo
    wetchungo <- which(nyu$lengths > wetlim & nyu$values == 
                         1)
  }
  else {
    wetchungo = NULL
  }
  
  dryspell <- nyu$lengths[which(nyu$values == 0)]
  if (length(wetspell) > 0) {
    drylim = chseco  
    drychungo <- which(nyu$lengths > drylim & nyu$values == 
                         0)
  }
  else {
    drychungo = NULL
  }
  if (dry & wet) {
    chungos <- c(wetchungo, drychungo)
  }
  if (dry & !wet) {
    chungos <- drychungo
  }
  if (!dry & wet) {
    chungos <- wetchungo
  }
  if (length(chungos) != 0) {
    ene <- length(chungos)
    for (i in 1:ene) {
      rocha <- nyu$lengths[chungos[i]]
      end <- sum(nyu$lengths[1:chungos[i]])
      start <- end - rocha + 1
      if (i == 1) {
        todo <- c(start:end)
      }
      else {
        todo <- c(todo, start:end)
      }
    }
  }
  return(todo)
}



updater <- function(QC, tester, valor, direct = TRUE) {
  if (length(tester) != 0) {
    for (i in 1:length(names(tester))) {
      target <- which(names(QC) == names(tester)[i])
      for (j in 1:length(tester[[i]])) {
        old_value <- as.numeric(QC[tester[[i]][j], target])
        if (old_value == 9) { QC[tester[[i]][j], target] <- old_value }
        if (old_value == 0) { QC[tester[[i]][j], target] <- valor }
        if (direct & old_value != 0 & old_value != 9) { QC[tester[[i]][j], target] <- valor }
        if (!direct & old_value != 0 & old_value != 9) { QC[tester[[i]][j], target] <- min(valor, old_value) }
      }
    }
  }
  return(QC)
}

extract_station_data <- function(prr,ptx,ptn, output_dir = "../qcdata", input_dir ='../data') {
  # Crear el directorio de salida si no existe
  if(dir.exists((output_dir))){ unlink(output_dir, recursive = TRUE)} #removes previous existing versions
  if (!dir.exists(output_dir)) { #creates it
    dir.create(output_dir)
    
    # Borrar todos los ficheros del directorio
    
  }

  nyustations <- unique(prr$Station)
  
  # Iterar sobre cada estación
  for (station in nyustations) {
    # Filtrar los datos para la estación actual
    
   
    RR <- prr %>% filter(Station == station) %>% select(Year,Month,Day,rr) 
    RX <- ptx %>% filter(Station == paste0('TX', substring(station,3,nchar(station)))) %>% mutate(tx = rr) %>% select(Year,Month,Day,tx) 
    RN <- ptn %>% filter(Station == paste0('TN', substring(station,3,nchar(station)))) %>% mutate(tn = rr) %>% select(Year,Month,Day,tn) 
    station_data <- full_join(RR,RX, by = c('Year','Month','Day')) %>% full_join(RN, by = c('Year','Month','Day')) %>% arrange(Year, Month, Day)
    station<-substring(station,3,nchar(station))
   # Guardar los datos en un archivo
    output_file <- paste0(output_dir, "/", station, ".txt")
    write.table(station_data, output_file, sep = "\t", row.names = FALSE,col.names  = FALSE, na ='-99.9')
  }
  

  
  rutaestaciones<-paste0(input_dir,'/stations.txt')
  kaka<-read.table(rutaestaciones,header=TRUE)
  kakaname<-paste0(output_dir,'/stations.txt')
  write.table(kaka,kakaname,row.names = FALSE, col.names = TRUE,quote=FALSE,sep='\t')
}




# INQC DEFAULTS
rr <- NULL
QClist <- NULL
nyu_default <- 60  # Default value for max tx allowed
seco_default <- -10 # Default value for smallest max temp
humedo_default<-200 # Default value for max number of consecutive wet days
plano_default <- 4  # Default flat
planofloat_default <- 10 # Default flatfloat
rutaestaciones<-NULL 
sustu_default<-15 # Default for jumpsABS
sustitu_default<-.999 # Default jumpQUANT
jedi_default <- 2 # Times * jumpsQUANT
zero_default<-5 # Default IQR, number of IQRs
indiscreta_default<-30 # Default IQR, window size (thanks Alfred and James)
repetido_default<-10 # 
repeyear_default<-60
repemonth_default<-15
margen_default <-0.999 ### quantile of the differences for newfriki
veces_default <- 3 ### times to multiply margen to detect fewer cases





# Define la interfaz de usuario (UI)
# Añade estas líneas a tu código ui
# Modifica tu código ui
# UI
ui <- fluidPage(
  titlePanel("QC Tmax Data with INQC"),
  fluidRow(
    column(3, 
           textInput("directory", "Path to data folder (either relative or full)",value = '../qcdataRR'),
           actionButton("showFiles", "Load Data"),
           actionButton("runINQC", "Run INQC for Tmax"),
           actionButton("extractStationData", "Extract Station Data"),
           textInput("outputDirectory", "Output Directory:", value = "../qcdataTX"),
           actionButton("cerrar", "Exit App.", icon = icon("times")),

    ),
    
    column(1,
           numericInput("nyuInput", "Max Val:", value = nyu_default),
           numericInput("planoInput","Flat Sequence", value = plano_default),
           numericInput("planofloatInput","Flat Seq.Decimal", value = planofloat_default),
 
    ),
    column(1,
           numericInput("secoInput","Min Val", value = seco_default),
           numericInput("humedoInput","Max CWDs", value = humedo_default),
           numericInput("repetidoInput","Max Dec. Month ", value = repetido_default),
 
           
    ),
    
    column(1,
           numericInput("sustuInput","ABS jump.", value = sustu_default),
           numericInput("sustituInput","QUANT jump", value = sustitu_default),
           numericInput("jediInput","Quant Mulitplier", value = jedi_default),
    ),
    column(1,
           numericInput("zeroInput","IQRs outliers", value = zero_default),
           numericInput("indiscretaInput","Window outliers", value = indiscreta_default),
           numericInput("repemonthInput","Repeated Month", value = repemonth_default),
           numericInput("repeyearInput","Repeated Year", value = repeyear_default),
           
           
    ),
    
    
    column(2,
           actionButton("deleter", "Delete  Failing Test"),
           actionButton("misser", "Set NA  Failing Test"),
           actionButton("dismisser", "Dismiss Failing Test"),
           textInput("whichtest", "Test:", value = "dates"),
     ),
    column(2,
           actionButton("misser2", "Set NA at QClevel"),
           actionButton("dismisser2", "Dismiss  QClevel"),
           textInput("whichcode", "Level:", value = 1),
           )
    
     
    
      ),
  
  tags$hr(),
  
  fluidRow(
    column(6, 
           verbatimTextOutput("dataLoadedMessage"),
           tableOutput("summaryTable")
    ),
    
    column(6, 
           DTOutput("qcResultTable")
    )
  )


  
    
)
  


# Define el servidor
server <- function(input, output, session) {
  
  rrv <- reactiveValues(rrpivot = NULL, QCrr = NULL)
  reactiveValuesData <- reactiveValues(modalData = data.frame())

 
  observeEvent(input$cerrar, {
    stopApp("App Closed.")
  })
  
  
  
  observeEvent(input$showFiles, {
    req(input$directory)
    
    # Construye la ruta completa al directorio
    data_dir <- normalizePath(input$directory, winslash = "/", mustWork = FALSE)
    
    # Verifica si la ruta del directorio es válida
    if (dir.exists(data_dir)) {
      txt_files <- list.files(data_dir, pattern = "\\.txt$", full.names = TRUE)
      
      # Excluye el archivo "stations.txt" de la lista
      txt_files <- setdiff(txt_files, file.path(data_dir, "stations.txt"))
      
      # Lee los archivos y aplica las transformaciones
      data_list <- txt_files %>%
        set_names() %>%
        map_df(~ read.delim(.x, header = FALSE, col.names = c("Year", "Month", "Day", "RR", "TX", "TN"),
                            na.strings = c("-99.9", "-999.9")) %>%
                 mutate(ID = str_extract(basename(.x), "\\d+")))
      
      # Pivot para TX
      rr <<- data_list %>% dplyr::select(-RR, -TN) %>%
        pivot_wider(names_from = ID, values_from = TX, names_prefix = 'TX') %>%
        mutate(Date = make_date(Year, Month, Day)) %>%
        select(Date, everything()) %>%
        mutate(Date = str_replace_all(Date, "-", "")) %>%
        arrange(Year, Month, Day)
      
      # Pivot para TN
      
      rrTN <<- data_list %>% dplyr::select(-RR, -TX) %>%
        pivot_wider(names_from = ID, values_from = TN, names_prefix = 'TN') %>%
        mutate(Date = make_date(Year, Month, Day)) %>%
        select(Date, everything()) %>%
        mutate(Date = str_replace_all(Date, "-", "")) %>%
        arrange(Year, Month, Day)
      
      rrRR <<- data_list %>% dplyr::select(-TN, -TX) %>%
        pivot_wider(names_from = ID, values_from = RR, names_prefix = 'RR') %>%
        mutate(Date = make_date(Year, Month, Day)) %>%
        select(Date, everything()) %>%
        mutate(Date = str_replace_all(Date, "-", "")) %>%
        arrange(Year, Month, Day)
      

      
      # Calcular el porcentaje de disponibilidad de cada serie RR
      available <- rr %>%
        dplyr::select(-Date, -Year, -Month, -Day) %>%
        summarise_all(~ sum(!is.na(.)) / n() * 100) %>%
        gather(key = "Serie", value = "Percentage") %>%
        mutate(Serie = gsub("^TX", "", Serie)) %>% 
        rename(ID = Serie, PERCENTAGE = Percentage) %>% 
        mutate(ID = as.double(ID)) # Remove "RR" prefix
      
      # Leer el archivo stations.txt 
     stations_path <- file.path(data_dir, "stations.txt")
    rutaestaciones<<-stations_path #variable global! Quizás redundante ...       

      
      
      if (file.exists(stations_path)) {
        stations <- read_table(stations_path) %>%
          dplyr::select(-COUNTRY) %>%
          full_join(available) %>%
          mutate(ID = as.character(ID), PERCENTAGE = as.double(PERCENTAGE))
      } else {
        stations <- data.frame(ID = character(), PERCENTAGE = double())
      }
      
      # Mostrar el mensaje y la tabla de resumen
      output$dataLoadedMessage <- renderPrint({
        cat("Data loaded, summary:\n")
      })
      # Actualizar los valores reactivos

      output$summaryTable <- renderTable({
        stations
      })
    } else {
      # Si la ruta del directorio no es válida, muestra un mensaje de error
      showModal(
        modalDialog(
          title = "Error",
          "La ruta del directorio no es válida. Por favor, ingrese una ruta válida.",
          easyClose = TRUE
        )
      )
    }
  })
  
  observeEvent(input$runINQC, {
    req(rr)
    
 
    output$qcResultTable <- renderTable(NULL)
    
    
    output$dataLoadedMessage <- renderPrint({
      cat("Running INQC:\n")
    })
    
    
    
    # PARAMETROS ENTRADA
    nyu <- input$nyuInput
    seco <-input$secoInput
    plano <-input$planoInput
    planofloat<-input$planofloatInput
    humedopar<-input$humedoInput
    sustu<-input$sustuInput
    sustitu<-input$sustituInput
    jedi<-input$jediInput
    zero<-input$zeroInput
    indiscreta<-input$indiscretaInput
    repetido<-input$repetidoInput
    repemonth<-input$repemonthInput
    repeyear<-input$repeyearInput
    veces<-input$vecesInput
    margen<-input$margenInput
    
 
    # WEIRDDATES 
    dates <- lapply(rr[, 5:ncol(rr)], function(x) { 
      x_numeric <- as.numeric(x)
      weirddate(data.frame(Date = paste0(rr$Year,
                                         sprintf("%02d", rr$Month),
                                         sprintf("%02d", rr$Day)), Value = x_numeric))
    }) %>% keep(~length(.x) > 0)
    QClist<-rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,dates,1,TRUE) %>%
      filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'dates')
    
    
    # DUPLICATED DAYS
    duplas <- INQC::duplas(paste0(rr$Year,sprintf("%02d", rr$Month),sprintf("%02d", rr$Day)))
    QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,duplas,1,FALSE) %>%
                        filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'duplas'))
    
 
    ## LARGE
    large <- rr[, 5:ncol(rr)] %>% lapply(function(x) INQC::physics(x, nyu = nyu, compare = 1)) %>% keep(~length(.x) > 0)
    QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,large,1,TRUE) %>%
                        filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'large'))
    
    ## SMALL
    small <- rr[, 5:ncol(rr)] %>% lapply(function(x) INQC::physics(x, nyu = seco, compare = 3)) %>% keep(~length(.x) > 0)
    QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,small,1,TRUE) %>%
                        filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'small'))
    
     # ## FLAT
    #
     flat<-rr[, 5:ncol(rr)] %>% lapply(function(x) INQC::flat(x, maxseq=plano-1,exclude = 0)) %>% keep(~length(.x) > 0) %>%
       lapply(function(x) c(x[which(diff(x)!=1)], tail(x, 1)))
     QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,flat,4,TRUE) %>%
                       filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'flat'))
    #
    # ## FLATFLOAT
    #
     flatfloat<-rr[, 5:ncol(rr)] %>% lapply(function(x) INQC::flat(x-floor(x), maxseq=planofloat-1,exclude = 0)) %>% keep(~length(.x) > 0) %>%
       lapply(function(x) c(x[which(diff(x)!=1)], tail(x, 1)))
     QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,flatfloat,4,TRUE) %>%
                         filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'flatfloat'))
   # ## NEWFRIKI

    newfriki<-rr[, 5:ncol(rr)] %>% lapply(function(x) INQC::newfriki(date=rr$Date,x,margina=margen,times=veces)) %>% keep(~length(.x) > 0)
    QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,newfriki,2,TRUE) %>%
                        filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'newfriki'))
    #
    # ## IQROUT
    #
     iqrout<-rr[, 5:ncol(rr)] %>% lapply(function(x) INQC::IQRoutliers(date=rr$Date,x,level=zero,window=indiscreta,exclude=0)) %>% keep(~length(.x) > 0)
     QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,iqrout,3,TRUE) %>%
                         filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'iqrout'))
    # # 
    # # ## JUMPS
    # # 
     jumpsABS <- lapply(rr[, 5:ncol(rr)], function(x) {
       x_numeric <- as.numeric(x)
       jumps2(date = rr$Date, value = x_numeric, force=sustu)
     }) %>% keep(~length(.x) > 0)  
     QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,jumpsABS,2,TRUE) %>%
                         filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'jumpsABS'))
 
     jumpsQUANT <- lapply(rr[, 5:ncol(rr)], function(x) {
       x_numeric <- as.numeric(x)
       jumps2(date = rr$Date, value = x_numeric, quanty=sustitu,times = jedi)
     }) %>% keep(~length(.x) > 0)  
     QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,jumpsQUANT,2,TRUE) %>%
                         filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'jumpsQUANT'))
     
     
     
     
    # ## ROUNDING
    roundprec <- lapply(rr[, 5:ncol(rr)], function(x) { 
      x_numeric <- as.numeric(x) %>% keep(~length(.x) > 0)
      rounding(data.frame(Date = rr$Date, Value = x_numeric), blocksize=repetido)
    }) %>% keep(~length(.x) > 0)
    QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,roundprec,4,TRUE) %>%
                        filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'rounding'))
    filtro <- QClist %>% filter(TEST == "rounding") %>% group_by(Year, Month) %>% arrange(Day) %>% filter(Day == max(Day)) %>% arrange(Year, Month)
    QClist<-QClist %>% filter(TEST != 'rounding') %>% bind_rows(filtro)

     # ## TOOMONTH
    
    toomonth <- lapply(rr[, 5:ncol(rr)], function(x) { ### issue: considers .5 and 5 as a hit! Might be solvable round(x,1)
      x_numeric <- as.numeric(x)
      toomany(data.frame(Date = rr$Date, Value = x_numeric), blockmany = repemonth, exclude=0,scope=1)
    }) %>% keep(~length(.x) > 0)
    QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,toomonth,4,TRUE) %>%
                        filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'toomonth'))

    filtro <- QClist %>% filter(TEST == "toomonth") %>% group_by(Year, Month) %>% arrange(Day) %>% filter(Day == max(Day)) %>% arrange(Year, Month)
    QClist<-QClist %>% filter(TEST != 'toomonth') %>% bind_rows(filtro)

#
    # ## TOOYEAR
    #
    tooyear <- lapply(rr[, 5:ncol(rr)], function(x) { ### issue: considers .5 and 5 as a hit! Might be solvable round(x,1)
      x_numeric <- as.numeric(x)
      toomany(data.frame(Date = rr$Date, Value = x_numeric), blockmany = repeyear, exclude=0,scope=2)
    }) %>% keep(~length(.x) > 0)
    QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,tooyear,4,TRUE) %>%
                        filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'tooyear'))
      filtro <- QClist %>% filter(TEST == "tooyear") %>% group_by(Year) %>% arrange(Month,Day,Year) %>% filter(Month == max(Month)) %>% filter(Day == max(Day)) %>% arrange(Year)
    QClist<-QClist %>% filter(TEST != 'tooyear') %>% bind_rows(filtro)


    #TXTN
    
    txtn1<-lapply(5:ncol(rr), function(i) which(rr[[i]] <= rrTN[[i]]));names(txtn1)<-colnames(rr)[5:ncol(rr)]
    txtn1<-txtn1 %>% keep(~length(.x) > 0)
    QClist<-bind_rows(QClist,rr%>% mutate_at(vars(5:ncol(rr)),~ ifelse(is.na(.), 9, 0)) %>% updater(.,txtn1,1,TRUE) %>%
                        filter(if_any(5:last_col(), ~.x != 0 & .x != 9)) %>% mutate(TEST = 'txtn'))
    
    
    
    rrpivot_data <-rr %>% pivot_longer(starts_with('TX')) %>% rename(rr = value, Station = name) %>%
       arrange(Station,Year,Month,Day) %>% mutate(row_id = row_number(), Date = ymd(Date))
    
    rrpivotTN_data <-rrTN %>% pivot_longer(starts_with('TN')) %>% rename(rr = value, Station = name) %>%
      arrange(Station,Year,Month,Day) %>% mutate(row_id = row_number(), Date = ymd(Date))
    
    rrpivotRR_data<-rrRR %>% pivot_longer(starts_with('RR')) %>% rename(rr = value, Station = name) %>%
      arrange(Station,Year,Month,Day) %>% mutate(row_id = row_number(), Date = ymd(Date))
    
    
    
    QCrr_data<- QClist %>% pivot_longer(cols=starts_with('TX')) %>% filter(value !=9, value != 0) %>% rename(QC = value, Station = name) %>%
      mutate(Date = ymd(Date)) %>%
      left_join(rrpivot_data) 
    
    # Asignar QCrr_data al reactiveValues
    rrv$QCrr <- QCrr_data
    
    
    rrv$TXpivot <- rrpivot_data
    rrv$TNpivot <- rrpivotTN_data
    rrv$RRpivot <- rrpivotRR_data
    
    output$qcResultTable <- DT::renderDataTable({
      datatable(rrv$QCrr, 
                selection = 'single',
                filter = 'top', 
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',
                  buttons = list(
                    'copy', 'csv', 'excel', 'pdf', 'print', 'colvis', 
                    list(
                      extend = 'colvisGroup',
                      text = 'Show All',
                      show = ':hidden'
                    )
                  ),
                  scrollX = TRUE,
                  pageLength = 10,
                  lengthMenu = c(5, 10, 15, 20),
                  columnDefs = list(
                    list(targets = 0, visible = FALSE),
                    list(className = 'dt-center', targets = "_all"),
                    list(width = '50px', targets = "_all")
                  ),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'font-size': '12px'});",
                    "}"
                  )
                ))
    }, callback = JS(
      "table.on('preXhr.dt', function () {",
      "  Shiny.setInputValue('showAll', false);",
      "});",
      "Shiny.addCustomMessageHandler('showAll', function(data) {",
      "  table.search('').columns().search('').draw();",
      "});"
    ))
    
    
    
      })

  
  shared_df <- reactiveVal(data)
  
  # Filter data based on selected row
  selected_row_data <- reactive({
    req(input$qcResultTable_rows_selected)
    rows_selected <- input$qcResultTable_rows_selected
    shared_df()[rows_selected, ]
  })
  
   # Render the selected record plot
  output$selectedRecordPlot <- renderPlot({
    req(input$qcResultTable_rows_selected)
    
    # Get the selected row indices
    selected_rows <- input$qcResultTable_rows_selected
    
    # Use the values from the selected record for plotting
    if (length(selected_rows) > 0) {
      # Extract relevant information from the selected record
      selected_record <- rrv$QCrr[selected_rows, ]
      if(nrow(selected_record)>1){selected_record<-selected_record[nrow(selected_record),]}
      # Extracting station, month, day, and creating date range
      station <- selected_record$Station
      month <- selected_record$Month
      day <- selected_record$Day
      nyear<-selected_record$Year
      #browser()
      if(!is.na(selected_record$Date)){
      fecha1 <- selected_record$Date -365
      fecha2 <- selected_record$Date + 365
      # Filter and plot based on the selected record values
      selected_boxplot <- rrv$TXpivot %>%
        filter(Station == as.character(station), Month == as.numeric(month))
      
      selected_barplot <- rrv$TXpivot %>%
        filter(Station == as.character(station), Date >= fecha1 & Date <= fecha2) 
      
      
      }else{
        #browser()
        selected_boxplot <- rrv$TXpivot %>%
          filter(Station == as.character(station), Month == as.numeric(month))
        selected_barplot <- rrv$TXpivot %>%
          filter(Station == as.character(station), Year == as.numeric(nyear)) 
        
        
      }

      # Assuming first_days is a vector of first days of each month
      first_days <- selected_barplot$Date
      
      # Assuming you have loaded the necessary libraries and created the selected_barplot data frame
      
      # Create a time series barplot
      plot1 <- ggplot(selected_barplot, aes(x = Date, y = rr)) +
        geom_line(stat = "identity") +
        ggtitle(paste("Station:", station)) +
        xlab("Date") +
        ylab("Tempeature, ªC") +
        scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d") +  # Display first day of each month
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      

      # Boxplot
      plot2 <- ggplot(selected_boxplot, aes(x = 1, y = rr)) +
        geom_boxplot() +
        ggtitle(paste("Station:", station, "and Month:", month)) +
        #theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
        coord_flip()+
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())+
        ylab("Temperature, ªC")
       # Show both plots side by side using gridExtra
      #grid.arrange(plot1, NULL, plot2, ncol = 2, heights = c(2, 1))
      grid.arrange(plot1,                                    # bar plot spaning two columns
                   plot2,                                # box plot and scatter plot
                   ncol = 1, nrow = 2 #, 
                   #layout_matrix = rbind(c(1,2), c(1,3))
                   ) ### sets rows!!!
      

     }
  })
  

  #modalTableData <- reactiveVal(data.frame())
  modalTableData <- reactiveValues(data=data.frame())
  
  # Lógica para abrir la ventana modal
  observe({
    selected_rows <- input$qcResultTable_rows_selected
    showModal <- length(selected_rows) > 0
    
    if (showModal) {
      showModal(
        modalDialog(
          plotOutput("selectedRecordPlot"),
          br(),        
          rHandsontableOutput("selectedRecordTable"),
          br(),
          actionButton("saveButton", "Save Changes and Exit"),
          br(),
          actionButton("deleteButton", "Delete Target Record"),
          size = 'l'
          
          
          
          
        )
      )
    }
  })
  
  col_defs <- list(
    row_id = list(readOnly = TRUE)
  )
  
  
  #### renderizamos la tabla modal!!
  output$selectedRecordTable <- renderRHandsontable({
    # Obtener las filas seleccionadas
    
    selected_rows <- input$qcResultTable_rows_selected
    
    # Extraer información relevante del registro seleccionado
    selected_record <- rrv$QCrr[selected_rows, ]
    if(nrow(selected_record)>1){selected_record<-selected_record[nrow(selected_record),]}
    
    
    # Extraer la estación, el mes y el día
    station <- selected_record$Station
    month <- selected_record$Month
    day <- selected_record$Day
    nyear <-selected_record$Year
    ndate <-selected_record$Date
    tiesto<-selected_record$TEST
    idito<-selected_record$row_id
    rarito<-selected_record$rr
    print(tiesto)
    
    if(!is.na(ndate)){
    # Crear un rango de fechas
      if(tiesto == 'wetseq' | tiesto == 'dryseq' | tiesto == 'susacum' | tiesto == 'flat' | tiesto == 'flatfloat' 
         | tiesto == 'roundprec' | tiesto == 'tooyear' | tiesto == 'toomonth'){
       if(tiesto== 'dryseq'){firsty<- rrv$TXpivot %>% filter(row_id <= idito) %>% slice(which(rr!=0)) %>% last() %>% pull(row_id)
       lasty<-rrv$TXpivot %>% filter(row_id == idito) %>% pull(row_id)}
       if(tiesto== 'wetseq'){firsty<- rrv$TXpivot %>% filter(row_id <= idito) %>% slice(which(rr==0)) %>% last() %>% pull(row_id)
       lasty<-rrv$TXpivot %>% filter(row_id == idito) %>% pull(row_id)}
       if(tiesto == 'susacum' | tiesto == 'flat' | tiesto == 'flatfloat'){firsty = idito -14; lasty = idito+5}  
       if(tiesto == 'roundprec' | tiesto == 'toomonth'){
        
         mes<-rrv$TXpivot %>% filter(Year == nyear, Month == month,Station == station)  %>% pull(row_id)
         firsty=min(mes);lasty=max(mes)
         
       }  
        if(tiesto == 'tooyear'){
          anyo<-rrv$TXpivot %>% filter(Year == nyear,Station == station)  %>% pull(row_id)
          firsty=min(anyo);lasty=max(anyo)
         }
        
        
               
      }
      else
      {
        firsty<-idito-8
        lasty<-idito+8
      }
    
    # Filtrar los registros circundantes



      surrounding_records <- rrv$TXpivot %>% filter(Station == as.character(station), row_id >= firsty, row_id <= lasty) %>%
      left_join(rrv$TNpivot,by='row_id',suffix = c('','.y')) %>% rename(rrTN = rr.y) %>% 
        dplyr::select(-ends_with('.y')) %>%
      select(-Station,  -Date) %>% select(Year, Month, Day, rr, rrTN, row_id)
    
    # Actualizar los datos de la tabla modal
    modalTableData$data <- surrounding_records
    
    
    } else {
      surrounding_records <- rrv$TXpivot %>%
        filter(Station == as.character(station), Year == as.numeric(nyear), Month == as.numeric(month)) %>% 
        left_join(rrv$TNpivot,by='row_id',suffix = c('','.y')) %>% rename(rrTN = rr.y) %>% 
        dplyr::select(-ends_with('.y')) %>%
        select(-Date, -Station) %>% select(Year, Month, Day, rr, rrTN, row_id)
     
      modalTableData$data <- surrounding_records
    }
    # Renderizar la tabla
    rhandsontable(modalTableData$data,height = 300, selctable = TRUE)
  })
  
  
  
  
  selectedPosition <- reactiveValues(value = NULL)
  
  # Observar el evento del botón Guardar
  observeEvent(input$saveButton, {
    selectedData <- hot_to_r(input$selectedRecordTable)
    
    selected_rows <- input$qcResultTable_rows_selected
    targetid<-rrv$QCrr %>% slice(selected_rows) %>% pull(row_id)
    # Paso 1: Identificar qué filas de rrv$QCrr hay que actualizar
    target<-selectedData$row_id
    sustis<-inner_join(selectedData,rrv$QCrr,by='row_id',suffix =c('','.y')) %>% select(-ends_with('.y')) %>% 
      select(Date,Year,Month,Day,TEST,Station,QC,rr,row_id)
   
     rrv$QCrr<-rrv$QCrr%>% filter(!row_id %in% target) %>% bind_rows(sustis) %>% 
       filter(row_id != targetid) %>% arrange(TEST, Station,Year, Month, Day)
     # add this to remove the filtered id in the qc table: filter(row_id != targetid) %>%
     
    sustis<-inner_join(selectedData,rrv$TXpivot,by='row_id',suffix =c('','.y')) %>% select(-ends_with('.y')) %>% 
      select(Date,Year,Month,Day,Station,rr,row_id)
    rrv$TXpivot<-rrv$TXpivot %>% filter(!row_id %in% target) %>% bind_rows(sustis) %>% arrange(Station,Year, Month, Day)
    
  
    sustis<-inner_join(selectedData,rrv$TNpivot,by='row_id',suffix =c('','.y')) %>% select(-ends_with('.y')) %>% 
      select(Date,Year,Month,Day,Station,rrTN,row_id) %>% rename(rr = rrTN)
    rrv$TNpivot<-rrv$TNpivot %>% filter(!row_id %in% target) %>% bind_rows(sustis) %>% arrange(Station,Year, Month, Day)
    
      
    # Cerrar la ventana modal después de realizar la actualización
    removeModal()
  })
  
 
  # Observar el evento del botón "Eliminar"
  observeEvent(input$deleteButton, {
    # Obtener las filas seleccionadas
    
    selected_rows <- input$qcResultTable_rows_selected
    
    # Extraer información relevante del registro seleccionado
    chungo<- rrv$QCrr[selected_rows, ] %>% dplyr::select(row_id) %>% as.numeric()
    
    # Eliminar las filas de rrv$QCrr y rrv$TXpivot
    rrv$QCrr<-rrv$QCrr %>% filter(row_id != chungo)
    rrv$TXpivot <- rrv$TXpivot   %>% filter(row_id != chungo)
    rrv$TNpivot <- rrv$TNpivot %>% filter(row_id != chungo)
    rrv$RRpivot <- rrv$RRpivot %>% filter(row_id != chungo)
    # Cerrar la ventana modal después de realizar la eliminación
    removeModal()
  })
  
  
  
  
  #Remove all records failing a test
  observeEvent(input$deleter, {
    req(input$whichtest)
    
    testkaput <- input$whichtest
    
    target<-rrv$QCrr %>% filter(TEST == testkaput) %>% pull(row_id)
    rrv$QCrr <- rrv$QCrr %>% filter(!row_id %in% target)
    rrv$RRpivot <- rrv$RRpivot %>% filter(!row_id %in% target)
    rrv$TXpivot <- rrv$TXpivot %>% filter(!row_id %in% target)
    rrv$TNpivot <- rrv$TNpivot %>% filter(!row_id %in% target)
    
    
    
    
    output$dataLoadedMessage <- renderPrint({
      cat("Deleted Records failing test :\n",testkaput)
    })
    
  })
  
  
  
  observeEvent(input$misser2, {
    req(input$whichcode)
    # Set to missing QC level
    
    QCkaput <- input$whichcode
    target<- rrv$QCrr %>% filter(QC == QCkaput) %>% pull(row_id)
    rrv$QCrr <- rrv$QCrr %>% filter(!row_id %in% target)
    rrv$TXpivot <- rrv$TXpivot %>% mutate(rr = ifelse(row_id %in% target, NA, rr))
    
    
    
    output$dataLoadedMessage <- renderPrint({
      cat("Set to Missing QC level :\n",QCkaput)
    })
    
  })
  
  observeEvent(input$dismisser2, {
    
    # dismiss test level
    req(input$whichcode)
    QCkaput <- input$whichcode
    
    target<- rrv$QCrr %>% filter(QC == QCkaput) %>% pull(row_id)
    rrv$QCrr <- rrv$QCrr %>% filter(!row_id %in% target)
    
    
    output$dataLoadedMessage <- renderPrint({
      cat("Dismissed QC level :\n",QCkaput)
    })
    
  })
  
  
  
  observeEvent(input$misser, {
    
    ## set to missing one TEST
    req(input$whichtest)
    testkaput <- input$whichtest
    target<- rrv$QCrr %>% filter(TEST == testkaput) %>% pull(row_id)
    rrv$QCrr <- rrv$QCrr %>% filter(!row_id %in% target)
    rrv$TXpivot <- rrv$TXpivot %>% mutate(rr = ifelse(row_id %in% target, NA, rr))
    
    
    
    output$dataLoadedMessage <- renderPrint({
      cat("Set to NA Records failing test :\n",testkaput)
    })
    
  })
  
  
  
  observeEvent(input$dismisser, {
    
    # dimiss one test
    
    req(input$whichtest)
    testkaput <- input$whichtest
    target<- rrv$QCrr %>% filter(TEST == testkaput) %>% pull(row_id)
    rrv$QCrr <- rrv$QCrr %>% filter(!row_id %in% target)
    
    output$dataLoadedMessage <- renderPrint({
      cat("Dismissed Records failing test :\n",testkaput)
    })
    
  })
  
  
  
  
  

  # Handle the extract station data button
  observeEvent(input$extractStationData,{
     req(input$outputDirectory)
    req(input$directory)
    # Get the directory path
    directory <- input$outputDirectory
    inputdir <- input$directory
    # Extract the station data
    extract_station_data(rrv$RRpivot, rrv$TXpivot, rrv$TNpivot,directory, inputdir)
    output$dataLoadedMessage <- renderPrint({
      cat("QC'd data saved to:\n", directory)
    })
  })
 
  
}


# Crea la aplicación Shiny
shinyApp(ui, server)
