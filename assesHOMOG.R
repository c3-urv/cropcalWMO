library(shiny)
library(DT)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(cowplot)
library(rhandsontable)

ui <- fluidPage(
  titlePanel("Qualitative Assessment of Rainfall Homogeneity"),
  
  fluidRow(
    column(4,
           textInput("inputdir", "Input Directory:", value = "../qcdata"),
           textInput("outputdir", "Output Directory:", value = "../assessed"),
           numericInput("ene", "Enter the value of 'ene':", value = 4),
           selectInput("station", "Select Station:", choices = c(), multiple = FALSE),
           actionButton("runButton", "Run"),
           actionButton("loadDataButton", "Load Data"),
           actionButton("extractDataButton", "Extract Data"),
           actionButton("exitButton", "Exit App"),
           verbatimTextOutput("dataLoadedMsg"),
           rHandsontableOutput("stationTable"),
           tags$style("#corcoTable { margin-top: 20px; }"),
           rHandsontableOutput("corcoTable")
    ),
    column(8,
           fluidRow(
             plotOutput("plots", height = "800px")
           )
    )
  )
)

server <- function(input, output, session) {
  loaded_data <- reactiveValues(data = NULL)
  extractor<-reactiveValues(data = NULL)
  updatedTableData <- reactiveVal()
  
  observeEvent(input$extractDataButton,{
    inputdir <- input$inputdir
    outputdir <- input$outputdir
    
    if(dir.exists((outputdir))){ unlink(outputdir, recursive = TRUE)} #removes previous existing versions
    if (!dir.exists(outputdir)) { #creates it
      dir.create(outputdir)
      
      # Borrar todos los ficheros del directorio
      
    }
    
     
    data <- hot_to_r(input$stationTable)



    ene<-nrow(data)
  rrpivot <-rr %>% pivot_longer(starts_with('RR')) %>% rename(rr = value, Station = name) %>%
    arrange(Station,Year,Month,Day) %>% mutate(row_id = row_number(), Date = ymd(Date))

  txpivot <-tx %>% pivot_longer(starts_with('TX')) %>% rename(tx = value, Station = name) %>%
    arrange(Station,Year,Month,Day) %>% mutate(row_id = row_number(), Date = ymd(Date)) 
  
  tnpivot <-tn %>% pivot_longer(starts_with('TN')) %>% rename(tn = value, Station = name) %>%
    arrange(Station,Year,Month,Day) %>% mutate(row_id = row_number(), Date = ymd(Date))
  
  for(i in 1:ene){
    targetr<-data$station[i]; targetx<-paste0('TX',substring(targetr,3,nchar(targetr))) ; targetn<-paste0('TN',substring(targetr,3,nchar(targetr)))
    target1 <- data$min_year[i]
    target2<- data$max_year[i]
    r<-rrpivot %>% filter(Station == targetr & Year >= target1 & Year <= target2 ) %>% dplyr::select(-Date,-Station,-row_id)
    x<-txpivot %>% filter(Station == targetx & Year >= target1 & Year <= target2 ) %>% dplyr::select(-Date,-Station,-row_id)
    n<-tnpivot %>% filter(Station == targetn & Year >= target1 & Year <= target2 ) %>% dplyr::select(-Date,-Station,-row_id)
    nyu<-full_join(r,x,by = c('Year','Month','Day')) %>% full_join(n,c('Year','Month','Day')) %>% arrange(Year, Month, Day) 
    output_file <- paste0(outputdir, "/", substring(targetr,3,nchar(targetr)), ".txt")
    write.table(nyu, output_file, sep = "\t", row.names = FALSE,col.names  = FALSE, na ='-99.9')
  }
  rutaestaciones<-paste0(inputdir,'/stations.txt')
  kaka<-read.table(rutaestaciones,header=TRUE)
  kakaname<-paste0(outputdir,'/stations.txt')
  write.table(kaka,kakaname,row.names = FALSE, col.names = TRUE,quote=FALSE,sep='\t')
  

  output$dataLoadedMsg <- renderText({
    "Data extracted"
  })
  
  
   })
  
  
  observeEvent(input$loadDataButton, {
    inputdir <- input$inputdir
    
    files <- list.files(inputdir, pattern = '\\.txt$', full.names = TRUE) %>%
      grep(pattern = 'stations\\.txt', invert = TRUE, value = TRUE)
    
    data_list <- files %>%
      set_names() %>%
      map_df(~ read.delim(.x, header = FALSE, col.names = c("Year", "Month", "Day", "RR", "TX", "TN"),
                          na.strings = c("-99.9", "-999.9")) %>%
               mutate(ID = str_extract(basename(.x), "\\d+"))) 
    
    rr <<- data_list %>% dplyr::select(-TX, -TN) %>%
      pivot_wider(names_from = ID, values_from = RR, names_prefix = 'RR') %>%
      mutate(Date = make_date(Year, Month, Day)) %>%
      dplyr::select(Date, everything()) %>%
      mutate(Date = str_replace_all(Date, "-", "")) %>%
      arrange(Year, Month, Day)

      tx <<- data_list %>% dplyr::select(-RR, -TN) %>%
      pivot_wider(names_from = ID, values_from = TX, names_prefix = 'TX') %>%
      mutate(Date = make_date(Year, Month, Day)) %>%
      dplyr::select(Date, everything()) %>%
      mutate(Date = str_replace_all(Date, "-", "")) %>%
      arrange(Year, Month, Day)
 
      tn <<- data_list %>% dplyr::select(-TX, -RR) %>%
      pivot_wider(names_from = ID, values_from = TN, names_prefix = 'TN') %>%
      mutate(Date = make_date(Year, Month, Day)) %>%
      dplyr::select(Date, everything()) %>%
      mutate(Date = str_replace_all(Date, "-", "")) %>%
      arrange(Year, Month, Day)
         
    rryear <- rr %>%
      group_by(Year) %>%
      summarise(across(starts_with("RR"), ~ ifelse(sum(!is.na(.)) >= 350, sum(.), NA))) %>% ungroup
    
    txyear <- tx %>%
      group_by(Year) %>%
      summarise(across(starts_with("TX"), ~ ifelse(sum(!is.na(.)) >= 350, sum(.), NA))) %>% ungroup
    tnyear <- tn %>%
      group_by(Year) %>%
      summarise(across(starts_with("TN"), ~ ifelse(sum(!is.na(.)) >= 350, sum(.), NA))) %>% ungroup
    
    
    
    
    
    output$dataLoadedMsg <- renderText({
      "Data loaded"
    })
    
    updateSelectInput(session, "station", choices = colnames(rryear)[-1])
    
    loaded_data$data <- rryear
    

    output$stationTable <- renderRHandsontable({
      # Extract station names excluding the first column (Year)
      station_names <- names(rryear)[2:length(names(rryear))]
      
      # Create a summary table with min and max years for each station
      station_info <- rryear %>%
        dplyr::select(Year, all_of(station_names)) %>%
        gather(key = "station", value = "value", -Year) %>%
        group_by(station) %>%
        summarise(min_year = min(Year[!is.na(value)]),
                  max_year = max(Year[!is.na(value)])) %>%
        ungroup()
      rhandsontable(station_info,height = 300, editable = TRUE)    
    })
     
      # datatable(station_info, rownames = FALSE, editable = TRUE, 
      #           options = list(
      #             edit = 'cell',  # Habilitar la edición de celdas
      #             onEdit = JS("function(row, cell, value, colName) { Shiny.onInputChange('edit', [row, colName, value]); }")  # Enviar información cuando se edita una celda
      #           ))
  #  
    
  })
  
  
  
  
  run_data <- reactive({
    req(input$runButton)
    
    inputdir <- input$inputdir
    ene <- input$ene
    selected_stations <- input$station
    
    xy <- cor(loaded_data$data[, 2:ncol(loaded_data$data)], use = 'pair')
    
    i <- which(names(loaded_data$data) == selected_stations) - 1
    target <- order(xy[i, ], decreasing = TRUE)[1:ene]
    df <- loaded_data$data %>% dplyr::select(1, target + 1)
    df <- df %>% mutate(across(3:last_col(), ~ . - df[[2]]))
    
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    numeric_cols <- numeric_cols[2:length(numeric_cols)]
    corco<-as.data.frame(xy[i,target]);names(corco) = c('Correlation')
    corco$Stations<-rownames(corco)
    corco<-corco %>% dplyr::select(Stations, Correlation)
    
    output$corcoTable <- renderRHandsontable({
      rhandsontable(corco,rowHeaders = FALSE) #%>%
      #hot_table(highlightCol = TRUE, stretchH = "all")  # This line makes the table wider
    })     
    
    
    
  
    plots_list <- map2(numeric_cols, 1:length(numeric_cols), function(.x, .y) {
      ggplot(df, aes(x = Year, y = .data[[.x]])) +
        geom_line(size = 1.5, color = ifelse(.y == 1, "red", "blue"), show.legend = TRUE) +
        geom_point(size = 2, color = 'black', show.legend = TRUE) +
        labs(title = .x, x = "Year", y = .x) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          panel.grid = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray", linetype = "dashed")
        ) +
        if (.y > 1) {
          geom_hline(yintercept = 0, linetype = "dashed", color = "red")
        } else {
          NULL
        }
    })  
    plot_grid(plotlist = plots_list, ncol = 1)
  })
  
  output$plots <- renderPlot({
    run_data()
  })
  
  observeEvent(input$exitButton, {
    stopApp()
  })
}



shinyApp(ui, server)
