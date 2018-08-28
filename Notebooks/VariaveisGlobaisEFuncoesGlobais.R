##############################################
# title: "Carrega Variaveis"
# author: "Grupo2IA"
# date: "July 23, 2018"
##############################################

# Variaveis Globais
weekdays <- c('Domingo', 'Segunda Feira', 'Terca Feira', 'Quarta Feira', 'Quinta Feira', 'Sexta Feira', 'Sabado')

if ( !exists( "ds_taxi_train" ) ) {
  ds_taxi_train <- NULL
}

if ( !exists( "ds_taxi_train_subset" ) ) {
  ds_taxi_train_subset <- NULL
}

if ( !exists("map") ) {
  long.min <- -74.07798
  long.max <- -73.85825
  lat.min <- 40.70172
  lat.max <- 40.86809
  map <- NULL
}
  
install_missing_packages <- function(needToBeFixed = FALSE,... ) {
  # Instala pacotes caso necessario
  if ( "plotly" %in% rownames(installed.packages()) == FALSE ) { install.packages("plotly", dependencies = TRUE) }
  if ( "stringr" %in% rownames(installed.packages()) == FALSE ) { install.packages("stringr", dependencies = TRUE) }
  if ( "data.table" %in% rownames(installed.packages()) == FALSE ) { install.packages("data.table", dependencies = TRUE) }
  if ( "dplyr" %in% rownames(installed.packages()) == FALSE ) { install.packages("dplyr", dependencies = TRUE) }
  if ( "ggplot2" %in% rownames(installed.packages()) == FALSE ) { install.packages("ggplot2", dependencies = TRUE) }
  if ( "gridExtra" %in% rownames(installed.packages()) == FALSE ) { install.packages("gridExtra", dependencies = TRUE) }
  if ( "grid" %in% rownames(installed.packages()) == FALSE ) { install.packages("grid", dependencies = TRUE) }
  if ( "ggmap" %in% rownames(installed.packages()) == FALSE ) { install.packages("ggmap", dependencies = TRUE) }
  if ( "h2o" %in% rownames(installed.packages()) == FALSE ) { install.packages("h2o", dependencies = TRUE) }
  if ( "knitr" %in% rownames(installed.packages()) == FALSE ) { install.packages("knitr", dependencies = TRUE) }
  if ( "plyr" %in% rownames(installed.packages()) == FALSE ) { install.packages("plyr", dependencies = TRUE) }
  if ( "tidyr" %in% rownames(installed.packages()) == FALSE ) { install.packages("tidyr", dependencies = TRUE) }
  if ( "readxl" %in% rownames(installed.packages()) == FALSE ) { install.packages("readxl", dependencies = TRUE) }
  if ( "openxlsx" %in% rownames(installed.packages()) == FALSE ) { install.packages("openxlsx", dependencies = TRUE) }
  if ( "sqldf" %in% rownames(installed.packages()) == FALSE ) { install.packages("sqldf", dependencies = TRUE) }
  if ( "DT" %in% rownames(installed.packages()) == FALSE ) { install.packages("DT", dependencies = TRUE) }
  if ( "zoo" %in% rownames(installed.packages()) == FALSE ) { install.packages("zoo", dependencies = TRUE) }
  if ( "lubridate" %in% rownames(installed.packages()) == FALSE ) { install.packages("lubridate", dependencies = TRUE) }
  if ( "rmarkdown" %in% rownames(installed.packages()) == FALSE ) { install.packages("rmarkdown", dependencies = TRUE) }
  if ( "shiny" %in% rownames(installed.packages()) == FALSE ) { install.packages("shiny", dependencies = TRUE) }
  if ( "swirl" %in% rownames(installed.packages()) == FALSE ) { install.packages("swirl", dependencies = TRUE) }
  if ( "rjson" %in% rownames(installed.packages()) == FALSE ) { install.packages("rjson", dependencies = TRUE) }
  if ( "devtools" %in% rownames(installed.packages()) == FALSE ) { install.packages("devtools", dependencies = TRUE) }
  if ( "curl" %in% rownames(installed.packages()) == FALSE ) { install.packages("curl", dependencies = TRUE) }
  if ( "visNetwork" %in% rownames(installed.packages()) == FALSE ) { install.packages("visNetwork", dependencies = TRUE) }
  if ( "readxl" %in% rownames(installed.packages()) == FALSE ) { install.packages('readxl', dependencies = TRUE) }
  if ( "lattice" %in% rownames(installed.packages()) == FALSE ) { install.packages('lattice', dependencies = TRUE) }
  if ( "readr" %in% rownames(installed.packages()) == FALSE ) { install.packages('readr', dependencies = TRUE) }
  if ( "scales" %in% rownames(installed.packages()) == FALSE ) { install.packages('scales', dependencies = TRUE) }
  if ( "dendextend" %in% rownames(installed.packages()) == FALSE ) { install.packages('dendextend', dependencies = TRUE) }
  if ( "ape" %in% rownames(installed.packages()) == FALSE ) { install.packages('ape', dependencies = TRUE) }
  
  #Para as bibliotecas abaixo sao necessarios dependencias
  if ( needToBeFixed ) {
    print("teste")
    if ( "xlsx" %in% rownames(installed.packages()) == FALSE ) { install.packages("xlsx", dependencies = TRUE) } #Necessita do RJava
    if ( "RMySQL" %in% rownames(installed.packages()) == FALSE ) { install.packages('RMySQL', dependencies = TRUE) } #Necessita do MySql ou MariaDb configurados
  }
}

load_packages <- function( ) {
  # Chamada das Bibliotecas necessarias
  require(readr)
  
  library( dplyr )
  library( lubridate ) 
  # library( DT )
  
  library( plotly )
  library( ggmap )
  library( gridExtra )
  library( grid )
  
  require("knitr")
}

load_file_dependenncies <- function() {
  # Seta Diretorio de Trabalho
  opts_knit$set(root.dir = "../", fig.height = 8, fig.width = 12, fig.align = 'center')

  WD_DATA_CSV_ZIP <- paste( getwd() ,"/../Data/train.csv.zip", sep = '') 

    # Carrega Dataset em meoria caso nao tenha sido carregada
  if ( is.null(ds_taxi_train) && file.exists( WD_DATA_CSV_ZIP )) {
    ds_taxi_train <<- read_csv(WD_DATA_CSV_ZIP)
  }
}

load_map_dependecy <- function(... ) {
  if ( is.null(map) ) {
    map <<- get_map(location = c(lon = -73.968285, lat = 40.785091), zoom = 12, source = "google") %>%
      ggmap(extent = "normal", legend="bottomleft")
  }
}

# Normaliza o Data Set para deixar apenas longitude e latitude dentro do limiar mínimo e máximo
normalization_dataset_longitude_latitude_between_min_and_max <- function(... ) {
  if ( exists("ds_taxi_train") ) {
    ds_taxi_train %>%
      filter(
          between(pickup_longitude , long.min, long.max) &
          between(dropoff_longitude , long.min, long.max) &
          between(pickup_latitude , lat.min, lat.max) &
          between(dropoff_latitude , lat.min, lat.max) 
        ) ->> ds_taxi_train_subset
  }
}


# Normaliza o Data Set para deixar apenas longitude e latitude dentro do limiar mínimo e máximo
normalization_dataset_trip_duration_between_min_and_max <- function(min, max ) {
  if ( exists("ds_taxi_train") ) {
    ds_taxi_train_subset %>%
      filter(
        between(trip_duration.minutes , min, max)
      ) ->> ds_taxi_train_subset
  }
}

calculate_manhattan_distance <- function(latatitude.min, latatitude.max, longitude.min, longitude.max ) {
  return( sqrt( ((latatitude.max - latatitude.min) + (longitude.max - longitude.min)) ^ 2 ) )
}


calculate_euclidean_distance <- function(latatitude.min, latatitude.max, longitude.min, longitude.max ) {
  return( sqrt((latatitude.max - latatitude.min) ^ 2 + (longitude.max - longitude.min) ^ 2 ))
}

convert_to_km <- function(miles ) {
  return(miles * 1.609344)
}

convert_to_m <- function(miles ) {
  return(convert_to_km(miles) / 1000)
}

# TODO - criar funcionalidade

convert_datetime_to_day_period <- function(datetime ) {
  return(datetime)
}

#Enriquecimento

# Calculando distancias
data_wrangling_distance_calculate <- function(...) {
  ds_taxi_train <<- 
    ds_taxi_train %>%
      mutate(
        distancia.manhattan = calculate_manhattan_distance(pickup_longitude, dropoff_longitude, pickup_latitude, dropoff_latitude), 
        distancia.euclidiana = calculate_euclidean_distance(pickup_longitude, dropoff_longitude, pickup_latitude, dropoff_latitude)
      )  %>%
      mutate(
        distancia.manhattan.km = convert_to_km(distancia.manhattan) ,
        distancia.manhattan.m =  convert_to_m(distancia.manhattan) ,
        distancia.euclidiana.km = convert_to_km(distancia.euclidiana) ,
        distancia.euclidiana.m =  convert_to_m(distancia.euclidiana)
      )
}

# Calcular e criar variaveis auxiliares

data_wrangling_auxiliar_variables_definition <- function( ) {
  ds_taxi_train <<- 
    ds_taxi_train %>% 
      mutate(
        trip_duration.minutes = as.integer(trip_duration / 60 ), 
        pickup_datetime = as_datetime(pickup_datetime) ,
        pickup_datetime.month = month(pickup_datetime) ,
        pickup_datetime.day = mday(pickup_datetime) ,
        pickup_datetime.weekday = weekdays[wday(pickup_datetime)] ,
        pickup_datetime.hour = hour(pickup_datetime) ,
        pickup_datetime.hour_period = convert_datetime_to_day_period(pickup_datetime) ,
        dropoff_datetime = as_datetime(dropoff_datetime) ,
        dropoff_datetime.month = month(dropoff_datetime) , 
        dropoff_datetime.day = mday(dropoff_datetime) , 
        dropoff_datetime.weekday = weekdays[wday(dropoff_datetime)] ,
        dropoff_datetime.hour = hour(dropoff_datetime) ,
        dropoff_datetime.day_period = convert_datetime_to_day_period(dropoff_datetime)
      ) %>%
      mutate( 
        pickup_datetime.weekday = factor(pickup_datetime.weekday, levels=weekdays, ordered=TRUE) ,
        dropoff_datetime.weekday = factor(dropoff_datetime.weekday, levels=weekdays, ordered=TRUE)
      )
}

# Calcular os quadrantes de inicio e fim de viagem
# TODO - criar funcionalidade para o calculo

data_wrangling_quadrant_section_definition <- function( ) {
  ds_taxi_train <<-
    ds_taxi_train %>% 
      mutate(
        quadrante.pickup = NULL ,
        quadrante.dropoff = NULL
      )
}

summarise_by_data <- function (data, data_type, page_length = 5){
  
  # Gerando Datatable
  # datatable(data, options = list(pageLength = page_length, autoWidth = TRUE, scrollX = TRUE), 
  #   caption = htmltools::tags$caption(
  #     style = 'caption-side: bottom; text-align: center;', 'Resumo das corridas: ', 
  #     htmltools::em(paste('Por ', data_type, ' - Pickup e Dropoff.'))
  #     )
  #   )
  
  data %>%
    as.data.frame()
  
  
  summary(data)
}

ploting_data <- function (data, x_breaks, y_limits, y_breaks, x_lab, y_lab, title, facet_wrap=FALSE) {
  plot <-
    ggplot(data=data, aes(x=x_value, group=1)) +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) + 
    xlab(x_lab) +
    ylab(y_lab)
  
  if ( !is.null( x_breaks ) ){
    plot <-
      plot +
        scale_x_continuous(breaks = x_breaks)
  }
  
  if ( facet_wrap == TRUE ){
    plot <- 
      plot +
      facet_wrap( ~facet_wrap )
  }
  
  # Gráfico de Pickup
  graph_pickup <- 
    plot +
    geom_line(aes(y= pickup_count)) +
    geom_hline(yintercept = median(data$pickup_count), alpha=1, linetype=2) +
    ggtitle(paste(title, " - Pickup"))
  
  # Gráfico de Dropoff
  graph_dropoff <- 
    plot +
    geom_line(aes(y= dropoff_count)) +
    geom_hline(yintercept = median(data$dropoff_count), alpha=1, linetype=2) +
    ggtitle(paste(title, " - Dropoff"))
  
  #Juntando os gráficos
  grid.arrange(graph_pickup, graph_dropoff)

}
