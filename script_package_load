## Packages ----
library(pacman)
p_load("tidyverse", "readxl",  "geomtextpath",
       "foreach", "data.table", "glue", "curl")
## ENOE Load Function ---- 
enoe_load <- function(año,n_trim ) { 
             #URL ENOES
             url_gn_enoe <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos"
             #Quarters 
             last_two_digits <- sprintf("%02d",as.numeric(substr(as.character(año), nchar(año) - 1, nchar(año))))
             #Path conditions
             #URL ENOE: TRIM I 2005  TRIM III de 2020
             if (((año >= 2005 & año <= 2019) & (n_trim <= 4))) {
               list_url <- list(glue("{url_gn_enoe}/{año}trim{n_trim}_csv.zip")) 
               #ETOE 2020
             } else if (año == 2020 & n_trim ==1 ) {
               list_url <- list(glue("{url_gn_enoe}/{año}trim{n_trim}_csv.zip"),
                                glue('ENOE_SDEMT{n_trim}{last_two_digits}.csv')) 
             
             } else if (año == 2020 & n_trim == 2) { #ETOE
    
    # data_etoe_total  <- list()
    # for (mes in c('abril', 'mayo', 'junio')) { 
    #   if (mes == 'abril'){
    #     mes_n <- 4
    #   } else if(mes == 'mayo'){
    #     mes_n <- 5
    #   } else {
    #     mes_n <- 6
    #   }
    #   list_url  <- list(glue("https://www.inegi.org.mx/contenidos/investigacion/etoe/microdatos/etoe_2020_{mes}_cpv2020_dbf.zip"),
    #                     glue("SDEMT0{mes_n}20.DBF"))
    #   url_menoe  <- list_url[[1]]
    #   path_sdem  <- list_url[[2]]
    #   # Directorio Temporal
    #   td  <- tempdir()
    #   #Archivo Temporal
    #   tf  <- tempfile(tmpdir= td, fileext = ".zip")
    #   
    #   download.file(url_menoe, tf)
    #   #Unzip
    #   unzip(tf, files = path_sdem, exdir = td,  overwrite=TRUE)
    #   #Path
    #   fpath = file.path(td,path_sdem)
    #   #Unlink
    #   unlink(td)
    #   #Load file 
    #   df_etoe <-  fread(fpath)
    #   data_etoe_total <- list(data_etoe_total,df_etoe)
    # }
    #Outcome
    etoe_link <- "https://www.inegi.org.mx/contenidos/investigacion/etoe"
    return(stop(glue("For this quarter does not exist ENOE version. Try this link for Telephone Survey of Occupation and Employment (ETOE) 2020: {etoe_link}")))
    #URL  ENOE N: TRIM III 2020 a TRIM IV 2022
  } else if ((año == 2020 & n_trim == 3 ) | ((año >= 2021 & n_trim >=1 ) & (año <= 2022  & n_trim <= 4))) {
               
               list_url <- list(glue("{url_gn_enoe}/enoe_n_{año}_trim{n_trim}_csv.zip"),
                                glue("ENOEN_SDEMT{n_trim}{last_two_digits}.csv")) 
               #URL ENOE: TRIM I 2023 
  } else if ((año == 2020 & n_trim == 4 )) {
    
               list_url <- list(glue("{url_gn_enoe}/enoe_n_{año}_trim{n_trim}_csv.zip"),
                                glue("enoen_sdemt{n_trim}{last_two_digits}.csv")) 
    
  } else if (año >= 2023 & n_trim >= 1) {
               list_url <- list(glue("{url_gn_enoe}/enoe_{año}_trim{n_trim}_csv.zip"),
                                glue("ENOE_SDEMT{n_trim}{last_two_digits}.csv")) 
             } else {
               cat("URL incorrecto")
             }
             #URL´s outcomes
             url_enoe  <- list_url[[1]]
             # Temporal Downloading
             # Temporal
             td  <- tempdir()
             #Temporal File
             tf <- tempfile(tmpdir= td, fileext = ".zip")
             dir.create(td, recursive = TRUE, showWarnings = FALSE)
             #Downloading
             #download.file(url_enoe, tf,  timeout = 500)
             curl::curl_download(url_enoe, tf, quiet = FALSE, mode = "wb")
             #Names of zipped files
             zipped_files <- as.data.frame(unzip(tf, list = TRUE,  overwrite=TRUE))$Name 
             #SDEM
             file_to_load <- zipped_files[grepl("(?i)sdem", zipped_files)]
             #Unzip 
             unzip(tf, files= file_to_load, exdir=td, overwrite=TRUE)
             #Load
             fpath <- file.path(td, file_to_load  )
             #Unlink
             unlink(td)
             #Load file
             data_enoe <- fread(fpath)
             #Outcome
             return(data_enoe)
}


#### ENOE list # ###
enoe_list <- function(start_year, end_year, start_q, end_q) {
  df_list <- list()
  for (year in start_year:end_year) {
    for (quarter in  start_q:end_q) {
      tryCatch({
      data <- enoe_load(año = year, n_trim = quarter)
      df_list[[paste0("enoe_", year, "_t", quarter)]] <- data},
      error = function(e){cat(paste("Error for Year:", year, ", Quarter:", quarter, " - Skipping. Message:", e$message, "\n"))}
      )
    }
  }
  return(df_list)
}


