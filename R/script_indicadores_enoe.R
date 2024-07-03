## Packages ----
library(pacman)

p_load('tidyverse', 'readxl', 
       'xlsx','shiny',
       'geomtextpath',
       'reactable','plotly', 'viridis', 
       'patchwork',  "foreach", "doParallel", 
       "survey", "data.table","ggtext", "mdthemes", 
       "glue", "scales", 
       "openxlsx")


source("scripts/script_package_load.R")

#Tratamiento de Datos ----
start_time <- Sys.time()
#ENOE´s
enoes_list_2005_2023 <- enoe_list(2005, 2023, 1,4) 
end_time <- Sys.time()
loading_time <- end_time - start_time
print(loading_time)

df_enoe <- enoe_load(2024 ,1)

#Function to estimate indicators
indicadores_enoe   <- function(df_enoe) { 
  #Primer filtro
  
  df_enoe_f <-  df_enoe |>  
                filter(r_def==0 & (c_res ==1 | c_res ==3) & (eda >= 15 &  eda<= 99))
  #Rename Variables 
  rename_dict <- c(
    "clase1" = "clas_PEA",
    "clase2" = "clas_PO",
    "emp_ppal" = "clas_INF",
    "cs_p13_2" = "años_fin",
    "cs_p13_1" = "grado_educa",
    "sex" = "sexo",
    "eda" = "edad",
    "n_hij" = "hijos",
    "hrsocup" = "n_hrs",
    "ingocup" = "ing_men",
    "ing_x_hrs" = "ing_hrs",
    "ambito2" = "dim_emp",
    "rama" = "clas_sec",
    "scian" = "clas_ind",
    "t_loc" = "t_loc",
    "rama_est2" = "eco_sec", 
    "ent" = "entidad", 
    "p14apoyos" = "apoyos"
    )
  colnames(df_enoe_f) <- rename_dict[match(colnames(df_enoe_f), 
                                                   names(rename_dict))]
  #Feauturing 
  df_enoe_f <- df_enoe_f |>  
    mutate(clas_informal = case_when(clas_INF == 1 ~ "Informal", 
                                     clas_INF == 2 ~ "Formal",
                                     TRUE   ~ NA), 
           clas_PEA_PNEA = case_when(clas_PEA ==1 ~ 'PEA', 
                                     clas_PEA == 2 ~ 'PNEA'),
           clas_ocupado = case_when(
            clas_PO == 1 ~ 'ocupados' , 
            clas_PO == 2 ~ 'desocupados',
            clas_PO == 3 ~ 'disponibles', 
            clas_PO == 4 ~ 'no disponibles',
            TRUE ~ NA),
            clas_subocup = sub_o,
            sex = case_when(sexo == 1 ~ "Hombre", 
                           sexo == 2 ~ "Mujer", 
                           TRUE ~ NA ),
           clas_pea_potencial = if_else(clas_PEA == 1 | (clas_PEA ==2 & clas_PO == 3), 1, 0),
           clas_desocup_disfrazado = if_else(clas_PEA == 2 & clas_PO == 3,1, 0),
           entidad_ab = case_when(
             entidad == 1 ~ "AGS",
             entidad == 2 ~ "BC",
             entidad == 3 ~ "BCS",
             entidad == 4 ~ "CAMP",
             entidad == 5 ~ "COAH",
             entidad == 6 ~ "COL",
             entidad == 7 ~ "CHIS",
             entidad == 8 ~ "CHIH",
             entidad == 9 ~ "CDMX",
             entidad == 10 ~ "DGO",
             entidad == 11 ~ "GTO",
             entidad == 12 ~ "GRO",
             entidad == 13 ~ "HGO",
             entidad == 14 ~ "JAL",
             entidad == 15 ~ "MEX",
             entidad == 16 ~ "MICH",
             entidad == 17 ~ "MOR",
             entidad == 18 ~ "NAY",
             entidad == 19 ~ "NL",
             entidad == 20 ~ "OAX",
             entidad == 21 ~ "PUE",
             entidad == 22 ~ "QRO",
             entidad == 23 ~ "QR",
             entidad == 24 ~ "SLP",
             entidad == 25 ~ "SIN",
             entidad == 26 ~ "SON",
             entidad == 27 ~ "TAB",
             entidad == 28 ~ "TAMS",
             entidad == 29 ~ "TLAX",
             entidad == 30 ~ "VER",
             entidad == 31 ~ "YUC",
             entidad == 32 ~ "ZAC",
             TRUE ~ "Desconocido"  
           ), 
           recibe_apo = if_else(apoyos == 1, 1 ,0)) |> 
  #          )  |> 
  # #Utilizar este filtro para ingresos  |> 
  filter(clas_PO == 1  & ing_men!=0 & n_hrs!=0)
  
  plot_dist <- 
    df_enoe_f |> 
    ggplot(aes(x = n_hrs, fill = clas_informal )) +
    geom_histogram(binwidth = 3, color = 'grey') +
    labs(title = "Distribución de horas (ENOE 2024)", x = "horas", y = "Frequency") +
    facet_wrap( ~ clas_informal)+
    
    theme_light()
  
  sity <- ggplot(TABLE, aes(x = vari)) + 
    geom_histogram(aes(y = ..density..), binwidth = bw, col = 1)
  
  # Recibe apoyo por hogar 
  #df_recibe_apoyo <- df_enoe_f |>  
  #                    mutate(id_hog = paste0(entidad, con,v_sel, n_hog, h_mud)) |> 
  #                    group_by( id_hog ,apoyos) |>
  #                    summarise(subtotal = n()) |>  
  #                    mutate(rece = if_else( any(apoyos == 1) , 'rece', "not")) |> 
  #                    ungroup() |> 
  #                     distinct(id_hog, .keep_all = TRUE)
  
    
  # df_enoe_f |> 
  #   
  #   filter(clas_ocupado  == 'ocupados' & apoyos != 3) |> 
  #   group_by(apoyos) |>
  #   summarise(subtotal = n()) |>  
  #   ungroup() |> 
  #   mutate(porcentaje = subtotal/sum(subtotal) *100)
  
 df_enoe_f_apoyos <- df_enoe_f |> 
              filter(clas_ocupado  == 'ocupados')
  
  #Estimaciones de ingreso ----
  
  #Mean with expansion factors
  list_colnames  <- colnames(df_enoe_f)
  factor <- list_colnames[grepl("(?i)fac",  list_colnames)][1]
  
  #Survey Design
  df_s_design <- svydesign(id = ~upm,
                           strata = ~est,
                           weights = as.formula(paste0("~",factor)),
                           data = df_enoe_f_apoyos,
                           nest =TRUE)
  #### PEA, PO, Brecha Laboral  -----
  # df_estados <- svyby(~clas_PEA_PNEA + clas_ocupado + clas_subocup  ,
  #                      by = ~entidad_ab + sex,
  #                      design = df_s_design,
  #                      FUN = svytotal,
  #                      na.rm = TRUE,
  #                      ci =TRUE) %>%
  #                      as.data.frame() |>
  #                      mutate(desempleo_dis = clas_ocupadodisponibles ,
  #                             pea_potencial = clas_PEA_PNEAPEA + desempleo_dis,
  #                             brecha_laboral = round((clas_ocupadodesocupados + desempleo_dis + clas_subocup) / pea_potencial,2))
  # 
  # 

  # 
  #### Tasa de informalidad laboral por estado y sexo ----
  # df_estados <- svytable(~ entidad_ab + clas_informal + sex,
  #                        design = df_s_design) |>
  #               as.data.frame()
  
  ##### Población Ocupada que recibe apoyos #####
  df_apoyos <- svytable(~  entidad_ab + recibe_apo,
                         design = df_s_design) |>
                as.data.frame() 
                
 
  # 
  # df_estados <- df_estados |>
  #               group_by(entidad_ab) |>
  #               mutate(ocupados = sum(Freq)) |>
  #               ungroup() |>
  #               group_by(entidad_ab , clas_informal) |>
  #               mutate(informalidad = sum(Freq),
  #                      tasa_informalidad = (informalidad/ocupados)) |>
  #               ungroup()  |>
  #               group_by(entidad_ab, sex) |>
  #               mutate(ocupados_sex = sum(Freq),
  #                      tasa_informalidad_sex = round(Freq/ocupados_sex, 2))  |>
  #              ungroup() |>
  #              filter(clas_informal == 'Informal')
  
  # 
  # df_nacional <- svytotal(~clas_PEA_PNEA + clas_ocupado + clas_subocup,
  #                          design = df_s_design) %>%
  #                           data.frame() %>%
  #                           t() |>
  #                           data.frame() |>
  #                           head(1) |>
  #                           mutate(desempleo_dis = clas_ocupadodisponibles ,
  #                                  pea_potencial = clas_PEA_PNEAPEA + desempleo_dis,
  #                                  brecha_laboral = (clas_ocupadodesocupados + desempleo_dis + clas_subocup) / pea_potencial)
  # df_nacional_infor <- svytable(~clas_informal,
  #                         design = df_s_design) %>%
  #                         data.frame() %>%
  #                         t() |>
  #                         data.frame()   
  
  # df_nacional_sex <- svyby(~clas_PEA_PNEA + clas_ocupado + clas_subocup  ,
  #                     by = ~sex,
  #                     design = df_s_design,
  #                     FUN = svytotal,
  #                     na.rm = TRUE,
  #                     ci =TRUE) %>%
  #   as.data.frame() |>
  #   mutate(desempleo_dis = clas_ocupadodisponibles ,
  #          pea_potencial = clas_PEA_PNEAPEA + desempleo_dis,
  #          brecha_laboral = round((clas_ocupadodesocupados + desempleo_dis + clas_subocup) / pea_potencial,2))
  
  # Medias de ingreso por condición en la ocupación
  
  # df_informal_income <- svyby(~ing_men , 
  #                             by = ~clas_informal, 
  #                             design = df_s_design,
  #                             na.rm.by = TRUE,
  #                             FUN = svymean)
  return( df_apoyos)
  
}
#Table for each df
df_indicadores_table <- map(enoes_list_2005_2023, indicadores_enoe)

#Only one df 
#df_indicadores_new <- indicadores_enoe(df_enoe) 
#Stacked Data Frame with id  
df_stacked <- bind_rows(df_indicadores_table, .id = "id_df") |> 
              mutate(id_df = str_remove(id_df, pattern = 'enoe_'))

#DF ENOE 2024 
#df_apoyos <- enoe_load(2024, 1)
df_apoyos <-   df_apoyos |>  
               mutate(id_df = '2024_t1')

df_stacked <- df_stacked |>  bind_rows(df_apoyos)


#New indicators table -----
# Creating XLSX file with indicators ----
if (file.exists("tablas_indicadores.xlsx")) {
  wb <- loadWorkbook("tablas_indicadores.xlsx")
  addWorksheet(wb, "tab_ocupados_apoyos")
  writeData(wb, sheet = "tab_ocupados_apoyos", x = df_stacked)
  saveWorkbook(wb, "tablas_indicadores.xlsx", overwrite = TRUE)
  
} else {
  wb <- createWorkbook()
  addWorksheet(wb, "tab_ocupados_apoyos")
  writeData(wb, sheet = "tab_ocupados_apoyos", x = df_stacked)
  saveWorkbook(wb, "tablas_indicadores.xlsx")
}

#Updating tables ---- 
df_tables_updated <- read_xlsx("tablas_indicadores.xlsx", sheet = "tab_informalidad_sex_nacional")
#Update the last vaue 
current_value <- as.numeric(str_extract(max(df_tables_updated$id_df), "\\d+$"))
new_value <- current_value + 1
new_id <- paste0("2023_t", new_value)
#New data with the new ID 
df_indicadores_new <- df_indicadores_new |>  
                      mutate(id_df =new_id) 
rownames(df_indicadores_new ) <- NULL
df_tables_updated  <- df_tables_updated |>  
                      bind_rows(df_indicadores_new)
#Updating 
wb <- loadWorkbook("tablas_indicadores.xlsx")
writeData(wb, sheet = "tab_informalidad_jov", x = df_tables_updated)
saveWorkbook(wb, "tablas_indicadores.xlsx", overwrite = TRUE)

df_tables_updated <- read_xlsx("tablas_indicadores.xlsx", sheet = "tab_ocupados_apoyos")


#Plot 1: Tendencias ----
my_theme <- function() { 
  
  theme_void() +
    theme(
     # plot.background = element_rect(fill = "grey88"),
      plot.title = element_markdown( vjust = 0,
                                     size=16,face ='bold', color = "black"),
      #plot.subtitle =   element_text(hjust = 0, lineheight = 1, size=12, color = "#5c5c5c"),
      plot.subtitle = element_markdown(size =14),
      plot.caption =  element_text(hjust = 0, size=8),
      legend.position = c("bottom"),
      text = element_text(size=14) )
      
} 

pal <- (c("#3E4A89FF",  "#6DCD59FF"))

date_indicadores_com  <- df_tables_updated |>  
                         mutate(tasa_desempleo = clas_ocupadodesocupados/clas_PEA_PNEAPEA)
#CoVID-19 T2
df_t2_2020 <- tibble(id_df = c('2020_t2'),
                     tasa_desempleo =c(NA))
date_indicadores_com <- date_indicadores_com |> bind_rows(df_t2_2020)

plot_1 <- date_indicadores_com %>%
  ggplot(aes(x= id_df, y= tasa_desempleo* 100,  group =1 )) +
  geom_rect(aes(xmin = "2008_t3", xmax = "2009_t4", ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.3) +
  geom_line(color ="slateblue1", size = .9 )+ 
  geom_point(color ="slateblue1", size = 1.4) +
  geom_text(data = date_indicadores_com %>% filter(tasa_desempleo ==max(tasa_desempleo, na.rm =TRUE) | id_df == '2020_t3' ),
            aes(label = paste0(round(tasa_desempleo*100,1),"%")), 
            position = position_dodge(width = 0.9), hjust=-0.17,
            color = "brown3", size = 3.2)+ 
  geom_vline(xintercept=date_indicadores_com$id_df[is.na(date_indicadores_com$tasa_desempleo)], linetype="dashed", color = "#5c5c5c") +
  geom_text(data = date_indicadores_com %>% filter( id_df == new_id),
            aes(label = paste( paste0(round(tasa_desempleo*100,1),"%"))), 
            position = position_dodge(width = 1.5), vjust= 1.3, hjust= .7,
            color ="seagreen" , size = 3.1)+
  

  annotate("text", x = date_indicadores_com$id_df[is.na(date_indicadores_com$tasa_desempleo)],
            y = -Inf, label = "Covid-19", vjust = -0.5,hjust = -2.5, color = "#5c5c5c", angle = 90) +
  annotate("text", x = "2009_t4",
           y = -Inf, label = "Crisis Financiera", vjust = -0.5,hjust = -.7, color = "#5c5c5c", angle = 90) +
  labs( y="", x= "",
        title = "Tasa de desempleo en México",
        subtitle =  "Histórico del trimestre I de 2005 al trimestre IV de 2023",
          
        caption="Fuente: Estimaciones propias con datos de la ENOE, ENOE-N | @Diegosreco. Nota: Para el trimestre II de 2020 no se levantó la encuesta.")  +
  my_theme()  +
  scale_y_continuous(labels = percent_format(scale = 1))
plot_1
# Plot 2: MAP -----
p_load('sf', 'scales', 'cowplot')
mapa <- st_read('mapa_nac') 
#Using iconv function to fix variables
mapa[["NOMGEO"]] <- iconv(mapa[["NOMGEO"]],
                          from = "latin1",
                          to = "UTF-8")

#Abbreviation to match
state_abbreviations <- c(
  "Aguascalientes" = "AGS",
  "Baja California" = "BC",
  "Baja California Sur" = "BCS",
  "Campeche" = "CAMP",
  "Coahuila de Zaragoza" = "COAH",
  "Colima" = "COL",
  "Chiapas" = "CHIS",
  "Chihuahua" = "CHIH",
  "Ciudad de México" = "CDMX",
  "Durango" = "DGO",
  "Guanajuato" = "GTO",
  "Guerrero" = "GRO",
  "Hidalgo" = "HGO",
  "Jalisco" = "JAL",
  "México" = "MEX",
  "Michoacán de Ocampo" = "MICH",
  "Morelos" = "MOR",
  "Nayarit" = "NAY",
  "Nuevo León" = "NL",
  "Oaxaca" = "OAX",
  "Puebla" = "PUE",
  "Querétaro" = "QRO",
  "Quintana Roo" = "QR",
  "San Luis Potosí" = "SLP",
  "Sinaloa" = "SIN",
  "Sonora" = "SON",
  "Tabasco" = "TAB",
  "Tamaulipas" = "TAMS",
  "Tlaxcala" = "TLAX",
  "Veracruz de Ignacio de la Llave" = "VER",
  "Yucatán" = "YUC",
  "Zacatecas" = "ZAC"
)

# Use mutate to add a new column with state abbreviations
mapa <- mapa %>%
        mutate(entidad_ab = state_abbreviations[NOMGEO])

df_estados <- df_estados |>  
              mutate(CVE_ENT = str_pad(row_number(), width = 2, pad = "0"))
mapa_indicadores <- mapa  |>  inner_join(df_estados, by = 'entidad_ab')
#Filter 
mapa_indicadores_f <- mapa_indicadores |> 
                      filter(!(entidad_ab %in% c( "CDMX", "HGO", "QRO", "MEX", "PUE", "TLAX", "MOR")))
mapa_indicadores_subset <- mapa_indicadores |> 
                            filter(entidad_ab %in% c( "CDMX", "HGO", "QRO", "MEX", "PUE", "TLAX", "MOR"))

#Define the bounding box for the specified states  (lat-lon limits)
bbox_selected_states <- st_bbox(mapa_indicadores_subset)

main_map <- mapa_indicadores |> 
ggplot() +
geom_sf( aes(fill = tasa_informalidad_sex )) +
geom_sf_text( data= mapa_indicadores_f, aes(label= scales::percent(tasa_informalidad_sex)),
             colour = 'black', size = 2.5)+
scale_fill_gradientn(
    colors = c("#9DBF9E", "#FCB97D", "#A84268"),
    na.value = "grey80",
    limits = c(0.30, 0.85),
    oob = scales::squish,
    labels = scales::percent,
    breaks = seq(0.30, 0.85, 0.10),
    name = "") +
  geom_rect(
    xmin = bbox_selected_states$xmin,
    ymin = bbox_selected_states$ymin,
    xmax = bbox_selected_states$xmax,
    ymax = bbox_selected_states$ymax,
    fill = NA,
    colour = "darkgreen",
    size = 0.3
  )+
facet_wrap(~sex)+
labs( title = "Tasa de informalidad laboral a nivel estatal por sexo",
      subtitle ="En el III trimestre de 2023 la tasa informalidad laboral para los hombres fue de 48.5%  y para las mujeres de 54.8%") +
my_theme()

#Zoom map
zoomed_plot <- main_map +
  coord_sf(xlim = c(bbox_selected_states$xmin, bbox_selected_states$xmax),
           ylim = c(bbox_selected_states$ymin, bbox_selected_states$ymax),
           expand = FALSE ) + 
  labs(title = "", subtitle = "", caption = "") + 
  theme(legend.position = "none") + 
  geom_sf_text( data= mapa_indicadores_subset, aes(label= scales::percent(tasa_informalidad_sex)), 
                colour = 'black', size = 2)
  
#Show the two plots joinned 
ggdraw() +
  draw_plot(main_map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(zoomed_plot, x = .35, y = 0, width = 0.35, height = 0.35) 

  


#PLOT 3: Hexbin Map ---- 
p_load("mxmaps")
#MAP 
data_mex <- mxhexbin.map |>  
            mutate(entidad_ab = case_when( 
                                state_abbr == 'CHPS'~ 'CHIS',
                                state_abbr == 'DF'~ 'CDMX',
                                state_abbr == 'QRO'~ 'QRO',
                                state_abbr == 'QROO'~ 'QR',
                                state_abbr == 'TAM'~ 'TAMS',
                                TRUE ~ state_abbr )) 


#Indicadores 

df_stacked <- read_xlsx("tablas_indicadores.xlsx", sheet = "tab_ocupados_apoyos")

df_ent_apoyos <- df_stacked |>  
                 filter(id_df %in% c("2018_t1","2024_t1"))   |> 
                 group_by(id_df, entidad_ab  ) |>  
                 mutate(ocupados_total = sum(Freq, na.rm = TRUE),
                        Porcentaje = Freq/ocupados_total) %>%
                 ungroup()  |> 
                 filter(recibe_apo == 1) |>  
                 mutate(id_df = as.integer(str_extract_all(id_df, "^\\d{4}"))) |> 
                 group_by( entidad_ab, recibe_apo ) |>  
                 mutate(lag_porcentaje = lag(Porcentaje)) |> 
                 ungroup() |> 
                 mutate(difference_2018_2021 = Porcentaje -lag_porcentaje ) |> 
                 filter(!is.na(lag_porcentaje))
  
View(df_ent_apoyos)



df_brecha_estados_lat_long <- df_ent_apoyos |>  
 
                              inner_join(data_mex, by = "entidad_ab" )  
  
  
center_points <- df_brecha_estados_lat_long %>%
                group_by(entidad_ab) %>%
                summarize(center_lat = mean(lat),
                          center_long = mean(long),
                          Porcentaje = mean(Porcentaje)) 

# Create a color palette function
color_palette <- colorRampPalette(c("cornflowerblue",  "brown3"))
num_colors <- 10  # Specify the number of colors you want
palette_colors <- color_palette(num_colors)

my_theme <- function() { 
  
  theme_void() +
    theme(
      # plot.background = element_rect(fill = "grey88"),
      plot.title = element_markdown( vjust = 0,
                                     size=14,face ='bold', color = "black", hjust = 0.5),
      #plot.subtitle =   element_text(hjust = 0, lineheight = 1, size=12, color = "#5c5c5c"),
      plot.subtitle = element_markdown(size =12,vjust = 0, hjust = 0.5),
      plot.caption =  element_text(hjust = 0, size=8),
      legend.position = c("bottom"),
      text = element_text(size=12) )
  
} 

df_map_apoyos <- ggplot() +
  geom_polygon(data = df_brecha_estados_lat_long, aes(fill = Porcentaje*100, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data =center_points, aes(x =center_long , y = center_lat, label = entidad_ab), size = 3, color = "gray4") +  # Replace state_name_variable with the variable containing state names
  geom_text(data =center_points, aes(x =center_long , y = center_lat-.8, label =  paste0(round(Porcentaje*100,1), "%") ), size = 3, color = "white") +  # Replace state_name_variable with the variable containing state names
  
  scale_fill_gradientn(name = "%",colors = palette_colors) +
  labs( title = "Porcentaje de personas ocupadas que reciben apoyos económicos",
        subtitle ="Encuesta Nacional de Ocupación y Empleo (ENOE) Trimestre I 2024", 
        caption = "Fuente: Estimaciones propias con la ENOE Trimestre I 2024 @Diegosreco") +
  my_theme()
  


## Plot Differences 
my_theme <- function() { 
  
  theme_minimal() +
    theme(
      # plot.background = element_rect(fill = "grey88"),
      plot.title = element_markdown( vjust = 0,
                                     size=14,face ='bold', color = "black", hjust = 0.5),
      #plot.subtitle =   element_text(hjust = 0, lineheight = 1, size=12, color = "#5c5c5c"),
      plot.subtitle = element_markdown(size =12,vjust = 0, hjust = 0.5),
      plot.caption =  element_text(hjust = 0, size=8),
      legend.position = c("none"),
      text = element_text(size=12) )
  
}


df_diff_apoyos <- df_ent_apoyos  %>%  
                   ggplot(aes(x=  reorder(entidad_ab, difference_2018_2021), y=(difference_2018_2021)*100, fill = difference_2018_2021 , 
                              text=paste(difference_2018_2021,"%")))+
                   geom_text(aes(label = paste0(round((difference_2018_2021)*100,1))) , 
                             hjust = ifelse(df_ent_apoyos$difference_2018_2021 < 0, 1.1, -0.1),  # Adjust the position based on value
                             color = "black",  # Set text color to black
                             size = 4) +    
                   geom_col() +
                   
                   coord_flip() +
                   my_theme()+
                   scale_fill_gradientn(name = "%",colors = rev(palette_colors) ,labels = scales::percent_format(scale = 1)) +
                   
                   labs(y="%",
                        x= "",
                        title = "Cambio en el % de personas ocupadas que reciben apoyos económicos ",
                        subtitle = "Trimestre I 2018 vs Trimestre I 2024",
                        caption="Fuente: ENOE | @Diegosreco") 
                   
df_diff_apoyos



df_map_apoyos + df_diff_apoyos
