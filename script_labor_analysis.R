rm(list=ls()) 
## Packages ----
library(pacman)
p_load('tidyverse', "survey", "data.table","ggtext", "mdthemes")

#URL ENOE 2023  
url_enoe_n <-"https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/enoe_2023_trim2_csv.zip"

# Directorio Temporal
td  <- tempdir()
#Archivo Temporal
tf = tempfile(tmpdir=td, fileext=".zip")

download.file(url_enoe_n, tf)

# Unzip
unzip(tf, files="ENOE_SDEMT223.csv", exdir=td, 
      overwrite=TRUE)
#Path
fpath=file.path(td,"ENOE_SDEMT223.csv")
#Unlink
unlink(td)
#Load file
df_enoe_2023_t2 <- fread(fpath)

#Tratamiento de Datos ----
#Filtros especiales para usar variables correctamente

df_enoe_2023_t2_f <- df_enoe_2023_t2 |>  
                     filter(r_def==0 & (c_res ==1 | c_res ==3) & (eda >= 15 &  eda<=98))
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
                  "ent" = "entidad")
colnames(df_enoe_2023_t2_f) <- rename_dict[match(colnames(df_enoe_2023_t2_f), 
                                                 names(rename_dict))]
#Feauturing 
df_enoe_2023_t2_f <- df_enoe_2023_t2_f |>  
                     mutate(clas_informal = case_when(clas_INF == 1 ~ "Informal", 
                                                      clas_INF == 2 ~ "Formal",
                                                      TRUE   ~ NA),
                            clas_ocupado = if_else(clas_PO == 1 , 1, 0),
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
                            ))  |>  
                     filter(clas_ocupado ==1 &  n_hrs!=0 & ing_men!=0 )



#Estimaciones de ingreso ----
#Mean with sample data
table_sample_mean <- df_enoe_2023_t2_f |>  
  filter(clas_ocupado ==1) |> 
  group_by(entidad_ab) |> 
  summarise(mean_laboral_wage = mean(ing_men, na.rm =TRUE),
            mean_labora_hrs   = mean(n_hrs),
            mean_ing_hrs   =mean(ing_hrs, na.rm = TRUE))

#Mean with expansion factors
#Aplicamos diseño de encuesta probabilística
df_s_design <- svydesign(id = ~upm,
                         strata = ~est,
                         weights = ~fac_tri ,
                         data = df_enoe_2023_t2_f )

#Table 1
mean_table_infor <- svyby(~ing_men + n_hrs,
                          by = ~clas_informal, 
                          design = df_s_design,
                          FUN = svymean,
                          na.rm = TRUE,
                          ci = TRUE)

#Table 2
means_table_entidad <- svyby(~ing_men + n_hrs,
                             by = ~ entidad_ab + clas_informal, 
                             design = df_s_design,
                             FUN = svymean,
                             na.rm = TRUE,
                             ci = TRUE)


## PLOT ----
my_theme <- function() { 
  
  theme_bw()+
    theme(
      plot.title = element_text( vjust = 1,
                                 size=18, colour = "darkgrey", face ='bold'),
      #plot.subtitle =   element_text(hjust = -.3, lineheight = 1, size=14),
      plot.subtitle = element_markdown(size =13),
      plot.caption =  element_text(hjust = 0, size=11),
      legend.position = c("none"),
      # panel.grid.major = element_line(size = 2),
      text = element_text(size=12),
      axis.text.y = element_text(face = "bold"),
      axis.text.x = element_text(face = "bold"), 
      strip.text =  element_text(face = "bold"),
      strip.background = element_rect(fill = "grey", colour = "grey" )) 
} 

pal <- c("#440154FF", "#31688EFF" )

plot_mean_informalidad_emt <- means_table_entidad  %>%
                              ggplot(aes(x= n_hrs, y= ing_men)) +
                              geom_point( aes(color = clas_informal),size= 3,alpha=1) +
                              geom_text( aes(label = entidad_ab), 
                                         size = 2.8 ,nudge_y =  260) +
                              geom_vline(data = mean_table_infor, aes(xintercept = n_hrs, color =clas_informal ), 
                                         linetype= "dashed", size =1)+
                              geom_hline(data = mean_table_infor, aes(yintercept = ing_men , color =clas_informal ), 
                                         linetype= "dotted", size =1)+
                              facet_grid( col= vars(clas_informal), scales = "free") +
                              labs(y="Ingreso laboral promedio mensual ($/MXN)",
                                   x= "Horas laborales promedio semanales",
                                   title = "Relación ingreso laboral y horas de trabajo por entidad y condición de informalidad (2023 2T)",
                                   subtitle =  
                                     paste0("A nivel nacional, el ingreso promedio mensual de los trabajadores
                                                        <span style='color: #440154FF;'>**formales fue de $11,588  y las horas de trabajo promedio semanales fueron 46**</span>;  
                                                        mientras que para los
                                                        <span style='color: #31688EFF;'>**informales el ingreso fue de $6,399 y 39 horas**</span>" ) ,
                                   caption="Fuente: Estimaciones propias con datos de la ENOE 2023 2T | @Diegosreco Nota: No se utilizan métodos de imputación para ingresos no reportados")  +
                              my_theme() +
                              scale_color_manual(name = "Categoría", values = c(pal))  


plot_mean_informalidad_emt
