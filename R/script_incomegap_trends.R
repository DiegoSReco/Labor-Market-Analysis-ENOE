## Packages ----
library(pacman)

p_load('tidyverse', 'readxl', 
       'xlsx','shiny', 'viridis',  
       "glue", "scales")

#### Load Data -----
df_tables_updated <- read_xlsx("df_estim_income_gap.xlsx)

#### Transform Data ----

df_table_inforcomp <- df_tables_updated |> 
  group_by(id_df, sex) |> 
  mutate(SexWithinInf = ing_men[clas_informal == 'Informal'] / ing_men[clas_informal == 'Formal']) |> 
  ungroup()  |>  
  group_by(id_df, clas_informal) |>  
  mutate(  SexBetweenInf = ing_men[sex == 'Mujer'] / ing_men[sex == 'Hombre'])
df_within <- df_table_inforcomp |>  
             filter(clas_informal == 'Formal')  |> 
             select(-c('SexBetweenInf')) |>  
             mutate(  Comparativa = paste(sex, 'Informal vs',sex,'Formal'))

df_between <- df_table_inforcomp |>  
             filter(sex == 'Hombre')  |> 
             select(-c('SexWithinInf')) |>  
             mutate(  Comparativa = paste('Mujer', clas_informal,'vs', 'Hombre',clas_informal))
#Add missing data from 2020 Q2 
df_t2_2020 <- tibble(id_df = c('2020_t2',  '2020_t2'),
                     SexBetweenInf =c(NA, NA), 
                     Comparativa = c('Mujer Formal vs Hombre Formal', 'Mujer Informal vs Hombre Informal')
                     )
df_between <- df_between |> bind_rows(df_t2_2020)

df_t2_2020 <- tibble(id_df = c('2020_t2',  '2020_t2'),
                     SexWithinInf =c(NA, NA), 
                     Comparativa = c("Hombre Informal vs Hombre Formal", "Mujer Informal vs Mujer Formal"))

df_within <- df_within |> bind_rows(df_t2_2020)

#Means of percentages
df_between |>  
  group_by(Comparativa) |>  
  summarise(average = mean(SexBetweenInf, na.rm = TRUE))

df_within |>  
  group_by(Comparativa) |>  
  summarise(average = mean(SexWithinInf, na.rm = TRUE))

#Plot 1: Tendencias ----
my_theme <- function() { 
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white"),
      plot.title.background = element_rect(fill = "grey90", color = NA),
      plot.title = element_markdown( vjust = 0,
                                     size=16,face ='bold', color = "midnightblue"),
      #plot.subtitle =   element_text(hjust = 0, lineheight = 1, size=12, color = "#5c5c5c"),
      plot.subtitle = element_markdown(size =14, vjust = 0, color = "midnightblue"),
      plot.caption =  element_text(hjust = 0, size=8),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 10),
      legend.position = c("top"),
      text = element_text(size=14))
  
}  

plot_trends <- df_between |> 
               ggplot(aes(x= id_df, y= SexBetweenInf* 100, 
                          color = Comparativa, linetype = Comparativa, group = Comparativa)) +
               
               geom_line( size = .9 )+ 
               geom_line( data = df_within, 
                          aes(x= id_df, y= SexWithinInf * 100, 
                              color = Comparativa, linetype = Comparativa, group = Comparativa),
                          size = .9 )+ 
               
               geom_point( size = 1.4) +
               geom_vline(xintercept=df_between$id_df[is.na(df_between$SexBetweenInf)], linetype="dashed", color = "#5c5c5c") +
               annotate("text", x = df_between$id_df[is.na(df_between$SexBetweenInf)],
                         y = -Inf, label = "Covid-19", vjust = -0.5,hjust = -3.5, color = "#5c5c5c", angle = 90) +
               labs( y="", x= "",
                     title = "Brecha en ingresos laborales por sexo y condición en el empleo",
                     subtitle =  "(trimestre I de 2005 - trimestre I de 2024)",
                     
                     caption="Fuente: Estimaciones propias con datos de la ENOE, ENOE-N | @Diegosreco. Nota: Para el trimestre II de 2020 no se levantó la encuesta.")  +
               my_theme()  +
               scale_y_continuous(labels = percent_format(scale = 1)) + 
               scale_color_viridis(name = "",discrete = TRUE) +
               scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"), name = "") +
               guides(color = guide_legend(ncol = 2, override.aes = list(linetype = c("solid", "dashed", "dotted", "dotdash"))),
               linetype = guide_legend(ncol = 2))
plot_trends







