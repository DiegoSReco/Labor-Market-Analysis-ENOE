# Labor Market Analysis in Mexico

The main objective of this repository is to automatically generate indicators of the labor market in Mexico. 
For this purpose, our main source of information is the National Occupation and Employment Survey (ENOE) conducted by the National Institute of Statistics and Geography (INEGI). This is a quarterly survey with a rotating panel design.

/n [ENOE site](https://www.inegi.org.mx/programas/enoe/15ymas/)

To make data loading easier, we created two functions to perform temporary downloads in R.

The first one is:
```r
enoe_load(year,quarter)
```
The arguments are the year ranging from 2005 to 2024 and the quarter ranging from 1 to 4.

The second one is:
```r
enoe_list(start_year, end_year, start_q, end_q) 
```
It is used to download data for a specific range of years and quarters. Its arguments are the start and end year, and the quarters of interest. The outcome is a list that will contain the requested surveys and you can perform computations on each survey in the list with the package [purrr(https://purrr.tidyverse.org/reference/map.html).
