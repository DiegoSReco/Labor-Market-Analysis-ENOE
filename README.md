# Labor Market Analysis in Mexico

The main objective of this repository is to automatically generate indicators of the labor market in Mexico. 
For this purpose, our main source of information is the National Occupation and Employment Survey (ENOE) conducted by the National Institute of Statistics and Geography (INEGI). This is a quarterly survey with a rotating panel design.

/n [ENOE site](https://www.inegi.org.mx/programas/enoe/15ymas/)

To make data loading easier, we created two functions to perform temporary downloads
La primera es 
```r
enoe_load(year,quarter)
```
