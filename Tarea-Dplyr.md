Tarea 3 dplyr
================
Kevin Huanca
18/1/2022

##DESARROLLO DE LA TAREA DPLYR # Comenzamos llamando a la libreria
dplyr, nycflingts13 y ggplt2 library(dplyr) library(tidyverse)
library(ggplot2) library(nycflights13) nycflights13::flights

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.1.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(nycflights13)
```

    ## Warning: package 'nycflights13' was built under R version 4.1.2

``` r
nycflights13::flights
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
##1.
    flights %>% 
      filter(arr_time>=120)
```

    ## # A tibble: 319,999 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 319,989 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

##2.

``` r
   vuelos01 <-flights %>%
       filter(dest=="IAH" | dest=="HOU")
vuelos01     
```

    ## # A tibble: 9,313 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      623            627        -4      933            932
    ##  4  2013     1     1      728            732        -4     1041           1038
    ##  5  2013     1     1      739            739         0     1104           1038
    ##  6  2013     1     1      908            908         0     1228           1219
    ##  7  2013     1     1     1028           1026         2     1350           1339
    ##  8  2013     1     1     1044           1045        -1     1352           1351
    ##  9  2013     1     1     1114            900       134     1447           1222
    ## 10  2013     1     1     1205           1200         5     1503           1505
    ## # ... with 9,303 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

##3.

``` r
flights %>%
  filter(carrier=="UA" | carrier=="AA" | carrier=="DL")
```

    ## # A tibble: 139,504 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      554            600        -6      812            837
    ##  5  2013     1     1      554            558        -4      740            728
    ##  6  2013     1     1      558            600        -2      753            745
    ##  7  2013     1     1      558            600        -2      924            917
    ##  8  2013     1     1      558            600        -2      923            937
    ##  9  2013     1     1      559            600        -1      941            910
    ## 10  2013     1     1      559            600        -1      854            902
    ## # ... with 139,494 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

##4.

``` r
vuelos02 <-flights %>% filter(month == 7 | month == 8 | month == 9)

vuelos02
```

    ## # A tibble: 86,326 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     7     1        1           2029       212      236           2359
    ##  2  2013     7     1        2           2359         3      344            344
    ##  3  2013     7     1       29           2245       104      151              1
    ##  4  2013     7     1       43           2130       193      322             14
    ##  5  2013     7     1       44           2150       174      300            100
    ##  6  2013     7     1       46           2051       235      304           2358
    ##  7  2013     7     1       48           2001       287      308           2305
    ##  8  2013     7     1       58           2155       183      335             43
    ##  9  2013     7     1      100           2146       194      327             30
    ## 10  2013     7     1      100           2245       135      337            135
    ## # ... with 86,316 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

##5

``` r
    flights %>% 
        filter(arr_time>=120, dep_delay<=0)
```

    ## # A tibble: 198,326 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      544            545        -1     1004           1022
    ##  2  2013     1     1      554            600        -6      812            837
    ##  3  2013     1     1      554            558        -4      740            728
    ##  4  2013     1     1      555            600        -5      913            854
    ##  5  2013     1     1      557            600        -3      709            723
    ##  6  2013     1     1      557            600        -3      838            846
    ##  7  2013     1     1      558            600        -2      753            745
    ##  8  2013     1     1      558            600        -2      849            851
    ##  9  2013     1     1      558            600        -2      853            856
    ## 10  2013     1     1      558            600        -2      924            917
    ## # ... with 198,316 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

##6

``` r
    flights %>%
       filter(dep_delay - arr_delay>=30, arr_delay>=60)
```

    ## # A tibble: 1,217 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1     1716           1545        91     2140           2039
    ##  2  2013     1     1     2205           1720       285       46           2040
    ##  3  2013     1     1     2326           2130       116      131             18
    ##  4  2013     1     3     1503           1221       162     1803           1555
    ##  5  2013     1     3     1821           1530       171     2131           1910
    ##  6  2013     1     3     1839           1700        99     2056           1950
    ##  7  2013     1     3     1941           1759       102     2246           2139
    ##  8  2013     1     3     2257           2000       177       45           2224
    ##  9  2013     1     4     1917           1700       137     2135           1950
    ## 10  2013     1     4     2010           1745       145     2257           2120
    ## # ... with 1,207 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

##7

``` r
    flights %>%
        filter(dep_time==2400 | dep_time<=600)
```

    ## # A tibble: 9,373 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 9,363 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

##2.Otra función de dplyr que es útil para usar filtros es between().
¿Qué hace? ¿Puedes usarla para simplificar el código necesario para
responder a los desafíos anteriores?

``` r
#Esta funcion nos permite filtrar filas que se encuentren dentro de un limite min y max que establece between
    lim_inf <-7 #julio
    lim_sup <-9 #septiemnbre
    x <-8
    between(lim_inf,lim_sup,x)
```

    ## [1] FALSE

#3.¿Cuántos vuelos tienen datos faltantes en horario_salida? ¿Qué otras
variables tienen valores faltantes? ¿Qué representan estas filas?

``` r
    Datos_Na <-flights$sched_dep_time
    
  ## is.na(Datos_Na)  
sum(is.na(Datos_Na))
```

    ## [1] 0

##Parte 2: Dplyr - arrange ##1

``` r
names(flights)[colSums(is.na(flights)) >0]
```

    ## [1] "dep_time"  "dep_delay" "arr_time"  "arr_delay" "tailnum"   "air_time"

``` r
flights %>% 
  arrange(desc(is.na(dep_time)),
          desc(is.na(dep_delay)),
          desc(is.na(arr_time)), 
          desc(is.na(arr_delay)),
          desc(is.na(tailnum)),
          desc(is.na(air_time)))
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     2       NA           1545        NA       NA           1910
    ##  2  2013     1     2       NA           1601        NA       NA           1735
    ##  3  2013     1     3       NA            857        NA       NA           1209
    ##  4  2013     1     3       NA            645        NA       NA            952
    ##  5  2013     1     4       NA            845        NA       NA           1015
    ##  6  2013     1     4       NA           1830        NA       NA           2044
    ##  7  2013     1     5       NA            840        NA       NA           1001
    ##  8  2013     1     7       NA            820        NA       NA            958
    ##  9  2013     1     8       NA           1645        NA       NA           1838
    ## 10  2013     1     9       NA            755        NA       NA           1012
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

##2

``` r
flights %>% 
  arrange(dep_delay)
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013    12     7     2040           2123       -43       40           2352
    ##  2  2013     2     3     2022           2055       -33     2240           2338
    ##  3  2013    11    10     1408           1440       -32     1549           1559
    ##  4  2013     1    11     1900           1930       -30     2233           2243
    ##  5  2013     1    29     1703           1730       -27     1947           1957
    ##  6  2013     8     9      729            755       -26     1002            955
    ##  7  2013    10    23     1907           1932       -25     2143           2143
    ##  8  2013     3    30     2030           2055       -25     2213           2250
    ##  9  2013     3     2     1431           1455       -24     1601           1631
    ## 10  2013     5     5      934            958       -24     1225           1309
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
retraso <-flights %>% 
  arrange(desc(dep_delay)) 
  
head(retraso,5)
```

    ## # A tibble: 5 x 19
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     1     9      641            900      1301     1242           1530
    ## 2  2013     6    15     1432           1935      1137     1607           2120
    ## 3  2013     1    10     1121           1635      1126     1239           1810
    ## 4  2013     9    20     1139           1845      1014     1457           2210
    ## 5  2013     7    22      845           1600      1005     1044           1815
    ## # ... with 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

##3

``` r
velocidad <-flights %>%
  arrange(desc(air_time))
head(velocidad,5)
```

    ## # A tibble: 5 x 19
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     3    17     1337           1335         2     1937           1836
    ## 2  2013     2     6      853            900        -7     1542           1540
    ## 3  2013     3    15     1001           1000         1     1551           1530
    ## 4  2013     3    17     1006           1000         6     1607           1530
    ## 5  2013     3    16     1001           1000         1     1544           1530
    ## # ... with 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

##4

``` r
distancia <-flights %>%
  arrange(desc(distance))
head(distancia,5)
```

    ## # A tibble: 5 x 19
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     1     1      857            900        -3     1516           1530
    ## 2  2013     1     2      909            900         9     1525           1530
    ## 3  2013     1     3      914            900        14     1504           1530
    ## 4  2013     1     4      900            900         0     1516           1530
    ## 5  2013     1     5      858            900        -2     1519           1530
    ## # ... with 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
tail(distancia,5)
```

    ## # A tibble: 5 x 19
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     3    16     1947           1950        -3     2055           2044
    ## 2  2013     3    23     1946           1950        -4     2030           2044
    ## 3  2013     3    30     1942           1950        -8     2026           2044
    ## 4  2013     4     6     1948           1950        -2     2034           2044
    ## 5  2013     7    27       NA            106        NA       NA            245
    ## # ... with 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

#Parte 3: Dplyr - select ##1

``` r
flights %>%
  select(dep_time, dep_delay, arr_time, arr_delay)
```

    ## # A tibble: 336,776 x 4
    ##    dep_time dep_delay arr_time arr_delay
    ##       <int>     <dbl>    <int>     <dbl>
    ##  1      517         2      830        11
    ##  2      533         4      850        20
    ##  3      542         2      923        33
    ##  4      544        -1     1004       -18
    ##  5      554        -6      812       -25
    ##  6      554        -4      740        12
    ##  7      555        -5      913        19
    ##  8      557        -3      709       -14
    ##  9      557        -3      838        -8
    ## 10      558        -2      753         8
    ## # ... with 336,766 more rows

``` r
flights %>%
  select(starts_with("dep"), starts_with("arr"))
```

    ## # A tibble: 336,776 x 4
    ##    dep_time dep_delay arr_time arr_delay
    ##       <int>     <dbl>    <int>     <dbl>
    ##  1      517         2      830        11
    ##  2      533         4      850        20
    ##  3      542         2      923        33
    ##  4      544        -1     1004       -18
    ##  5      554        -6      812       -25
    ##  6      554        -4      740        12
    ##  7      555        -5      913        19
    ##  8      557        -3      709       -14
    ##  9      557        -3      838        -8
    ## 10      558        -2      753         8
    ## # ... with 336,766 more rows

``` r
flights %>%
  select("dep_time", "dep_delay", "arr_time", "arr_delay")
```

    ## # A tibble: 336,776 x 4
    ##    dep_time dep_delay arr_time arr_delay
    ##       <int>     <dbl>    <int>     <dbl>
    ##  1      517         2      830        11
    ##  2      533         4      850        20
    ##  3      542         2      923        33
    ##  4      544        -1     1004       -18
    ##  5      554        -6      812       -25
    ##  6      554        -4      740        12
    ##  7      555        -5      913        19
    ##  8      557        -3      709       -14
    ##  9      557        -3      838        -8
    ## 10      558        -2      753         8
    ## # ... with 336,766 more rows

``` r
flights %>%
  select(matches("^(dep|arr)_(time|delay)$"))
```

    ## # A tibble: 336,776 x 4
    ##    dep_time dep_delay arr_time arr_delay
    ##       <int>     <dbl>    <int>     <dbl>
    ##  1      517         2      830        11
    ##  2      533         4      850        20
    ##  3      542         2      923        33
    ##  4      544        -1     1004       -18
    ##  5      554        -6      812       -25
    ##  6      554        -4      740        12
    ##  7      555        -5      913        19
    ##  8      557        -3      709       -14
    ##  9      557        -3      838        -8
    ## 10      558        -2      753         8
    ## # ... with 336,766 more rows

##2

``` r
flights %>%
  select(air_time, air_time, dep_delay)
```

    ## # A tibble: 336,776 x 2
    ##    air_time dep_delay
    ##       <dbl>     <dbl>
    ##  1      227         2
    ##  2      227         4
    ##  3      160         2
    ##  4      183        -1
    ##  5      116        -6
    ##  6      150        -4
    ##  7      158        -5
    ##  8       53        -3
    ##  9      140        -3
    ## 10      138        -2
    ## # ... with 336,766 more rows

##3

``` r
Demora_deposito <-flights$dep_delay
m <-c("Tiempo", "transportista", "vuelos", "dep_delay", "air_time")

flights %>%
  select(any_of(m))
```

    ## # A tibble: 336,776 x 2
    ##    dep_delay air_time
    ##        <dbl>    <dbl>
    ##  1         2      227
    ##  2         4      227
    ##  3         2      160
    ##  4        -1      183
    ##  5        -6      116
    ##  6        -4      150
    ##  7        -5      158
    ##  8        -3       53
    ##  9        -3      140
    ## 10        -2      138
    ## # ... with 336,766 more rows

#Parte 4: Dplyr - mutate

##1.Las variables horario_salida y salida_programada tienen un formato
conveniente para leer, pero es difícil realizar cualquier #cálculo con
ellas porque no son realmente números continuos.Transfórmalas hacia un
formato más conveniente como número de minutos desde la medianoche.
###horario_salida

``` r
transmute(
  flights,
  dep_time,
  hour = dep_time %/% 100,
  minute = dep_time %% 100,
  newhours = hour * 60 + minute)
```

    ## # A tibble: 336,776 x 4
    ##    dep_time  hour minute newhours
    ##       <int> <dbl>  <dbl>    <dbl>
    ##  1      517     5     17      317
    ##  2      533     5     33      333
    ##  3      542     5     42      342
    ##  4      544     5     44      344
    ##  5      554     5     54      354
    ##  6      554     5     54      354
    ##  7      555     5     55      355
    ##  8      557     5     57      357
    ##  9      557     5     57      357
    ## 10      558     5     58      358
    ## # ... with 336,766 more rows

##2.Salida_programada

``` r
transmute(
  flights,
  dep_time,
  hour = sched_dep_time %/% 100,
  minute = sched_dep_time %% 100,
  newhours = hour * 60 + minute
)
```

    ## # A tibble: 336,776 x 4
    ##    dep_time  hour minute newhours
    ##       <int> <dbl>  <dbl>    <dbl>
    ##  1      517     5     15      315
    ##  2      533     5     29      329
    ##  3      542     5     40      340
    ##  4      544     5     45      345
    ##  5      554     6      0      360
    ##  6      554     5     58      358
    ##  7      555     6      0      360
    ##  8      557     6      0      360
    ##  9      557     6      0      360
    ## 10      558     6      0      360
    ## # ... with 336,766 more rows

##Compara tiempo_vuelo con horario_llegada - horario_salida. ¿Qué
esperas ver? ¿Qué ves? ¿Qué necesitas hacer para arreglarlo?
tiempo_vuelo con horario_llegada - horario_salida

``` r
# air_time == (arr_time - dep_time)
```

#arr_time - dep_time

``` r
diferencia <-transmute(
  flights,
  diferencia = arr_time - dep_time,)
```

#obtengo la columna de air_time

``` r
diferencia2 <-transmute(
  flights,
  air_time ,)
```

#luego comparamos

``` r
#diferencia2 <= diferencia
#diferencia2 > diferencia
#iferencia2 != diferencia
#diferencia2 >= diferencia
```

#Compara horario_salida, salida_programada, y atraso_salida. ¿Cómo
esperarías que esos tres números estén relacionados? ##horario_salida

``` r
horario_salida<-transmute(
  flights,
  dep_time ,
)
```

##salida_programada

``` r
salida_programada<-transmute(
  flights,
  sched_dep_time ,
)
```

#atraso_salida

``` r
atraso_salida<-transmute(
  flights,
  dep_delay ,
)
```

#comparamos

``` r
## air_time - sched_dep_time == dep_delay
```

#Encuentra los 10 vuelos más retrasados utilizando una función de
ordenamiento. ¿Cómo quieres manejar los empates? Lee atentamente la
documentación de min_rank().

``` r
retraso_de_llegada<-head(arrange(flights,desc(arr_delay)),10)
retraso_de_llegada
```

    ## # A tibble: 10 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     9      641            900      1301     1242           1530
    ##  2  2013     6    15     1432           1935      1137     1607           2120
    ##  3  2013     1    10     1121           1635      1126     1239           1810
    ##  4  2013     9    20     1139           1845      1014     1457           2210
    ##  5  2013     7    22      845           1600      1005     1044           1815
    ##  6  2013     4    10     1100           1900       960     1342           2211
    ##  7  2013     3    17     2321            810       911      135           1020
    ##  8  2013     7    22     2257            759       898      121           1026
    ##  9  2013    12     5      756           1700       896     1058           2020
    ## 10  2013     5     3     1133           2055       878     1250           2215
    ## # ... with 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
vuelos_Retrasados<-retraso_de_llegada$arr_delay
vuelos_Retrasados
```

    ##  [1] 1272 1127 1109 1007  989  931  915  895  878  875

``` r
min_rank(vuelos_Retrasados)
```

    ##  [1] 10  9  8  7  6  5  4  3  2  1

#¿Qué devuelve 1:3 + 1:10? ¿Por qué? ##Nos devuelve que la longitud del
objeto más largo(1:10) no ##es un múltiplo de la longitud del objeto más
corto(1:3) #¿Qué funciones trigonométricas proporciona R? #Nos brinda
Seno en R, Coseno en R, Tangente en R y Cotangente en R:

``` r
sin(pi/2)
```

    ## [1] 1

``` r
cos(pi)
```

    ## [1] -1

``` r
tan(pi/2)
```

    ## [1] 1.633124e+16

``` r
2/tan(pi)
```

    ## [1] -1.633124e+16

``` r
tan(pi)
```

    ## [1] -1.224647e-16

``` r
tan(2*pi)
```

    ## [1] -2.449294e-16

``` r
a=asin(0)
sin(a)
```

    ## [1] 0
