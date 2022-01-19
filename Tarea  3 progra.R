install.packages("tidyverse")
library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
flights
?flights
#9.1 Parte 1: Dplyr - filter
##1. Tuvieron un retraso de llegada de dos o mas horas
flights %>% 
  filter(arr_time>=120)

##2.Volaron a Houston (IAH oHOU)
vuelos01 <-flights %>%
  filter(dest=="IAH" | dest=="HOU")
vuelos01     
data.frame(vuelos01)

##3.Fueron operados por United, American o Delta
flights %>%
  filter(carrier=="UA" | carrier=="AA" | carrier=="DL")

##4.Partieron en invierno del hemisferio sur (julio, agosto y septiembre)
vuelos02 <-flights %>% filter(month == 7 | month == 8 | month == 9)

vuelos02

##5.Llegaron mÃ¡s de dos horas tarde, pero no salieron tarde

flights %>% 
  filter(arr_time>=120, dep_delay<=0)

##6.Se retrasaron por lo menos una hora, pero repusieron mÃ¡s de 30 minutos en vuelo

flights %>%
  filter(dep_delay - arr_delay>=30, arr_delay>=60)

##7.Partieron entre la medianoche y las 6 a.m. (incluyente)

flights %>%
  filter(dep_time==2400 | dep_time<=600)

#2.Otra funcion de dplyr que es util para usar filtros es between(). ¿Que hace? ¿Puedes usarla para simplificar el codigo necesario para responder a los desafios anteriores?

#Esta funcion nos permite filtrar filas que se encuentren dentro de un limite min y max que establece between
lim_inf <-7 #julio
lim_sup <-9 #septiemnbre
x <-8
between(lim_inf,lim_sup,x)

#3.¿Cuantos vuelos tienen datos faltantes en horario_salida? ¿Que otras variables tienen valores faltantes? ¿Que representan estas filas?

Datos_Na <-flights$sched_dep_time

is.na(Datos_Na)  
sum(is.na(Datos_Na))

summary(flights)
#Las filas faltantes dan a entiender valores que debieron haberse registrado, pero no lo fueron. R almacena los valores faltantes como NA, lo que significa Not Available.
is.na(flights$dep_delay)
is.na(flights$air_time)

#9.2 Parte 2: Dplyr - arrange
#¿Cómo podrías usar arrange() para ordenar todos los valores faltantes al comienzo? (Sugerencia: usa is.na()).
names(flights)[colSums(is.na(flights)) >0]

flights %>% 
  arrange(desc(is.na(dep_time)),
          desc(is.na(dep_delay)),
          desc(is.na(arr_time)), 
          desc(is.na(arr_delay)),
          desc(is.na(tailnum)),
          desc(is.na(air_time)))
#Ordena vuelos para encontrar los vuelos más retrasados. Encuentra los vuelos que salieron más temprano.
##vuelos mas retrasadados. vuelos que salieron mas temprano
flights %>% 
  arrange(dep_delay)

retraso <-flights %>% 
  arrange(desc(dep_delay)) 

head(retraso,5)

#Ordena vuelos para encontrar los vuelos más rápidos (que viajaron a mayor velocidad).
velocidad<-mutate(
  flights,
  speed = distance / (air_time * 60)
)
arrange(velocidad, desc(speed))
head(velocidad,5)
#¿Cuáles vuelos viajaron más lejos? ¿Cuál viajó más cerca?
##mas lejos 
arrange(flights, desc(distance))
flights %>% 
  summarise(max_distance =max(distance))
##mas cerca
arrange(flights, distance)
flights %>% 
  summarise(min_distance =min(distance))


flights$distance
distancia <-flights %>%
  arrange(desc(distance))
head(distancia,5)
tail(distancia,5)

view(tail(distancia,5))

##3.Parte 3: Dplyr - select

##1.Haz una lluvia de ideas sobre tantas maneras como sea posible para seleccionar dep_time, dep_delay, arr_time, and arr_delay de flights.


flights %>%
  select(dep_time, dep_delay, arr_time, arr_delay)

flights %>%
  select(starts_with("dep"), starts_with("arr"))

flights %>%
  select("dep_time", "dep_delay", "arr_time", "arr_delay")

flights %>%
  select(matches("^(dep|arr)_(time|delay)$"))

##2. Â¿QuÃ© sucede si incluyes el nombre de una variable varias veces en una llamada a select()?

flights %>%
  select(air_time, air_time, dep_delay)

##3.Â¿QuÃ© hace la funciÃ³n any_of()? Â¡Â¿Por quÃ© podria ser Ãºtil en conjunto con este vector?
Demora_deposito <-flights$dep_delay
m <-c("Tiempo", "transportista", "vuelos", "dep_delay", "air_time")

flights %>%
  select(any_of(m))
#9.4 Parte 4: Dplyr - mutate
#Las variables horario_salida y salida_programada tienen un formato conveniente para leer, pero es difícil realizar cualquier 
#cálculo con ellas porque no son realmente números continuos.Transfórmalas hacia un formato más conveniente como número de minutos desde la medianoche.
###horario_salida
transmute(
  flights,
  dep_time,
  hour = dep_time %/% 100,
  minute = dep_time %% 100,
  newhours = hour * 60 + minute
)
###salida_programada
transmute(
  flights,
  dep_time,
  hour = sched_dep_time %/% 100,
  minute = sched_dep_time %% 100,
  newhours = hour * 60 + minute
)
#Compara tiempo_vuelo con horario_llegada - horario_salida. ¿Qué esperas ver? ¿Qué ves? ¿Qué necesitas hacer para arreglarlo?
##tiempo_vuelo con horario_llegada - horario_salida
air_time == (arr_time - dep_time)
#arr_time - dep_time
diferencia <-transmute(
  flights,
  diferencia = arr_time - dep_time,
)
#obtengo la columna de air_time
diferencia2 <-transmute(
  flights,
  air_time ,
)
#luego comparamos 
diferencia2 == diferencia
diferencia2 <= diferencia
diferencia2 > diferencia
diferencia2 != diferencia
diferencia2 >= diferencia
#Compara horario_salida, salida_programada, y atraso_salida. ¿Cómo esperarías que esos tres números estén relacionados?
##horario_salida                     
horario_salida<-transmute(
  flights,
  dep_time ,
)
##salida_programada
salida_programada<-transmute(
  flights,
  sched_dep_time ,
)
#atraso_salida
atraso_salida<-transmute(
  flights,
  dep_delay ,
)
#comparamos
horario_salida - salida_programada = atraso_salida
#Encuentra los 10 vuelos más retrasados utilizando una función de ordenamiento. ¿Cómo quieres manejar los empates? Lee atentamente la documentación de min_rank().
retraso_de_llegada<-head(arrange(flights,desc(arr_delay)),10)
retraso_de_llegada
vuelos_Retrasados<-retraso_de_llegada$arr_delay
vuelos_Retrasados
min_rank(vuelos_Retrasados)
#¿Qué devuelve 1:3 + 1:10? ¿Por qué?
##Nos devuelve que la longitud del objeto más largo(1:10) no 
##es un múltiplo de la longitud del objeto más corto(1:3)
#¿Qué funciones trigonométricas proporciona R?
#Nos brinda Seno en R, Coseno en R, Tangente en R y Cotangente en R:
sin(pi/2)
cos(pi)
tan(pi/2)
2/tan(pi)
tan(pi)
tan(2*pi)
a=asin(0)
sin(a)



