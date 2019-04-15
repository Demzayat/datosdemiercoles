library(tidyverse)
library(readr)
library(stringr)

# funcion para limpiar los datos -------------------------------------------------

#armado de la funcion
function_jugadores <- function(url_lectura, nombremundial){
  equipo <- read.delim(url_lectura, encoding ="UTF-8")
  formacion_df <-  equipo %>% 
    mutate(mundial = nombremundial) %>% 
    slice(3:nrow(equipo)) %>%
    separate(1, into = c("nombre","equipos"), sep= "##") %>% 
    separate(nombre, sep= 8, into =c("posicion", "nombre")) %>% 
  #  filter(!is.na(equipos)) %>% 
    mutate(equipos = str_replace(equipos," \\(.*\\)", ""),
           posicion = str_replace(str_trim(posicion), "-", "")) %>% 
    mutate(posicion = str_replace(str_trim(posicion), "\\(.*\\)",""), 
            nombre = str_trim(nombre), 
            equipos = str_replace(str_trim(equipos), ".*,", "")) 
  formacion_df
}

#para probar la funcion
  url_lectura <- "https://raw.githubusercontent.com/openfootball/world-cup/master/2006--germany/squads/ar-argentina.txt"
  function_jugadores(url_lectura, nombremundial = "Alemania 2006")

# listas de parametros --------------------------------------------------------------

urls <- c("https://raw.githubusercontent.com/openfootball/world-cup/master/1930--uruguay/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/1934--italy/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/1962--chile/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/1966--england/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/1974--west-germany/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/1978--argentina/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/1982--spain/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/1986--mexico/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/1990--italy/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/1990--italy/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/1998--france/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/2002--south-korea-n-japan/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/2006--germany/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/2010--south-africa/squads/ar-argentina.txt")

nombre_mundial <- c("Uruguay 1930",
                    "Italia 1934",
                    "Chile 1962",
                    "Inglaterra 1966",
                    "Alemania 1974",
                    "Argentina 1978",
                    "España 1982",
                    "Mexico 1986",
                    "Italia 1990",
                    "Estados Unidos 1994",
                    "Francia 1998",
                    "Japon Korea 2002",
                    "Alemania 2006",
                    "Sudafrica 2010")
                    
# casos especiales --------------------------------------------------------
team_bra14 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/2014--brazil/squads/ar-argentina.txt", encoding = "UTF-8")
team_rus18 <- read.csv("teamrus18.csv", encoding = "UTF-8") #datos de wikipedia

##15 Brasil 2014
team_bra14 <- team_bra14 %>% 
  mutate(mundial = "Brasil 2014") %>% 
  separate(1, into = c("nombre","equipos"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre")) %>% 
  filter(!is.na(equipos)) %>% 
  mutate(posicion = str_replace(str_trim(posicion), "\\(.*\\)", ""),
         nombre = str_trim(nombre),
         equipos = str_replace (str_trim(equipos),".*,", "")) %>% 
  mutate(equipos = str_replace(equipos, " \\(.*\\)", ""))

##16 Rusia 2018
team_rus18 <- team_rus18 %>% 
  mutate(mundial = "Rusia 2018") %>% 
  select(posicion =1, 
         nombre = 3,
         equipos = 6, 7)
team_rus18$posicion <- as.character(team_rus18$posicion)


# corriendo la funcion ------------------------------------------------------------

todos_mundiales <- pmap(.l=list(url_lectura=urls,
                                nombremundial=nombre_mundial),
                        .f=function_jugadores)


# uniendo todo ------------------------------------------------------------
todos_mundiales_df <- todos_mundiales %>% 
  reduce(union_all)

todos_mundiales_df <- todos_mundiales_df %>% # todos
  bind_rows(team_bra14, team_rus18) %>% 
  filter (!is.na(equipos)) %>% 
  filter(equipos != "") %>% 
  mutate(equipos = recode(equipos,
                         " Estudiantes"= " Estudiantes de La Plata")) %>% 
  mutate(equipos = str_trim(equipos))


# Graficos ----------------------------------------------------------------

xequipo <- todos_mundiales_df %>% 
  group_by(equipos) %>% 
  summarise(cant = n()) %>% 
  arrange(desc(cant)) 

xjugador <- todos_mundiales_df %>% 
  group_by(nombre) %>% 
  summarise(cant = n()) %>% 
  arrange(desc(cant))

ggplot(data = head(xequipo,20), aes(x=reorder(equipos, cant), y = cant))+
  geom_col(fill = "steelblue")+
  geom_text(aes(y = cant+1, label = cant))+
  labs(subtitle = "Jugadores en la Selección Argentina por Equipos", x="", y="")+
  coord_flip()+
  theme_bw()

ggplot(data = head(xjugador,22), aes(x=reorder(nombre, cant), y = cant))+
  geom_col(fill = "orange")+
  geom_text(aes(y = cant+0.1, label = cant))+
  labs(subtitle = "Jugadores que más mundiales jugaron para la Selección Argentina", x="", y="")+
  coord_flip()+
  theme_bw()
  