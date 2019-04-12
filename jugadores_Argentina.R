library(tidyverse)
library(stringr)

## Descargando los jugadores convocados para cada mundial
team_uru30 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/1930--uruguay/squads/ar-argentina.txt", encoding = "UTF-8")
team_ita34 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/1934--italy/squads/ar-argentina.txt", encoding = "UTF-8")
team_chi62 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/1962--chile/squads/ar-argentina.txt", encoding = "UTF-8")
team_ing66 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/1966--england/squads/ar-argentina.txt", encoding = "UTF-8")
team_ale74 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/1974--west-germany/squads/ar-argentina.txt", encoding = "UTF-8")
team_arg78 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/1978--argentina/squads/ar-argentina.txt", encoding = "UTF-8")
team_esp82 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/1982--spain/squads/ar-argentina.txt", encoding = "UTF-8")
team_mex86 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/1986--mexico/squads/ar-argentina.txt", encoding = "UTF-8")
team_ita90 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/1990--italy/squads/ar-argentina.txt", encoding = "UTF-8")
team_usa94 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/1990--italy/squads/ar-argentina.txt", encoding = "UTF-8")
team_fra98 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/1998--france/squads/ar-argentina.txt", encoding = "UTF-8")
team_kor02 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/2002--south-korea-n-japan/squads/ar-argentina.txt", encoding = "UTF-8")
team_ale06 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/2006--germany/squads/ar-argentina.txt", encoding = "UTF-8")
team_suda10 <-  read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/2010--south-africa/squads/ar-argentina.txt", encoding = "UTF-8")
team_bra14 <- read.delim("https://raw.githubusercontent.com/openfootball/world-cup/master/2014--brazil/squads/ar-argentina.txt", encoding = "UTF-8")

team_rus18 <- read.csv("teamrus18.csv", encoding = "UTF-8") #datos de wikipedia

# Ordenando los datos y agregando dato del mundial

##1 Uruguay 30
team_uru30 <- team_uru30 %>% 
  mutate(mundial = "Uruguay 1930") %>% 
  slice(3:nrow(team_uru30)) %>%  
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre"))
team_uru30$posicion <- str_replace(team_uru30$posicion, "-","") %>% 
  str_trim(side ="both")
team_uru30$nombre <- str_trim(team_uru30$nombre, side="both")
team_uru30$equipo <- str_replace(team_uru30$equipo, "-,", "") %>% 
  str_trim(side="both")

##2 Italia 34
team_ita34 <- team_ita34 %>% 
  mutate(mundial = "Italia 1934") %>% 
  slice(3:nrow(team_ita34)) %>%  
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre"))
team_ita34$posicion <- str_replace(team_ita34$posicion, "-","") %>% 
  str_trim(side ="both")
team_ita34$nombre <- str_trim(team_ita34$nombre, side="both")
team_ita34$equipo <- str_replace(team_ita34$equipo, "-,", "") %>% 
  str_trim(side="both")

##3 Chile 62
team_chi62 <- team_chi62 %>% 
  mutate(mundial = "Chile 1962") %>% 
  slice(3:nrow(team_chi62)) %>%  
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre"))
team_chi62$posicion <- str_replace(team_chi62$posicion, "-","") %>% 
  str_trim(side ="both")
team_chi62$nombre <- str_trim(team_chi62$nombre, side="both")
team_chi62$equipo <- str_replace(team_chi62$equipo, "-,", "") %>% 
  str_trim(side="both")

##4 Inglaterra 66
team_ing66 <- team_ing66 %>% 
  mutate(mundial = "Inglaterra 1966") %>% 
  slice(3:nrow(team_ing66)) %>%  
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre"))
team_ing66$posicion <- str_replace(team_ing66$posicion, "-","") %>% 
  str_trim(side ="both")
team_ing66$nombre <- str_trim(team_ing66$nombre, side="both")
team_ing66$equipo <- str_replace(team_ing66$equipo, "-,", "") %>% 
  str_trim(side="both")

##5 Alemania 74
team_ale74 <- team_ale74 %>% 
  mutate(mundial = "Alemania 1974") %>% 
  slice(3:nrow(team_ale74)) %>%  
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre"))
team_ale74$posicion <- str_replace(team_ale74$posicion, "-","") %>% 
  str_trim(side ="both")
team_ale74$nombre <- str_trim(team_ale74$nombre, side="both")
team_ale74$equipo <- str_replace(team_ale74$equipo, "-,", "") %>% 
  str_trim(side="both")

##6 Argentina 78
team_arg78 <- team_arg78 %>% 
  mutate(mundial = "Argentina 1978") %>% 
  slice(3:nrow(team_arg78)) %>%  
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre"))
team_arg78$posicion <- str_replace(team_arg78$posicion, "-","") %>% 
  str_trim(side ="both")
team_arg78$nombre <- str_trim(team_arg78$nombre, side="both")
team_arg78$equipo <- str_replace(team_arg78$equipo, "-,", "") %>% 
  str_trim(side="both")

##7 Espana 82
team_esp82 <- team_esp82 %>% 
  mutate(mundial = "España 1982") %>% 
  slice(3:nrow(team_esp82)) %>%  
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre"))
team_esp82$posicion <- str_replace(team_esp82$posicion, "-","") %>% 
  str_trim(side ="both")
team_esp82$nombre <- str_trim(team_esp82$nombre, side="both")
team_esp82$equipo <- str_replace(team_esp82$equipo, "-,", "") %>% 
  str_trim(side="both")

##8 Mexico 86
team_mex86 <- team_mex86 %>% 
  mutate(mundial = "Mexico 1986") %>% 
  slice(3:nrow(team_mex86)) %>%  
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre"))
team_mex86$posicion <- str_replace(team_mex86$posicion, "-","") %>% 
  str_trim(side ="both")
team_mex86$nombre <- str_trim(team_mex86$nombre, side="both")
team_mex86$equipo <- str_replace(team_mex86$equipo, "-,", "") %>% 
  str_trim(side="both")

##9 Italia 90
team_ita90 <- team_ita90 %>% 
  mutate(mundial = "Italia 1990") %>% 
  slice(3:nrow(team_ita90)) %>%
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre")) %>% 
  filter(!is.na(equipo))
team_ita90$posicion <- str_replace(team_ita90$posicion, "-","") %>% 
  str_trim(side ="both")
team_ita90$nombre <- str_trim(team_ita90$nombre, side="both")
team_ita90$equipo <- str_replace(team_ita90$equipo, "-,", "") %>% 
  str_trim(side="both")

##10 EEUU 94
team_usa94 <- team_usa94 %>% 
  mutate(mundial = "Estados Unidos 1994") %>% 
  slice(3:nrow(team_usa94)) %>%
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre")) %>% 
  filter(!is.na(equipo))
team_usa94$posicion <- str_replace(team_usa94$posicion, "-","") %>% 
  str_trim(side ="both")
team_usa94$nombre <- str_trim(team_usa94$nombre, side="both")
team_usa94$equipo <- str_replace(team_usa94$equipo, "-,", "") %>% 
  str_trim(side="both")

##11 Francia 98
team_fra98 <- team_fra98 %>% 
  mutate(mundial = "Francia 1998") %>% 
  slice(3:nrow(team_fra98)) %>%
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre")) %>% 
  filter(!is.na(equipo))
team_fra98$posicion <- str_replace(team_fra98$posicion, "-","") %>% 
  str_trim(side ="both")
team_fra98$nombre <- str_trim(team_fra98$nombre, side="both")
team_fra98$equipo <- str_replace(team_fra98$equipo, "-,", "") %>% 
  str_trim(side="both")

##12 Korea 02
team_kor02 <- team_kor02 %>% 
  mutate(mundial = "Japon Korea 2002") %>% 
  slice(3:nrow(team_kor02)) %>%
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre")) %>% 
  filter(!is.na(equipo))
team_kor02$posicion <- str_replace(team_kor02$posicion, "-","") %>% 
  str_trim(side ="both")
team_kor02$nombre <- str_trim(team_kor02$nombre, side="both")
team_kor02$equipo <- str_replace(team_kor02$equipo, ".*,", "") %>% 
  str_trim(side="both")

##13 Alemania 06
team_ale06 <- team_ale06 %>% 
  mutate(mundial = "Alemania 2006") %>% 
  slice(3:nrow(team_ale06)) %>%
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre")) %>% 
  filter(!is.na(equipo))
team_ale06$posicion <- str_replace(team_ale06$posicion, "-","") %>% 
  str_trim(side ="both")
team_ale06$nombre <- str_trim(team_ale06$nombre, side="both")
team_ale06$equipo <- str_replace(team_ale06$equipo, ".*,", "") %>%
  str_trim(side="both")
team_ale06$equipo <- str_replace(team_ale06$equipo," \\(.*\\)", "")

##14 Sudafrica 2010
team_suda10 <- team_suda10 %>% 
  mutate(mundial = "Sudafrica 2010") %>% 
  slice(3:nrow(team_suda10)) %>%
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre")) %>% 
  filter(!is.na(equipo))
team_suda10$posicion <- str_replace(team_suda10$posicion, "-","") %>% 
  str_trim(side ="both")
team_suda10$nombre <- str_trim(team_suda10$nombre, side="both")
team_suda10$equipo <- str_replace(team_suda10$equipo, ".*,", "") %>%
  str_trim(side="both")
team_suda10$equipo <- str_replace(team_suda10$equipo," \\(.*\\)", "")

##15 Brasil 2014
team_bra14 <- team_bra14 %>% 
  mutate(mundial = "Brasil 2014") %>% 
  slice(3:nrow(team_bra14)) %>%
  separate(1, into = c("nombre","equipo"), sep= "##") %>% 
  separate(nombre, sep= 8, into =c("posicion", "nombre")) %>% 
  filter(!is.na(equipo))
team_bra14$posicion <- str_replace(team_bra14$posicion, "-","") %>% 
  str_trim(side ="both")
team_bra14$nombre <- str_trim(team_bra14$nombre, side="both")
team_bra14$equipo <- str_replace(team_bra14$equipo, ".*,", "") %>%
  str_trim(side="both")
team_bra14$equipo <- str_replace(team_bra14$equipo," \\(.*\\)", "")

##16 Rusia 2018
team_rus18 <- team_rus18 %>% 
  mutate(mundial = "Rusia 2018") %>% 
  select(posicion =1, nombre = 3,equipo = 6, 7)
team_rus18$posicion <- as.character(team_rus18$posicion)
team_rus18$equipo <- str_trim(team_rus18$equipo, side = "both")
team_rus18$nombre <- str_trim(team_rus18$nombre, side = "both")

# 2 uniendo los datos
jugadores <- bind_rows(team_uru30, team_ita34, team_chi62, team_ing66, team_ale74, team_arg78,
                       team_esp82, team_mex86, team_ita90, team_usa94, team_fra98, team_kor02,
                       team_ale06, team_suda10, team_bra14, team_rus18)
jugadores <- jugadores %>% 
  mutate(equipo = recode(equipo,
                         "Estudiantes"= "Estudiantes de La Plata"))
xequipo <- jugadores %>% 
  group_by(equipo) %>% 
  summarise(cant = n()) %>% 
  arrange(desc(cant)) 

xjugador <- jugadores %>% 
  group_by(nombre) %>% 
  summarise(cant = n()) %>% 
  arrange(desc(cant))


ggplot(data = head(xequipo,20), aes(x=reorder(equipo, cant), y = cant))+
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
