# se recomienda dividir el archivo de codigo en secciones
# para separarlo el atajo de teclas en rstudio es ctrl+shift+r

# COMENTARIOS -------------------------------------------------------------


# respecto de los jugadores,
# te podes clonar el repo de worldcup.
# luego podes hacer referencia mas facil a los archivos bajados
# ejemplo: 
# https://github.com/jas1/world-cup/blob/master/r/01_data_clean.r
# en este repo me clone a world cup , agregue una carpeta R , y me puse a parsear todo. 
# puede que haga cosas "medias locas" , pero luego de que te acostumbras no es tan loco =). 


# importaciones de librerias ----------------------------------------------
# se recomienda hacer importaciones de librerias en 1 solo lado

library(dplyr)


# FUNCIONES ---------------------------------------------------------------


# si replicas el mismo codigo varias veces >> es una funcion
# primero pones un nombre cualquiera a la funcion y definis: 

# paso 1: 
# felicidades tenes tu primera funcion que hace nada :p 
nombre_cualquiera <- function(){}
# invocacion a funcion que no hace nada.
nombre_cualquiera()

# paso 2: una funcion que haga algo
funcion_que_hace_algo <- function(){
    print("hola, soy una funcion que hace algo")
}
funcion_que_hace_algo()

# paso 3: le ponemos un nombre acorde a lo que hace
imprimir_hola <- function(){
    print("hola, vengo a  imprimir HOLA :D")
}
imprimir_hola()

# paso 4: funcion que recibe parametros
imprimir_hola_parametro_1 <- function(parametro_1){
    print(paste0("hola, vengo a  imprimir lo que me pasaste de parametro: ",parametro_1))
}
imprimir_hola_parametro_1("hola soy palabras que van de parametro 1")

# paso 5: funcion de enserio:
# pensar que recibe: parametro texto
# pensar que queres que devuelva: quiero que devuelva un texto
hola_funcion_de_enserio <- function(parametro_recibo_texto){
    devolucion_texto <- paste0("Hola, este es el parametro que recibo: ", parametro_recibo_texto)
    # al poner la variable al final de la variable esto devuelve por " default " en R 
    devolucion_texto
}
hola_funcion_de_enserio("hola soy palabras que van de parametro 1")


# FUNCIONES PARA EL CASO MUNDIALES ----------------------------------------

# paso 6: funcion de enserio en caso aplicado de los jugadores: 
# pensar que recibe: recibe una URL de donde voy a extraer los datos.
# pensar que queres que devuelva: quiero que devuelva un dataframe
funcion_jugadores <- function(url_lectura,parametro_mundial){
    
    #equipo a leer
    # esto lee el equipo desde la URL con encoding UTF-8,
    # leyendo con " delimitadores " pero fijate que no lo especificaste.
    # por lo tanto no separa nada, solo lee una columna que se llama X........ por default 
    # url_lectura="https://raw.githubusercontent.com/openfootball/world-cup/master/1930--uruguay/squads/ar-argentina.txt"
    equipo <- read.delim(url_lectura, encoding = "UTF-8")
    
    # ojo aca estas " hardcodeando" == " poniendo a mano el valor " ; 
    # el de "Uruguay 1930", te conviene pasarlo como parametro, para que puedas usar otros mundiales
    # lo dejo comentado para que puedas ver que estaba y que esta ahora.
    #equipo %>% mutate(mundial = "Uruguay 1930") %>% 
    resultado_df <- equipo %>% mutate(mundial = parametro_mundial ) %>% 
        slice(3:nrow(equipo)) %>%  
        separate(1, into = c("nombre","equipo"), sep= "##") %>% 
        separate(nombre, sep= 8, into =c("posicion", "nombre")) %>% 
        # agregada transformacionde posicion: pongo trim y replace en 1 solo paso
        # side both es por default, por eso no lo especifico.
        mutate(posicion=str_trim(str_replace(posicion, "-",""))) %>% 
        # solo trim de nombre
        mutate(nombre=str_trim(nombre)) %>% 
        # agregada transformacion de equipo: trim y replace en 1 solo paso.
        mutate(equipo=str_trim(str_replace(equipo, "-,", "")))
    # ex transformacionde posicion    
    # team_uru30$posicion <- str_replace(team_uru30$posicion, "-","") %>% 
    #     str_trim(side ="both")
    # team_uru30$nombre <- str_trim(team_uru30$nombre, side="both")
    # team_uru30$equipo <- str_replace(team_uru30$equipo, "-,", "") %>% 
    #     str_trim(side="both")
    # 
    
    # devuelvo el dataframe resultante
    resultado_df
}
#invocacion a la funcion 
# pasandole como parametro la URL donde voy a levantar los datos
# pasandole como otro parametro el texto que quiero que figure en la columna mundial
uru_1930 <- funcion_jugadores(url_lectura="https://raw.githubusercontent.com/openfootball/world-cup/master/1930--uruguay/squads/ar-argentina.txt",
                  parametro_mundial="Uruguay 1930")



# funciones para todos ----------------------------------------------------
# ahora que tengo la funcion falta extraer para el resto de lo smundiales 
# si tengo el listado de donde extraer, 
# luego aplico la funcion con los parametros muchas veces
# hay varias formas de hacer esto, te pongo la que mas me parece
# definir paremtros 

# parametro URL: 
URLS <- c("https://raw.githubusercontent.com/openfootball/world-cup/master/1930--uruguay/squads/ar-argentina.txt",
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
          "https://raw.githubusercontent.com/openfootball/world-cup/master/2010--south-africa/squads/ar-argentina.txt",
          "https://raw.githubusercontent.com/openfootball/world-cup/master/2014--brazil/squads/ar-argentina.txt")

# paremetro
descripciones <- c("Uruguay 1930",
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
                   "Sudafrica 2010",
                   "Brasil 2014")
                   #"Rusia 2018")# 2018 n ol opongo porque es un caso especial




# ahora la magia: 
# dados los parametros URLS y descripciones, 
# aplica la funcion recibiendo eso.
# para eso  usas el paquete purrr, 
# usando la funcion pmap , tambien conocido como parameter map

# en .l recibe una lista de parametros

# la lista de parametros debe tener la misma cantidad
#length(URLS)
#length(descripciones)

# en.f recibe la funcion a la cual le pasaas sus parametros.
todos_mundiales <- purrr::pmap(.l=list(url_lectura=URLS,
                                       parametro_mundial=descripciones),
                               .f=funcion_jugadores)


#te quedo una lista de dataframes, 
# ahora como lo unis todo en 1 ?

# agregando rusia 2018 ----------------------------------------------------
team_rus18 <- read.csv("teamrus18.csv", encoding = "UTF-8") #datos de wikipedia
team_rus18 <- team_rus18 %>% 
    mutate(mundial = "Rusia 2018") %>% 
    select(posicion =1, nombre = 3,equipo = 6, 7) %>% 
    mutate(posicion = as.character(posicion)) %>% 
    mutate(equipo = str_trim(equipo)) %>% 
    mutate(nombre = str_trim(nombre))

# unir DF de todos --------------------------------------------------------
# a la lista de dataframes, 
# se la pasas al metodo " reduce " 
# y al reduce, le decis a traves de que metodo queres reducir.
# en nuestro caso es " union de todos ", 
# porque queres unir todos los dataframes con todos los valores sin sacar nada

todos_mundiales_df <- todos_mundiales %>% 
    reduce(union_all) %>%  # todos
    union_all(team_rus18) # mas rusia aparte

# aca tenes todos juntos
todos_mundiales_df


# parte de transformacion  ------------------------------------------------

jugadores <- todos_mundiales_df %>% 
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


# parte de plotear --------------------------------------------------------

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




