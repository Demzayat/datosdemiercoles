library(tidyverse)
library(gganimate)

# Carga de los datos de gapminder -----------------------------------------

gapminder <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-24/gapminder_es.csv")

options(scipen=999) #elimina anotación científica


# Grafico animado ---------------------------------------------------------

p <- ggplot(data = gapminder, aes(y=esperanza_de_vida, x = pib_per_capita))+
  geom_point(aes(size = poblacion, colour = continente), alpha = 0.8)+
  labs(x= "PBI per capita", y = "Esperanza de vida", 
       title = "Evolución de la esperanza de vida y el PBI per cápita por país")+
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  theme_bw()+
  theme(#panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
        
  
p <- p + facet_wrap(~continente, nrow = 1)+
  transition_states(anio, transition_length = 1, state_length = 1)+
  labs(subtitle = "Año: {closest_state}")
 # shadow_mark(alpha = 0.3, size = 0.5)


# guardando con resolucion wide -------------------------------------------

anim_save(filename = "gapminder.gif",animation = animate(p, height = 500, width= 1100))

