library(tidyverse)
library(spData)
library(sf)
        
leyes <- read_csv("/Users/demian/Downloads/ILGA_State_Sponsored_Homophobia_2019_dataset.csv")
cambios <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-19/cambios.csv")

leyes <- leyes %>% 
    mutate(Legal = recode(Legal, "DE FACTO" = "NO",
                          "Y" = "YES", "N"="NO"),
           COUNTRY = recode(COUNTRY, "DRC" = "Democratic Republic of the Congo",
                            "Russia" = "Russian Federation",
                            "North Korea" = "Dem. Rep. Korea",
                            "Czechia" = "Czech Republic",
                            "Eswatini" = "eSwatini",
                            "East Timor" = "Timor-Leste",
                            "Gambia" = "The Gambia",
                            "Congo" = "Republic of the Congo",
                            "Palestine (1)" = "Palestine",
                            "North Macedonia" = "Macedonia",
                            "Laos" = "Lao PDR",
                            "Indonesia (2)" = "Indonesia",
                            "Dominica" = "Dominican Republic",
                            ))
           
#cargar los shapes del mundo
mundo <- st_as_sf(world, crs = 4326)

#unir
leyes_mundo <- leyes %>% 
    left_join(world, by = c("COUNTRY" = "name_long")) %>% 
    st_as_sf(crs = 4326)
#grafico
ggplot()+
    geom_sf(data = leyes_mundo, aes(fill = Legal))+
    labs(title = "Países del mundo donde se criminalizas la relaciones\nsexuales entre personas adultas del mismo sexo",
         subtitle = "Año 2019", caption = "Datos de www.ilga.org")+
    scale_fill_discrete(name = "Criminaliza", labels = c("Si", "No"))+
    theme_minimal()
