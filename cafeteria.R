library(tidyr)
library(dplyr)
library(stringr)
library(readr)


# CAFETERIA ---------------------------------------------------------------


# CAMBIO DE DIRECTORIO DE TRABAJO -----------------------------------------
getwd()
setwd("C:/Users/ivan_/Desktop/NEOLAND/BootCamp 25-03-11/5 - R/MIOS/Proyectos")


# IMPORT DE CSV -----------------------------------------------------------
ventas <- read_csv("Cafeteria/data/dirty_cafe_sales.csv")

ventas_dirty <- ventas
# VISUALIZAR LA TABLA -----------------------------------------------------
View(ventas_dirty)

View(ventas)
show(ventas)

show(ventas$"Total Spent")

ventas %>% 
  select("Total Spent") %>% 
  View()


# BORRADO COLUMNAS NO NECESITAMOS -----------------------------------------
ventas <- ventas %>% 
  select(-"Payment Method")

ventas <- ventas %>% 
  select(-"Location")


# VALORES DE LAS COLUMNAS -------------------------------------------------
ventas %>% distinct(`Transaction ID`)
ventas %>% distinct(Item)
ventas %>% distinct(Quantity)
ventas %>% distinct(`Price Per Unit`)
ventas %>% distinct(`Total Spent`)
ventas %>% distinct(`Transaction Date`)

ventas %>%
  group_by(`Price Per Unit`) %>% 
  select (Item) %>%
  summarise(Item)%>% 
  View()

#Valores unicos
ventas %>% 
  distinct(`Price Per Unit`, Item) %>% 
  filter(!`Price Per Unit` %in% c("ERROR", NA , "UNKNOWN")&
        !Item %in% c("ERROR", NA , "UNKNOWN")) %>% 
  View()


# LIMPIEZA DE DATA --------------------------------------------------------

#FUNCION QUE ELIMINA VALORES ERROR,UNKNOWN Y NULL CALCULANDO SU VALOR CON EL RESTO DE CAMPOS
limpiar_valores <- function(item1, item2, item3, operador) {
  operador_fn <- match.fun(operador)
  
  item1_sym <- rlang::sym(item1)
  item2_sym <- rlang::sym(item2)
  item3_sym <- rlang::sym(item3)
  
  resultado <- ventas %>%
    mutate(
      !!item1_sym := case_when(
        (is.na(as.numeric(as.character(!!item1_sym))) | !!item1_sym == "ERROR" | !!item1_sym == "UNKNOWN") &
          !(is.na(as.numeric(as.character(!!item2_sym))) | !!item2_sym == "ERROR" | !!item2_sym == "UNKNOWN" |
              is.na(as.numeric(as.character(!!item3_sym))) | !!item3_sym == "ERROR" | !!item3_sym == "UNKNOWN") ~ 
          operador_fn(as.numeric(as.character(!!item2_sym)), as.numeric(as.character(!!item3_sym))),
        TRUE ~ as.numeric(as.character(!!item1_sym))
      )
    )

  return(resultado)
}

ventas <- limpiar_valores("Price Per Unit", "Total Spent", "Quantity", "/")
View(ventas)

sum(is.na(ventas$`Price Per Unit`))

show(ventas$`Transaction Date`)



# ELIMINAR FILAS CON NA -----------------------------
ventas <- ventas %>%
  filter(
    !is.na(Quantity) &
    !is.na(`Price Per Unit`) &
    !is.na(`Total Spent`) &
    !is.na(Item)
  )

#Eliminamos ERROR y UNKOWN en la columna ITEM
ventas %>% 
  filter(
    Item=="ERROR" | 
    Item=="UNKNOWN"
  ) %>% 
  nrow()

ventas <- ventas %>% 
  filter(
    !Item=="ERROR" |
    !Item=="UNKNOWN"
  )

# BAJAMOS UNA FECHA LOS VALORES NULOS --------------------------------------
# Reemplazar "ERROR", "UNKNOWN" y NA por el valor de arriba
ventas <- ventas %>%
  mutate(
    `Transaction Date` = na_if(`Transaction Date`, "ERROR"),
    `Transaction Date` = na_if(`Transaction Date`, "UNKNOWN")
  ) %>%
  fill(`Transaction Date`, .direction = "down")


# CAMBIAMOS EL FORMATO DE LA FECHA AÑADIENDO DIAS -------------------------
ventas <-ventas %>%
  mutate(
    `Transaction Date` = as.Date(`Transaction Date`, format = "%Y-%m-%d"),
    `Día Semana` = weekdays(`Transaction Date`)
  )


# EXPORTAR A CSV ----------------------------------------------------------
write.csv(ventas, "C:/Users/ivan_/Desktop/NEOLAND/BootCamp 25-03-11/5 - R/MIOS/Proyectos/Cafeteria/data/clear_cafe_sales.csv", row.names = FALSE)


