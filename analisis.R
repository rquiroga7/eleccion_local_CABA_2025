# Load necessary libraries
library(jsonlite)
library(ggplot2)
library(readxl)
library(tibble)
library(dplyr)
library(tidyr)

# Define the folder containing JSON files
json_folder <- "json"

# Get a list of all JSON files in the folder
json_files <- list.files(json_folder, pattern = "\\.json$", full.names = TRUE)

# Initialize an empty data frame to store all data
all_data <- data.frame()

# Loop through each JSON file and read its content
for (file in json_files) {
  # Read JSON file
  data <- tryCatch(fromJSON(file), error = function(e) NULL)
  
  if (!is.null(data) && "resultados" %in% names(data)) {
    resultados <- data$resultados
    
    # Ensure 'resultados' is a data frame and contains required columns
    if (is.data.frame(resultados) && all(c("cant_votos", "descripcion_candidatura") %in% colnames(resultados))) {
      # Extract 'descripcion_ubicacion' from the main data
      barrio <- if ("descripcion_ubicacion" %in% names(data)) data$descripcion_ubicacion else NA
      #save the text after CABA. and before the next dot into "comuna" 
      comuna <- if ("id_ubicacion" %in% names(data)) {
        id_ubicacion <- data$id_ubicacion
        # Extract the text after "CABA." and before the next dot
        comuna <- sub(".*CABA\\.([^\\.]+).*", "\\1", id_ubicacion)
      } else {
        NA
      }
      # Add 'descripcion_ubicacion' as a new column to 'resultados'
      resultados <- resultados[, c("descripcion_candidatura", "cant_votos")]
      resultados$BARRIO <- barrio
      resultados$COMUNA <- comuna
      
      # Append to all_data
      all_data <- rbind(all_data, resultados)
    } else {
      message(paste("File", file, "does not contain the required structure in 'resultados'. Skipping."))
    }
  } else {
    message(paste("File", file, "does not contain 'resultados'. Skipping."))
  }
}


View(all_data)

#Add coalicion column using coaliciones.csv
coaliciones <- read.csv("coaliciones.csv", sep = ",", encoding = "UTF-8", header = TRUE)
#Strip beginning and end spaces from descripcion_candidatura
coaliciones$descripcion_candidatura <- trimws(coaliciones$descripcion_candidatura)

#agregar la columna coalicion a all_data
all_data <- merge(all_data, coaliciones, by = "descripcion_candidatura", all.x = TRUE)

# Summarize votes by party
party_results <- aggregate(cant_votos ~ descripcion_candidatura, data = all_data, sum)

# Plot the results
ggplot(party_results, aes(x = reorder(descripcion_candidatura, -cant_votos), y = cant_votos, fill = descripcion_candidatura)) +
  geom_bar(stat = "identity") +
  labs(title = "Election Results by Party", x = "Lista", y = "cant_votos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Now Summarize votes by location and descripcion_candidatura
location_party_results <- aggregate(cant_votos ~ barrio + descripcion_candidatura, data = all_data, sum)

# Plot the results
ggplot(location_party_results, aes(x = reorder(barrio, -cant_votos), y = cant_votos, fill = descripcion_candidatura)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Election Results by Location and Party", x = "Location", y = "cant_votos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Add year =2025 to the data
all_data$year <- 2025

#Por barrio y año, calcular una nueva fila con el total de votos, descripcion_candidatura = "TOTALES", agregar a all_data
all_data_totales <- all_data %>%
  group_by(BARRIO, COMUNA, year) %>%
  summarise(COALICION = "Otros", cant_votos = sum(cant_votos), descripcion_candidatura = "TOTALES") %>%
  ungroup()
#Ahora, por barrio y por año, calcular una nueva fila con el total de votos POSITIVOS, descripcion_candidatura = "POSITIVOS", agregar a all_data
all_data_positivos <- all_data %>%
  group_by(BARRIO, COMUNA, year) %>%
  filter(COALICION != "Otros") %>%
  summarise(COALICION = "Otros", cant_votos = sum(cant_votos), descripcion_candidatura = "POSITIVOS") %>%
  ungroup()

  # Append the totals to all_data
all_data <- rbind(all_data, all_data_totales, all_data_positivos)
View(all_data)

#Cargar archivo XLSX con informacion de barrio y circuito electoral para armar un diccionario
circuitos_data <- read_excel("circuitos-electorales.xlsx")

circuitos_data <- as_tibble(circuitos_data) %>%
  select(CIRCUITO_N, BARRIO)
# create a dictionary for circuitos
circuitos_dict <- circuitos_data %>%
  group_by(CIRCUITO_N) %>%
  summarise(BARRIO = paste(unique(BARRIO), collapse = ", ")) %>%
  ungroup()

#Now load 2021 results from datos_2021/generales_dl_circuitos_nativos_2021.csv
# Read the CSV file
data_2021 <- read.csv("datos_2021/generales_dl_circuitos_nativos_2021.csv", sep = ";", encoding = "UTF-8")

data_2021 <- data_2021 %>%
  mutate(year = 2021)

#Columns are ﻿COMUNA;CIRCUITO;Autodeterminación y Libertad;Juntos por el Cambio;Frente de Todos;Frente de Izquierda y los Trabajadores;La libertad avanza;EN BLANCO;NULOS;RECURRIDOS;IMPUGNADOS;TOTALES
# long to wide

data_2021_wide <- data_2021 %>%
  pivot_longer(cols = -c(COMUNA, CIRCUITO, year), names_to = "descripcion_candidatura", values_to = "cant_votos") %>%
  mutate(cant_votos = as.numeric(cant_votos))

coaliciones_2021 <- read.csv("coaliciones_2021.csv", sep = ",", encoding = "UTF-8", header = TRUE)
#Strip beginning and end spaces from descripcion_candidatura
coaliciones_2021$descripcion_candidatura <- trimws(coaliciones_2021$descripcion_candidatura)

# Merge with circuitos_dict to get barrio information
data_2021_wide <- data_2021_wide %>%
  left_join(circuitos_dict, by = c("CIRCUITO" = "CIRCUITO_N")) %>%
  left_join(coaliciones_2021, by = c("descripcion_candidatura" = "descripcion_candidatura")) %>%
  select(-CIRCUITO) %>%
  select(descripcion_candidatura, cant_votos, BARRIO, COMUNA, COALICION, year)

#Uniformize column names for both dataframes
colnames(data_2021_wide) <- c("descripcion_candidatura", "cant_votos", "barrio", COMUNA, "year", "barrio")

#Ahora, por barrio y por año, calcular una nueva fila con el total de votos POSITIVOS, descripcion_candidatura = "POSITIVOS", agregar a all_data
data_2021_wide_positivos <- data_2021_wide %>%
  group_by(BARRIO, COMUNA, year) %>%
  filter(COALICION != "Otros") %>%
  summarise(COALICION = "Otros", cant_votos = sum(cant_votos), descripcion_candidatura = "POSITIVOS") %>%
  ungroup()

data_2021_wide <- rbind(data_2021_wide, data_2021_wide_positivos)

View(all_data)
View(data_2021_wide)

# Combine the two data frames
combined_data <- rbind(all_data, data_2021_wide)

# Save the combined data to a JSON file
write_json(combined_data, "combined_data.json", pretty = TRUE, auto_unbox = TRUE)

#Primero, crear nuevas filas para la sumatoria de votos por barrio y año, con descripcion_candidatura = "JxC+LLA" y COALICION = "JxC+LLA"
combined_data_new <- combined_data %>%
  group_by(BARRIO, COMUNA, year) %>%
  summarise(cant_votos = sum(cant_votos[COALICION %in% c("JxC", "LLA")]), 
            descripcion_candidatura = "JxC+LLA", 
            COALICION = "JxC+LLA") %>%
  ungroup()

combined_data <- rbind(combined_data, combined_data_new)

View(combined_data)

#For descripcion_candidatura = "TOTALES", change COALICION to "TOTALES". Do the same for POSITIVOS
combined_data$COALICION[combined_data$descripcion_candidatura == "TOTALES"] <- "TOTALES"
combined_data$COALICION[combined_data$descripcion_candidatura == "POSITIVOS"] <- "POSITIVOS"

#For combined_data, make a new long dataframe where columns are "BARRIO", "COMUNA", "year", and where "cant_votos" is split into three columns, one for COALICION %in% c("JxC", "LLA", "FdT" and "JxC+LLA"), one for COALICION == "TOTALES", and one for COALICION == "POSITIVOS" and another for COALICION == "TOTALES"
combined_comuna_data_wide <- combined_data %>%
  filter(COALICION %in% c("JxC", "LLA", "FdT", "JxC+LLA", "TOTALES", "POSITIVOS")) %>%
  # First group and summarize to ensure one row per combination
  group_by(COMUNA, year, COALICION) %>%
  summarise(cant_votos = sum(cant_votos, na.rm = TRUE), .groups = "drop") %>%
  # Then complete any missing combinations
  group_by(COMUNA, year) %>%
  complete(COALICION = c("JxC", "LLA", "FdT", "JxC+LLA", "TOTALES", "POSITIVOS")) %>%
  ungroup() %>%
  # Then do the pivot operations
  pivot_wider(
    id_cols = c(COMUNA, year),
    names_from = COALICION,
    values_from = cant_votos
  ) %>%
  pivot_longer(
    cols = c("JxC", "LLA", "FdT", "JxC+LLA"),
    names_to = "COALICION",
    values_to = "cant_votos"
  ) %>%
  mutate(cant_votos = replace_na(cant_votos, 0))

combined_barrio_data_wide <- combined_data %>%
  filter(COALICION %in% c("JxC", "LLA", "FdT", "JxC+LLA", "TOTALES", "POSITIVOS")) %>%
  # First group and summarize to ensure one row per combination
  group_by(COMUNA, BARRIO, year, COALICION) %>%
  summarise(cant_votos = sum(cant_votos, na.rm = TRUE), .groups = "drop") %>%
  # Then complete any missing combinations
  group_by(COMUNA, BARRIO, year) %>%
  complete(COALICION = c("JxC", "LLA", "FdT", "JxC+LLA", "TOTALES", "POSITIVOS")) %>%
  ungroup() %>%
  # Then do the pivot operations
  pivot_wider(
    id_cols = c(COMUNA, BARRIO, year),
    names_from = COALICION,
    values_from = cant_votos
  ) %>%
  pivot_longer(
    cols = c("JxC", "LLA", "FdT", "JxC+LLA"),
    names_to = "COALICION",
    values_to = "cant_votos"
  ) %>%
  mutate(cant_votos = replace_na(cant_votos, 0))




# Define coalition colors
coalition_colors <- c(
  "FdT" = "#55BBFF",    # Light blue
  "LLA" = "#9B4F96",    # Purple
  "JxC" = "#FFD700",    # Gold
  "JxC+LLA" = "#FF8C00" # Orange
)

# Create the improved faceted scatterplot
# Create the improved faceted scatterplot
ggplot(combined_comuna_data_wide, aes(x = TOTALES, y = cant_votos, fill = COALICION)) +
  # Draw points on top
  geom_point(aes(shape = factor(year)), 
             size = 4, 
             stroke = 1.5, 
             color = "black") +
  # Draw paths with arrows first
  geom_path(aes(group = COMUNA), 
            arrow = arrow(length = unit(0.08, "cm"), 
                         type = "closed",
                         ends = "last"), # Change to point at end (2025)
            color = "black",
            alpha = 0.6, 
            linewidth = 0.8) +
  facet_wrap(~COALICION, scales = "fixed") +
  scale_fill_manual(values = coalition_colors) +
  scale_shape_manual(values = c(21, 24)) +  # Using fillable shapes
  labs(
    title = "Evolución del voto por coalición en barrios de CABA (2021-2025)",
    subtitle = "Las flechas conectan resultados del mismo barrio entre años",
    x = "Total de votantes",
    y = "Votos obtenidos",
    shape = "Año",
    fill = "Coalición"  # Changed from color to fill to match aesthetic
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = NA),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  )

library(ggrepel)

ggplot(combined_comuna_data_wide, aes(x = TOTALES, y = cant_votos, fill = COALICION)) +
  # Draw points
  geom_point(aes(shape = factor(year)), 
             size = 4, 
             stroke = 1.5, 
             color = "black") +
  # Draw paths with arrows first
  geom_path(aes(group = COMUNA), 
            arrow = arrow(length = unit(0.08, "cm"), 
                         type = "closed",
                         ends = "last"),
            color = "black",
            alpha = 0.6, 
            linewidth = 0.8) +
  # Add labels for 2025 points
  geom_text_repel(
    data = . %>% filter(year == 2025),
    aes(label = COMUNA),
    size = 5,
    box.padding = 0.5,
    point.padding = 0.5,
    force = 2,
    segment.color = "grey50"
  ) +
  facet_wrap(~COALICION, scales = "free_y") +
  scale_fill_manual(values = coalition_colors) +
  scale_shape_manual(values = c(21, 24)) +
  labs(
    title = "Evolución del voto por coalición en barrios de CABA (2021-2025)",
    subtitle = "Las flechas conectan resultados del mismo barrio entre años",
    x = "Total de votantes",
    y = "Votos obtenidos",
    shape = "Año",
    fill = "Coalición"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = NA),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  )


# Calculate the differences for both TOTALES and cant_votos
combined_comuna_data_diff <- combined_comuna_data_wide %>%
  group_by(COMUNA, COALICION) %>%
  summarise(
    votos_diff = cant_votos[year == 2025] - cant_votos[year == 2021],
    positivos_diff = POSITIVOS[year == 2025] - POSITIVOS[year == 2021],
    .groups = "drop"
  )

# Find the maximum absolute value for both axes to set symmetric limits
max_limit <- max(abs(c(
  combined_comuna_data_diff$votos_diff,
  combined_comuna_data_diff$positivos_diff
)))

# Create the faceted scatterplot
# Calculate linear models first
trend_lines <- combined_comuna_data_diff %>%
  group_by(COALICION) %>%
  summarise(
    model = list(lm(votos_diff ~ positivos_diff)),
    intercept = coef(model[[1]])[1],
    slope = coef(model[[1]])[2],
    .groups = "drop"
  )

# Create the faceted scatterplot
ggplot(combined_comuna_data_diff, 
       aes(x = positivos_diff, y = votos_diff)) +
  # Add trend lines first
  geom_abline(data = trend_lines,
              aes(slope = slope, intercept = intercept),
              color = "red",
              linetype = "dotted",
              size = 0.8) +
  geom_point(aes(fill = COALICION),
             shape = 21,
             size = 4,
             stroke = 1.5) +
  facet_wrap(~COALICION, scales = "fixed") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_fill_manual(values = coalition_colors, guide = "none") +
  coord_fixed(ratio = 1, xlim = c(-max_limit, max_limit), ylim = c(-max_limit, max_limit)) +
  labs(
    title = "Cambios en votos por coalición vs cambios en total de votantes",
    subtitle = "Por comuna, diferencias 2025-2021",
    x = "Diferencia en total de votos positivos",
    y = "Diferencia en votos de la coalición"
  ) +
  theme_light()+
    theme(
    strip.text = element_text(size = 16, face = "bold",color = "black"),  # Increased size and added bold
    strip.background = element_rect(fill = "grey95", color = "black"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  )