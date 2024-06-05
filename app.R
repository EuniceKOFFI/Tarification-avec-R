################## Chargement des packages et données #########################
# Chargement des packages
source("setup_packages.R")

# Charger les données
data("freMTPLfreq", package = "CASdatasets")
data("freMTPLsev", package = "CASdatasets")

# Description des données 
freMTPLfreq_desc <- c(
  "The policy ID (used to link with the claims dataset)",
  "Number of claims during the exposure period.",
  "The period of exposure for a policy, in years.",
  "The power of the car (ordered categorical).",
  "The vehicle age, in years.",
  "The driver age, in years (in France, people can drive a car at 18).",
  "The car brand divided in the following groups: A- Renaut Nissan and Citroen, B- Volkswagen, Audi, Skoda and Seat, C- Opel, General Motors and Ford, D- Fiat, E- Mercedes Chrysler and BMW, F- Japanese (except Nissan) and Korean, G- other.",
  "The car gas, Diesel or regular.",
  "The policy region in France (based on the 1970-2015 classification).",
  "The density of inhabitants (number of inhabitants per km2) in the city the driver of the car lives in."
)

freMTPLsev_desc <- c(
  "The occurence date (used to link with the contract dataset).",
  "The cost of the claim, seen as at a recent date."
)

# Création des dataframes pour les tableaux
freq_data_desc <- data.frame(
  Column = names(freMTPLfreq),
  Description = freMTPLfreq_desc[1:length(names(freMTPLfreq))]
)

sev_data_desc <- data.frame(
  Column = names(freMTPLsev),
  Description = freMTPLsev_desc[1:length(names(freMTPLsev))]
)

# Summary 
generate_data_summary <- function(data) {
  # Calculer les statistiques de base
  desc <- sapply(data, function(x) {
    c(
      Type = class(x)[1],
      "Non-missing Values" = sum(!is.na(x)),
      "Missing Values" = sum(is.na(x))
    )
  })
  
  # Convertir en dataframe et ajouter les noms de colonnes comme une colonne
  desc_df <- as.data.frame(t(desc))
  desc_df$Variable <- rownames(desc_df)
  rownames(desc_df) <- NULL
  
  # Retourner le dataframe
  return(desc_df)
}

# Statistiques descriptives
describe_data <- function(df) {
  # Filtrer pour exclure les colonnes de type facteur
  numeric_df <- df %>%
    select_if(~ !is.factor(.))
  
  # Calculer les statistiques pour les colonnes numériques
  description_df <- numeric_df %>%
    summarise(across(everything(), list(
      mean = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      q1 = ~quantile(., 0.25, na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      q3 = ~quantile(., 0.75, na.rm = TRUE),
      max = ~max(., na.rm = TRUE)
    )))
  
  # Pivoter pour organiser les variables en lignes et les indicateurs en colonnes
  description_pivot <- description_df %>%
    pivot_longer(
      cols = everything(),
      names_to = c(".value", "statistic"),  # Sépare les noms de variables des indicateurs
      names_sep = "_"
    )
  
  return(description_pivot)
}



# UI setup
ui <- fluidPage(
  includeCSS("styles.css"),
  titlePanel(div("Tarification en assurance dommage", class = "yellow-box")),
  
  tabsetPanel(
    tabPanel("Présentation des données", 
             h2("Présentation des données", class = "centered-title"),
             fluidPage(
               mainPanel(
                 
                 h3(" 1. Description des variables"),
                 h4("freMTPLfreq : Données sur les contrats sinistrés"),
                 dataTableOutput("tableFreq"),
                 br(),
                 h4("freMTPLsev : Montants des sinistres"),
                 dataTableOutput("tableSev"),
                 br(),
                 
                 h3(" 2. Caractéristiques des variables"),
                 h4("freMTPLfreq : Données sur les contrats sinistrés"),
                 tableOutput("tableFreq1"),
                 br(),
                 h4("freMTPLsev : Montants des sinistres"),
                 tableOutput("tableSev1"),
                 br()

               ),
               
               div(class="red-box",
                   strong("Bilan de l'étude :"),
                   tags$ul(
                     tags$li("Nous avons deux bases de données."),
                     tags$li("Nous n'avons pas de valeurs manquantes."),
                     tags$li("Toutes les variables sont au bon format à l'exception de la variable PolicyID de la deuxième table."),
                     tags$li("Dans la section suivante nous allons analyser les données au travers de graphiques et de stats.")
                   )
               )
             )
    ),
    
    tabPanel("Visualisation des données", 
             h2("Visualisation des données", class = "centered-title"),
             fluidPage(
               h3(" 1. Statistiques descriptives"),
               h4("freMTPLfreq : Données sur les contrats sinistrés"),
               DTOutput("tableFreq2"),
               br(),
               h4("freMTPLsev : Montants des sinistres"),
               DTOutput("tableSev2"),
               br(),
               
               h3(" 2. Analyse univariée"),
               h4("2.1. Variables quantitatives")
               
          
             )),
    
    tabPanel("Modélisation",
             h2("Modélisation", class = "centered-title"),
             fluidPage(
               # Contenu pour Modélisation
               h3("Modèle de Prédiction"),

             )),
    
    tabPanel("Résultats et prévisions",
             h2("Résultats et prévisions", class = "centered-title"),
             fluidPage(
               # Contenu pour Résultats et prévisions
               h3("Visualisation des Prévisions"),

             ))
  )
)


# Server setup
server <- function(input, output) {
  
  #### Onglet 1 : Présentation des données ####
  # Description des données 
  output$tableFreq <- renderDataTable({
    DT::datatable(freq_data_desc, options = list(pageLength = 5))
  })
  output$tableSev <- renderDataTable({
    DT::datatable(sev_data_desc, options = list(pageLength = 5))
  })
  
  # Statistiques
  summaryFreq <- reactive({ generate_data_summary(freMTPLfreq) })
  summarySev <- reactive({ generate_data_summary(freMTPLsev) })
  
  output$tableFreq1 <- renderTable({summaryFreq()})
  output$tableSev1 <- renderTable({summarySev()})

  
  #### Onglet 2 : Visualisation des données ####
  # Statistiques descriptives
  output$tableFreq2 <- renderDT({describe_data(freMTPLfreq)})
  output$tableSev2 <- renderDT({describe_data(freMTPLsev)})
  
  # Ajouter des éléments similaires pour les autres onglets...
}

# Run the application 
shinyApp(ui = ui, server = server)
