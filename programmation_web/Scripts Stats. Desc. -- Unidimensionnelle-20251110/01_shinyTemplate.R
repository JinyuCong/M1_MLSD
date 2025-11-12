library(shiny)

# Contenu de l'interface
ui <- fluidPage("Hello MLSD students")

# Commandes à executer
server <- function(input, output){}

# Association interface & commandes
shinyApp(ui = ui, server = server)
