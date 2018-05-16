library(shiny)
library(tidyverse)


match_up <- function(flavors){
        
        results <- list()
        
        for(i in 1:length(flavors)){
                
                results[[i]] <- bible %>% 
                        filter(main == flavors[i]) %>% 
                        pull(pairing)
        }
        
        reduce(results, intersect)
}

bible <- read.csv("flavor_bible_full.csv")
names(bible) <- c("main","pairing")

ui <- fluidPage(
        titlePanel("Flavor Bible Suggestions"),
        sidebarLayout(
                sidebarPanel(selectInput("flavorInput", 
                                         "Input Flavor(s)",
                                         choices = levels(bible$main),
                                         selected = "ACHIOTE SEEDS",
                                         multiple = TRUE
                                         )
                             ),
                mainPanel(tableOutput("results"))
        )
)
server <- function(input, output) {
        
        output$results <- renderTable({
                if (all(input$flavorInput == "")) {
                "Matches shown here."
                } else {
                filtered <- match_up(input$flavorInput)
                filtered
                }
        })
}

# options(shiny.port = 139)
# 
# options(shiny.host = "192.168.0.4")

shinyApp(ui = ui, server = server)