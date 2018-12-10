
library(ggplot2)
library(tidyverse)
library(fmsb)
library(rsconnect)
library(shiny)
library(stringr)
library(shinythemes)
pokemon <- read.csv("Pokemon.csv")
ui <- fluidPage(theme = shinytheme("flatly"),
  #titlePanel("Pokemon Dataset"),
  navbarPage("Pokemon Dataset Visualizations:",
  tabPanel("Distribution of Base Stats by Generation",
           sidebarPanel(
             selectInput("generation_hist", "Generation:", 
                         c("Gen 1" = 1, 
                           "Gen 2" = 2,
                           "Gen 3" = 3,
                           "Gen 4" = 4,
                           "Gen 5" = 5,
                           "Gen 6" = 6)),
             selectInput("stat_hist", "Base Stat:",
                         c(
                           "HP",
                           "Attack",
                           "Defense",
                           "Special Attack" = "Sp..Atk",
                           "Special Defense" = "Sp..Def",
                           "Speed",
                           "Total"
                           ))
           ),
           mainPanel(plotOutput("stat_histogram"))
           ),
  tabPanel("Type Averages of Base Stats by Generation",
           sidebarPanel(
             selectInput("generation_bar", "Generation:", 
                         c("Gen 1" = 1, 
                           "Gen 2" = 2,
                           "Gen 3" = 3,
                           "Gen 4" = 4,
                           "Gen 5" = 5,
                           "Gen 6" = 6)),
             selectInput("stat_bar", "Base Stat:",
                         c(
                           "HP",
                           "Attack",
                           "Defense",
                           "Special Attack" = "Sp..Atk",
                           "Special Defense" = "Sp..Def",
                           "Speed",
                           "Total"
                         ))
           ),
           mainPanel(plotOutput("stat_bargraph"))  
          ),
  tabPanel("Spider Graph for Specific Pokemon Base Stats",
           sidebarPanel(
             #textInput("name", "Enter Pokemon Name", "")
             selectizeInput("name", "Enter Pokemon Name:", choices = pokemon$Name)
           ),
           mainPanel(plotOutput("spiderGraph"))
           ),
  tabPanel("Top 10 Pokemon by Base Stat and Type", 
           sidebarPanel(
             selectInput("type_chart", "Type:", 
                         c(
                           "Bug",
                           "Dark",
                           "Dragon",
                           "Electric",
                           "Fairy",
                           "Fighting",
                           "Fire",
                           "Ghost",
                           "Grass",
                           "Ground",
                           "Ice",
                           "Normal",
                           "Poison",
                           "Psychic",
                           "Rock",
                           "Steel",
                           "Water"
                         )),
             selectInput("stat_chart", "Base Stat:",
                         c(
                           "HP",
                           "Attack",
                           "Defense",
                           "Special Attack" = "Sp..Atk",
                           "Special Defense" = "Sp..Def",
                           "Speed",
                           "Total"
                         ))
           ),
           mainPanel(textOutput("typeTitle") ,tableOutput("typeTable"))
           )
  )
)
  
server <- function(input, output) {
  
  #Graph for the distribution of stats by gen
  output$stat_histogram <- renderPlot({
    ggplot(subset(pokemon, Generation == input$generation_hist)) + 
      geom_histogram(aes_string(input$stat_hist, fill = "Type.1")) +
      labs(title = str_c(c("Distribution of ", input$stat_hist, " for Gen ", input$generation_hist), collapse = ""))
  })
  
  #Graph for the averages of stats by type for given gen
  output$stat_bargraph <- renderPlot({
    subset(pokemon, Generation == input$generation_bar) %>% 
      group_by(Type.1) %>% 
      summarise(avg = mean(eval(as.name(input$stat_bar)))) %>%
      ggplot() +
      geom_bar(aes(x = Type.1, y = avg, fill = Type.1), stat = "sum") +
      coord_flip() +
      labs(x = "Type",
           y = str_c(c("Average ", input$stat_bar), collapse = "" ),
           title = str_c(c("Type Averages for ", input$stat_bar, " for Gen ", input$generation_bar), collapse = "")
           )
  })
  
  #Spider chart for the selected pokemon
  output$spiderGraph <- renderPlot({
    if (!(input$name %in% pokemon$Name)) {
      stop("Please enter a valid Pokemon name")
    }
    max <- 150
    min <- 0
    pokemon_data <- pokemon %>% filter(Name == input$name) %>% select(HP, Attack, Defense, Sp..Atk, 
                                                                      Sp..Def, Speed)
    pokemon_data <- rbind(rep(max,6) , rep(min,6) , pokemon_data)
    radarchart(pokemon_data,
               title = str_c(c("Base Stats for ", input$name),collapse = ""),
               cglcol="grey",
               cglty=1,
               cglwd=.9,
               pcol=rgb(.8,.1,.1,.9),
               pfcol=rgb(.8,.1,.1,.5),
               plwd=5
    )
    
  })
  
  #Title for the top 10 table
  output$typeTitle <- renderText({
    str_c(c("Top 10 ", input$type_chart, " Type Pokemon for ", input$stat_chart, ":"), collapse = "")
  })
  
  #Table for the top 10 for stat by given type
  output$typeTable <- renderTable({
    filter(pokemon, Type.1 == input$type_chart) %>%
      arrange(desc(eval(as.name(input$stat_chart)))) %>%
      head(10)
  })
  
  waterPokemon <- pokemon %>%
    filter(Type.1 == "Water")
  avgHP <- 
  avgAttack<- mean(waterPokemon$Attack)
  avgDefense <- mean(waterPokemon$Defense)
  avgSPAtk <- mean(waterPokemon$Sp..Atk)
  avgSPDef <- mean(waterPokemon$Sp..Def)
  avgSpeed <- mean(waterPokemon$Speed)
  
  hpCI <- c(mean(waterPokemon$HP) - 2 * sd(waterPokemon$HP), mean(waterPokemon$HP) + 2 * sd(waterPokemon$HP))
  atkCI <- c(mean(waterPokemon$Attack) - 2 * sd(waterPokemon$Attack), mean(waterPokemon$Attack) + 2 * sd(waterPokemon$Attack))
  defCI <- c(mean(waterPokemon$Defense) - 2 * sd(waterPokemon$Defense), mean(waterPokemon$Defense) + 2 * sd(waterPokemon$Defense))
  spAtkCI <- c(mean(waterPokemon$Sp..Atk) - 2 * sd(waterPokemon$Sp..Atk), mean(waterPokemon$Sp..Atk) + 2 * sd(waterPokemon$Sp..Atk))
  spDefCI <- c(mean(waterPokemon$Sp..Def) - 2 * sd(waterPokemon$Sp..Def), mean(waterPokemon$Sp..Def) + 2 * sd(waterPokemon$Sp..Def))
}
shinyApp(ui, server)