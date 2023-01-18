#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
pacman::p_load(shiny, tidymodels, tidyverse, rvest)

#Call the players information webscrapp function
source("get_players_data.R")

#Call the countries code webscrapp function
source("get_iso_country.R")

#Charge the data about teh player
player_data <- get_players_data()

#Charge the data about the country
country_data <- get_iso_country()

#Charge the prediction model
LR_workflow <- readRDS(file = "fit_tuned_LR_workflow.rds")

#Charge Data
tennis_data <- readRDS(file = "tennis_clean.rds")

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Tennis match prediction"),

    
    splitLayout(
      navlistPanel(header = "Enter Data",
        tabPanel("Player 1",
                 textInput("player1_first_name", 
                           label="Enter First Name", 
                           value = "Rafael",
                           placeholder = "Use either upper or lower case. Be careful not to type space after the last letter. Type a player that belongs to the TOP2000 ATP"
                 ),
                 textInput("player1_second_name", 
                           label="Enter Second Name",
                           value = "Nadal"
                 ),
                 radioButtons("player1_seed",
                              label = "Is the player seed ?",
                              choiceNames = c("No",
                                              "Yes"),
                              choiceValues = as.character(sort(unique(tennis_data$is_winner_seed)))
                 ),
                 radioButtons("player1_hand", 
                              label = "Select player's strong hand", 
                              choiceNames = c("Left handed",
                                              "Right handed",
                                              "Ambidextrous"),
                              choiceValues = as.character(sort(unique(tennis_data$winner_hand)))
                 ),
          ),
          tabPanel("Player 2",
                 textInput("player2_first_name", 
                           label="Enter First Name",
                           value = "Novak",
                           placeholder = "Use either upper or lower case. Be careful not to type space after the last letter. Type a player that belongs to the TOP2000 ATP"
                           
                 ),
                 textInput("player2_second_name", 
                           value = "Djokovic",
                           label="Enter Second Name"
                 ),
                 radioButtons("player2_seed",
                              label = "Is the player seed ?",
                              choiceNames = c("No",
                                              "Yes"),
                              choiceValues = as.character(sort(unique(tennis_data$is_winner_seed)))
                 ),
                 radioButtons("player2_hand", 
                              label = "Select player's strong hand", 
                              choiceNames = c("Left handed",
                                              "Right handed",
                                              "Ambidextrous"),
                              choiceValues = as.character(sort(unique(tennis_data$winner_hand)))
                 ),
          ),
          tabPanel("Tournament",
                   radioButtons("tournament_level", 
                               label = "Select the level of tournament",
                               choices = as.character(sort(unique(tennis_data$tourney_level)))
                               ),
                   selectInput("tournament_country", 
                             label = "Select the country of the tournamnent",
                             choices = country_data |> pull(countries)
                   ),
                   radioButtons("tournament_surface",
                                label = "Select tournament's surface",
                                choices = as.character(sort(unique(tennis_data$surface)))
                   ),
                   radioButtons("tournament_best_of",
                                label = "Select the maximum number of sets",
                                choices = as.character(sort(unique(tennis_data$best_of)))
                   ),
                   radioButtons("tournament_round",
                                label = "Select match's round level",
                                choiceNames = c("BR",
                                                "First round of qualification",
                                                "Second round of qualification",
                                                "Third round of qualification",
                                                "Round of 128",
                                                "Round of 64", 
                                                "Round of 32", 
                                                "ROund of 16", 
                                                "Quarter-final",
                                                "Round Robin",
                                                "Semi-final",
                                                "Final"),
                                choiceValues = as.character(sort(unique(tennis_data$round)))
                   ),
          ),
          tabPanel("Confirm",
                   actionButton("validate",
                                "Confirm your selection")
          ),
      ),
      mainPanel(
            titlePanel("Sum up : Best ranked player"),
            textOutput("best_player_name"),
            textOutput("best_player_rank"),
            textOutput("best_player_age"),
  
            titlePanel("Sum up : Worst ranked player"),
            textOutput("worst_player_name"),
            textOutput("worst_player_rank"),
            textOutput("worst_player_age"),
            titlePanel("Odds"),
            textOutput("odds_best_player"),
      ),
    )
)


#Set up the server function
server1 <- function(input, output) {
  
  #Output best player name
  output$best_player_name <- bindEvent(renderText({ifelse((as.numeric(player_data |> filter(names == 
                                                                                              paste(str_to_upper(input$player1_first_name), str_to_upper(input$player1_second_name),sep=" ")) |> pull(ranking)) 
                                                           <= 
                                                             as.numeric(player_data |> filter(names == 
                                                                                                paste(str_to_upper(input$player2_first_name), str_to_upper(input$player2_second_name),sep=" ")) |> pull(ranking)))
                                                          , 
                                                          paste(input$player1_first_name, input$player1_second_name, sep = " "),
                                                          paste(input$player2_first_name, input$player2_second_name, sep = " ")
                                                          )
                                                  }),
                                       input$validate, 
                                       ignoreInit = TRUE)
  
  #Output best player rank
  output$best_player_rank <- bindEvent(renderText({ifelse((as.numeric(player_data |> filter(names == 
                                                                                              paste(str_to_upper(input$player1_first_name), str_to_upper(input$player1_second_name),sep=" ")) |> pull(ranking)) 
                                                           <= 
                                                             as.numeric(player_data |> filter(names == 
                                                                                                paste(str_to_upper(input$player2_first_name), str_to_upper(input$player2_second_name),sep=" ")) |> pull(ranking)))
                                                          ,
                                                          paste("Rank : ", player_data |> filter(names == 
                                                                                                   paste(str_to_upper(input$player1_first_name), str_to_upper(input$player1_second_name),sep=" "))|> pull(ranking),sep = ""),
                                                          paste("Rank : ", player_data |> filter(names == 
                                                                                                   paste(str_to_upper(input$player2_first_name), str_to_upper(input$player2_second_name),sep=" "))|> pull(ranking), sep = "")
                                                          )
                                                  }),
                                       input$validate, 
                                       ignoreInit = TRUE)
  
  #Output best player age
  output$best_player_age <- bindEvent(renderText({ifelse((as.numeric(player_data |> filter(names == 
                                                                                             paste(str_to_upper(input$player1_first_name), str_to_upper(input$player1_second_name),sep=" ")) |> pull(ranking)) 
                                                          <= 
                                                            as.numeric(player_data |> filter(names == 
                                                                                               paste(str_to_upper(input$player2_first_name), str_to_upper(input$player2_second_name),sep=" ")) |> pull(ranking)))
                                                         ,
                                                          paste("Age : ", player_data |> filter(names == 
                                                                                                   paste(str_to_upper(input$player1_first_name), str_to_upper(input$player1_second_name),sep=" ")) |> pull(ages),sep = ""),
                                                          paste("Age : ", player_data |> filter(names == 
                                                                                                   paste(str_to_upper(input$player2_first_name), str_to_upper(input$player2_second_name),sep=" ")) |> pull(ages), sep = "")
                                                          )
                                                  }),
                                       input$validate, 
                                       ignoreInit = TRUE)
  
  #Output worst player name
  output$worst_player_name <- bindEvent(renderText({ifelse((as.numeric(player_data |> filter(names == 
                                                                                               paste(str_to_upper(input$player1_first_name), str_to_upper(input$player1_second_name),sep=" ")) |> pull(ranking)) 
                                                            <= 
                                                              as.numeric(player_data |> filter(names == 
                                                                                                 paste(str_to_upper(input$player2_first_name), str_to_upper(input$player2_second_name),sep=" ")) |> pull(ranking)))
                                                           , 
                                                          paste(input$player2_first_name, input$player2_second_name, sep = " "),
                                                          paste(input$player1_first_name, input$player1_second_name, sep = " ")
                                                          )
                                                  }),
                                        input$validate, 
                                        ignoreInit = TRUE)
  
  #Output worst player rank
  output$worst_player_rank <- bindEvent(renderText({ifelse((as.numeric(player_data |> filter(names == 
                                                                                               paste(str_to_upper(input$player1_first_name), str_to_upper(input$player1_second_name),sep=" ")) |> pull(ranking)) 
                                                            <= 
                                                              as.numeric(player_data |> filter(names == 
                                                                                                 paste(str_to_upper(input$player2_first_name), str_to_upper(input$player2_second_name),sep=" ")) |> pull(ranking)))
                                                           ,
                                                          paste("Rank : ", player_data |> filter(names == 
                                                                                                   paste(str_to_upper(input$player2_first_name), str_to_upper(input$player2_second_name),sep=" ")) |> pull(ranking),sep = ""),
                                                          paste("Rank : ", player_data |> filter(names == 
                                                                                                   paste(str_to_upper(input$player1_first_name), str_to_upper(input$player1_second_name),sep=" ")) |> pull(ranking), sep = "")
                                                          )
                                                  }),
                                        input$validate, 
                                        ignoreInit = TRUE)
  
  #Output worst player age
  output$worst_player_age <- bindEvent(renderText({ifelse((as.numeric(player_data |> filter(names == 
                                                                                              paste(str_to_upper(input$player1_first_name), str_to_upper(input$player1_second_name),sep=" ")) |> pull(ranking)) 
                                                           <= 
                                                             as.numeric(player_data |> filter(names == 
                                                                                                paste(str_to_upper(input$player2_first_name), str_to_upper(input$player2_second_name),sep=" ")) |> pull(ranking)))
                                                          ,
                                                         paste("Age : ", player_data |> filter(names == 
                                                                                                  paste(str_to_upper(input$player2_first_name), str_to_upper(input$player2_second_name),sep=" ")) |> pull(ages),sep = ""),
                                                         paste("Age : ", player_data |> filter(names == 
                                                                                                  paste(str_to_upper(input$player1_first_name), str_to_upper(input$player1_second_name),sep=" "))|> pull(ages), sep = "")
                                                         )
                                                  }),
                                       input$validate, 
                                       ignoreInit = TRUE)
  
  #Compute the odds of winning for the best ranked player !!
  output$odds_best_player <- bindEvent(renderText({paste("Odds for the best player to win : ",
    LR_workflow |> predict(new_data = tibble(surface =input$tournament_surface,
                                             tourney_level =input$tournament_level,
                                             best_of =  input$tournament_best_of,
                                             round =  input$tournament_round,
                                             winner_hand =  input$player1_hand,
                                             loser_hand =  input$player1_hand,
                                             winner_rank = player_data |>
                                               filter(names == paste(str_to_upper(input$player1_first_name),
                                                                     str_to_upper(input$player1_second_name),
                                                                     sep = " ")) |>
                                               pull(ranking),
                                             loser_rank = player_data |>
                                               filter(names == paste(str_to_upper(input$player2_first_name),
                                                                     str_to_upper(input$player2_second_name),
                                                                     sep = " ")) |>
                                               pull(ranking),
                                             winner_age = player_data |>
                                               filter(names == paste(str_to_upper(input$player1_first_name),
                                                                     str_to_upper(input$player1_second_name),
                                                                     sep = " ")) |>
                                               pull(ages),
                                             loser_age =
                                               player_data |>
                                               filter(names == paste(str_to_upper(input$player2_first_name),
                                                                     str_to_upper(input$player2_second_name),
                                                                     sep = " ")) |>
                                               pull(ages),
                                             is_winner_seed = input$player1_seed,
                                             is_loser_seed = input$player2_seed,
                                             is_winner_home = ifelse(((country_data |> filter(countries == input$tournament_country) |> pull(iso_code)) == (player_data |> filter(names == paste(str_to_upper(input$player1_first_name), str_to_upper(input$player1_second_name), sep = " ")) |> pull(country_code))),
                                                                     "1",
                                                                     "0"),
                                             is_loser_home = ifelse(((country_data |> filter(countries == input$tournament_country) |> pull(iso_code)) == (player_data |> filter(names == paste(str_to_upper(input$player2_first_name), str_to_upper(input$player2_second_name), sep = " ")) |> pull(country_code))),
                                                                    "1",
                                                                    "0")), type = "prob") |> pull(.pred_1) |> as.character(),
    sep = "")
  }),
  input$validate,
  ignoreInit = TRUE
  )
}

# Run the application 
shinyApp(ui = ui, server = server1)


