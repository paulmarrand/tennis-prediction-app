get_players_data <- function(){
  
  #Get the current date
  current_date <- Sys.Date()
  
  #Get the last Monday date
  last_monday <- 7 * floor(as.numeric(current_date-1+4)/7) + as.Date(1-4, origin="1970-01-01")
  
  #Set URL
  url_player <- paste0("https://www.atptour.com/en/rankings/singles?rankRange=1-5000&rankDate=",last_monday)
  player_rankings <- read_html(url_player)
  
  #Get rankings
  ranking <- 
    player_rankings |>
    html_elements(".border-left-4") |>
    html_text2()
  
  #Get names
  names <- 
    player_rankings |>
    html_elements(".player-cell-wrapper") |>
    html_element("a:nth-child(1)") |>
    html_text2() 
  
  #Get ages
  ages <- 
    player_rankings |>
    html_elements(".age-cell.border-right-4") |>
    html_text2()
  
  #Get countries
  countries <- 
    player_rankings |>
    html_elements(".country-item") |>
    html_element("img") |>
    html_attr("alt")
  
  #Gather data in a tibble
  players_data <- tibble(names, country_code = countries, ages, ranking)
  
  #Clean Names
  players_data <- 
    players_data |>
    mutate(names = names |> 
             str_remove("\\r") |>
             str_remove("\\r") |>
             str_trim() |>
             str_to_upper()
           )
           
  #Clean up ages
  players_data <- 
    players_data |>
    mutate(ages = ages |>
             str_remove("\\r") |>
             str_trim()
    )
  
  #Clean up ranking
  players_data <- 
    players_data |>
    mutate(ranking = ranking |> 
             str_remove("\\r") |>
             str_remove("\\r") |>
             str_remove("T") |>
             str_trim()
    )
  #Set up the type of variables and remove missing data
  players_data <-
    players_data |>
    drop_na(ages) |>
    mutate(across(ages:ranking, as.numeric))

  return(players_data)
}