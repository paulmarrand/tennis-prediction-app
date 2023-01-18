get_iso_country <- function(r){
  
  #set URL
  url_country <- "https://www.iban.com/country-codes"
  country <- read_html(url_country)
  
  #Get countries
  countries <- 
    country |>
    html_elements("td:nth-child(1)") |>
    html_text2()
  
  #Get iso code
  iso_code <- 
    country |>
    html_elements("td:nth-child(3)") |>
    html_text2()
  
  #Gather in a tibble
  code_country <- tibble(countries, iso_code)
  
  return(code_country)
}