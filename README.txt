DESCRIPTION : 

This a directory that gather all the code necessary to run a shiny app 
designed to display odds about a tennis match. 
Here is the purpose of every file that you can find in the directory:

- app.R : main file that contain the code to run the app
- get_iso_country.R : R function used to webscrapp informations about countries and their iso3 notation.
- get_iso_players_data.R : R function used to webscrapp informations about the TOP 2000 ATP single players.
- tennis_clean.rds : cleaned dataset used for the modelisation.
- fit_tuned_LR_workflow.rds : Logistic Regression Workflow fitted to the data. Works with tidymodels.
- workflow_sum_up.png : Informations about the workflow used to fit the data.   

HOW TO USE IT ? 

- After downloading the zip archive, create a directory with it.

- Create a new R script

- Run the following commands : 

	pacman::p_load(shiny)
	runApp(<path to directory>)

- Wait a minute before the shiny app opened

- Note that once you have entered all the information in the left part of the app, you have to click the
confirmation buttons on the fourth panel on the left in order to display the odds. 

WHAT CAN I PREDICT ? 

The application will give the user the odds for the best ranked player within the two to win the match.
It will to do so by using some informations that the users must enter in the app : 

	About the players : 
		-First Name
		-Second Name
		-Best hand
		-Whereas the player is a seed for this match. Normally, it's the kind of informations you can easily
		find on any tennis score websites.

	About the tournament : 
		- The country where the tournament is happening.
		- The type of surface
		- The level of the tournament : Grand Slams, Masters 1000, ATP 500, ATP 250.
	
	About the match : 
		- The round of the match.


