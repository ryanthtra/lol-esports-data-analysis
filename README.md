# lol-esports-data-analysis
Analysis, insights, and modeling for various League of Legends eSports leagues for the 2018 Spring Split -- a Springboard Intro to Data Science capstone project

### Introduction:
League of Legends (LoL) is a multiplayer online battle arena videogame by Riot Games.  The primary game mode for LoL is Summoner's Rift (which is also the name of the map, or arena), in which two teams of five players vie to destroy their opponents' base, called the Nexus.  Each player chooses one of 140 unique avatars, called champions, to represent them, and each champion has its own unique blend of attributes, skills, and abilities.

For more detailed information about the game, please see <a href="https://en.wikipedia.org/wiki/League_of_Legends#Gameplay" target="_blank">League of Legends on Wikipedia.</a>


## What is the problem you want to solve?

Just like the data analysis used by the Oakland A's as mentioned in the book Moneyball to find the most important baseball statistics contributing to runs scored/allowed and wins/losses, I will be using data analysis to find confirm or deny the most important statistics in League of Legends that contributes to an eSports team's success in its league.

I am testing the two hypotheses that in League of Legends professional eSports leagues, kill count differential is the most significant predictor for wins and losses, and crowd control and vision are the most significant predictors for kill count differential.

Kill count differential is the number of champion kills one team achieved minus the number of champion kills the opposing team achieved.  

Crowd control, in terms of League of Legends, is the ability to hinder the mobility or abilities of opposing champions.  Examples of such hinderances includes slows (reduced movement speed), stuns (inability to move, attack, or use abilities), and silences (inability to use abilities).

Vision is placing wards, i.e., short-range 360-degree cameras, on the map, which assist in the helping a team know where their opponents are located in order to protect themselves from possible flanking maneuvers or to create awareness of their opponents' activities.

Additionally, I will be providing other statistical analyses obtained from the match data.

## Who is your client and why do they care about this problem? In other words, what will your client DO or DECIDE based on your analysis that they wouldn’t have otherwise?

The League of Legends community at large, could be interested in this problem.  Professional teams can confirm that the best strategies to use in their leagues are picking champions with high crowd control abilities.  Regular players (i.e., non-professionals) become more aware of the importance of placing wards all over the map.  If the hypothesis is not proven true, then other features of the data can show their importance to success.


## What data are you going to use for this? How will you acquire this data?

All of the data to used for this project will come from the <a href="https://acs.leagueoflegends.com">https://acs.leagueoflegends.com</a> domain.  The match IDs used to access individual professional eSports matches come from <a href="https://www.lolesports.com">https://www.lolesports.com</a>.  Each individual match contains a Region ID, a Match ID, and a Game Hash, all of which are used to call a Web API for obtaining the match data.

httr will be used to make the web API calls, and jsonlite will be used to convert the JSON responses of these API calls into R objects.


## In brief, outline your approach to solving this problem (knowing that this might change later).

My approach to solving this problem is by obtaining all the data for the matches for the 2018 Spring Split of each of the five easily-accessible leagues: the North America League of Legends Championship Series (NALCS), the Europe League of Legends Championship Series (EULCS), League of Legends Champions Korea (LCK), LoL Pro League (LPL; in China), and the League of Legends Master Series (LMS; Taiwan, Hong Kong, and Macau).

Each individual match will have its data wrangled into its desired observation format, which will then be concatenated to a cumulative data frame.  Further data wrangling will be used in order to calculate certain statistics, such as totals, ratios, percentages, and probabilities.