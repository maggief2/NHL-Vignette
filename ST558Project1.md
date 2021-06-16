National Hockey League (NHL) Vignette
================
Maggie Feng
June 20, 2021

-   [Functions to read in the data](#functions-to-read-in-the-data)
    -   [NHL records API](#nhl-records-api)
    -   [NHL stats API](#nhl-stats-api)
    -   [API wrapper](#api-wrapper)
-   [Exploratory Data Analysis](#exploratory-data-analysis)
    -   [Contingency tables](#contingency-tables)
    -   [Numerical Summaries](#numerical-summaries)
    -   [Bar plot](#bar-plot)
    -   [Histogram](#histogram)
    -   [Box plot](#box-plot)
    -   [Scatter plot](#scatter-plot)

Required packages to run this vignette:

``` r
library(httr)
library(jsonlite)
library(tidyverse)
```

# Functions to read in the data

## NHL records API

The `getRecords` function will contact the [NHL records
API](https://gitlab.com/dword4/nhlapi/-/blob/master/records-api.md),
pull data from one of the six enpoints, and output the data as a data
frame.

The first argument `input` is to specify one of the six endpoints:

-   `"franchise"` will return id, firstSeasonId, lastSeasonId, and name
    of every team in the history of the NHL
-   `"teamtotal"` will return the total stats for every franchise
-   `"season"` will drill-down into season records and can do so for a
    specific franchise
-   `"goalie"` will return goalie records for all or a specified
    franchise
-   `"skater"` will return skater records for all or a specified
    franchise
-   `"history"` will return admin history and retired numbers

The arguments `name` and `id` are used to specify which team or
franchise’s records you wish to output. Adding `name` or `id` for the
first two endpoints, `"franchise"` and `"teamtotal"`, will have the same
result of not adding the arguments at all.

``` r
#Function to contact the NHL records API
#returns well-formatted, parsed data
getRecords <- function(input = c("franchise", "teamtotal", "season", "goalie", "skater", "history"), 
                       name = NULL, id = NULL){
  ##Note: if both team name and id arguments are filled, team name overrides id number
  
  #All queries are prefixed by this base_url
  base_url <- "https://records.nhl.com/site/api"
  
  #First two endpoints: do not have name/id specification
  if (input == "franchise" | input == "teamtotal"){
    if (input == "franchise")
      endpoint <- "franchise"
    if (input == "teamtotal")
      endpoint <- "franchise-team-totals"
    full_url <- paste0(base_url, "/", endpoint)
  }
  
  #Third, fourth, and fifth endpoints: with name/id specification by franchiseId
  if (input == "season" | input == "goalie" | input == "skater"){
    if (input == "season")
      endpoint <- "franchise-season-records"
    if (input == "goalie")
      endpoint <- "franchise-goalie-records"
    if (input == "skater")
      endpoint <- "franchise-skater-records"
    
    if (is.null(name) & is.null(id)){
      full_url <- paste0(base_url,"/", endpoint)
    }
    else {
      if (!is.null(id)) {
        full_url <- paste0(base_url,"/", endpoint, "?cayenneExp=franchiseId=", id)
      }
      if (!is.null(name)) {
        v <- c("Montreal Canadiens", "Montreal Wanderers", "St. Louis Eagles", "Hamilton Tigers",
               "Toronto Maple Leafs", "Boston Bruins", "Montreal Maroons", "Brooklyn Americans",
               "Philadelphia Quakers", "New York Rangers", "Chicago Blackhawks", "Detroit Red Wings",
               "Cleveland Barons", "Los Angeles Kings", "Dallas Stars", "Philadelphia Flyers", 
               "Pittsburgh Penguins", "St. Louis Blues", "Buffalo Sabres", "Vancouver Canucks", 
               "Calgary Flames", "New York Islanders", "New Jersey Devils", "Washington Capitals",
               "Edmonton Oilers", "Carolina Hurricanes", "Colorado Avalanche", "Arizona Coyotes",
               "San Jose Sharks", "Ottawa Senators", "Tampa Bay Lightning", "Anaheim Ducks",
               "Florida Panthers", "Nashville Predators", "Winnipeg Jets", "Columbus Blue Jackets",
               "Minnesota Wild", "Vegas Golden Knights", "Seattle Kraken")
        nameid <- match(name, v)
        full_url <- paste0(base_url,"/", endpoint,"?cayenneExp=franchiseId=", nameid)
      }
    }
  }
  
  #Sixth endpoint: has name/id specification by mostRecentTeamId instead of franchiseId
  if (input == "history"){
    endpoint <- "franchise-detail"
    if (is.null(name) & is.null(id)){
      full_url <- paste0(base_url, "/", endpoint)
    }
    else {
      if (!is.null(id)) {
        full_url <- paste0(base_url, "/", endpoint, "?cayenneExp=mostRecentTeamId=", id)
      }
      if (!is.null(name)) {
        u <- c("New Jersey Devils", "New York Islanders", "New York Rangers", "Philadelphia Flyers",
               "Pittsburgh Penguins", "Boston Bruins", "Buffalo Sabres", "Montreal Canadiens",
               "Ottawa Senators", "Toronto Maple Leafs", "Carolina Hurricanes", "Florida Panthers",
               "Tampa Bay Lightning", "Washington Capitals", "Chicago Blackhawks", 
               "Detroit Red Wings", "Nashville Predators", "St. Louis Blues", "Calgary Flames", 
               "Colorado Avalanche", "Edmonton Oilers", "Vancouver Canucks", "Anaheim Ducks", 
               "Dallas Stars","Los Angeles Kings", "San Jose Sharks", "Columbus Blue Jackets", 
               "Minnesota Wild", "Hamilton Tigers", "Philadelphia Quakers", "Montreal Wanderers",
               "Montreal Maroons","St. Louis Eagles", "Cleveland Barons", "Brooklyn Americans",
               "Winnipeg Jets", "Arizona Coyotes", "Vegas Golden Knights", "Seattle Kraken")
        nameid <- match(name, u)
        unum <- c(1:10, 12:30, seq(37,45,by=2), 49, 51:55)
        full_url <- paste0(base_url,"/", endpoint, "?cayenneExp=mostRecentTeamId=", unum[nameid])
      }
    }
  }
  x <- GET(full_url)           #Retrieving data from URL
  y <- content(x, as = "text") #Converiting it to JSON text
  z <- fromJSON(y)             #Converting to list
  as.data.frame(z$data)        #Converting to data frame
}
```

Below are some examples of how to use the function. The first outputs
all of the franchise information. The second shows how adding an `id`
(or `name`) argument will not affect the `"teamtotal"` (and
`"franchise"`) inputs. The third shows all season records, but can also
have a specification of `id` or `name`. The fourth shows goalie
information for the team with 1 as the `franchiseId`. The fifth shows
the skater inforamtion for the team with `name` Carolina Hurricanes. The
sixth shows the admin history and retired numbers for the team with 1 as
the `mostRecentTeamId`.

``` r
## Examples:
#Franchise information
franchise <- getRecords("franchise")

#Total stats for every franchise
total <- getRecords("teamtotal", id=1)

#Season records for specific franchise or for all 
season <- getRecords("season")

#Goalie records for specific franchise
goalie <- getRecords("goalie", id=1)

#Skater records for for specific franchise
skater <- getRecords("skater", name = "Carolina Hurricanes")

#Admin history and retired numbers, Note: id is mostRecentTeamId
admin <- getRecords("history", id=1)
```

## NHL stats API

The `getStats` function will contact the [NHL stats
API](https://gitlab.com/dword4/nhlapi/-/blob/master/stats-api.md) for
the modifier and will output a data frame for a specified team or all
teams. The arguments `recentId` and `name` can be specified if we want
to look at a specific team. Otherwise, we can leave the arguments empty
to output all teams stats.

Note: The Seattle Kraken is a new team that joined in 2021, so there is
no current data on them.

``` r
getStats <- function(name = NULL, recentId = NULL){
  base_url <- "https://statsapi.web.nhl.com/api/v1/teams"
  
  if (is.null(name) & is.null(recentId)){
      full_url <- paste0(base_url, "?expand=team.stats")
      x <- GET(full_url)
      y <- content(x, as = "text")
      z <- fromJSON(y)
      df <- NULL
      for (i in 1:31){
        a <- z$teams[i,]$teamStats[[1]]$splits[[1]]$stat[1,]
        b <- z$teams[i,][,c("id","name")]
        c <- cbind(b,a)
        df <- rbind(df,c)
      }
      df
    }
    else {
      if (!is.null(recentId)) {
        if (recentId == 55){
          stop("Seattle Kraken is a new team in the NHL (as of 2021), 
               so there is no data on them")
        }
        full_url <- paste0(base_url, "/", recentId, "?expand=team.stats")
      }
      if (!is.null(name)) {
        u <- c("New Jersey Devils", "New York Islanders", "New York Rangers", "Philadelphia Flyers",
               "Pittsburgh Penguins", "Boston Bruins", "Buffalo Sabres", "Montreal Canadiens",
               "Ottawa Senators", "Toronto Maple Leafs", "Carolina Hurricanes", "Florida Panthers",
               "Tampa Bay Lightning", "Washington Capitals", "Chicago Blackhawks", 
               "Detroit Red Wings", "Nashville Predators", "St. Louis Blues", "Calgary Flames", 
               "Colorado Avalanche", "Edmonton Oilers", "Vancouver Canucks", "Anaheim Ducks", 
               "Dallas Stars","Los Angeles Kings", "San Jose Sharks", "Columbus Blue Jackets", 
               "Minnesota Wild", "Hamilton Tigers", "Philadelphia Quakers", "Montreal Wanderers",
               "Montreal Maroons","St. Louis Eagles", "Cleveland Barons", "Brooklyn Americans",
               "Winnipeg Jets", "Arizona Coyotes", "Vegas Golden Knights", "Seattle Kraken")
        nameid <- match(name, u)
        unum <- c(1:10, 12:26, 28:30, seq(37,45,by=2), 49, 51:55)
        if (unum[nameid] == 55){
          stop("Seattle Kraken is a new team in the NHL (as of 2021), 
               so there is no data on them")
        }
        full_url <- paste0(base_url, "/", unum[nameid], "?expand=team.stats")
      }
      x <- GET(full_url)
      y <- content(x, as = "text")
      z <- fromJSON(y)
      a <- z$teams[1,]$teamStats[[1]]$splits[[1]]$stat[1,]
      b <- z$teams[1,][,c("id","name")]
      cbind(b,a)
    }
}
```

Below are examples of calling the `getStats` function. The first example
calls all teams and the second calls the first team. The third calls for
the team named Seattle Kraken, which is a new team in the NHL (as of
2021), so there is currently no data on them in the API.

``` r
## Examples
#All stats for all teams
all <- getStats()

#For a specific team
team1 <- getStats(recentId = 1)

#For Seattle Kraken (recentId = 55)
getStats(name = "Seattle Kraken")
```

    ## Error in getStats(name = "Seattle Kraken"): Seattle Kraken is a new team in the NHL (as of 2021), 
    ##                so there is no data on them

## API wrapper

The `getEndpoint` function is a wrapper function that allows the user to
access any of the six API endpoints of `getRecords` or the stats from
`getStats` including the modification for selecting the team using
`name` or `id`. The `input` argument takes the six choices from
`getRecords` and a new choice `"stats"`, which will output `getStats`
for all or a specified team.

``` r
getEndpoint <- function(input, name = NULL, id = NULL){
  if (input == "stats"){
    getStats(name, recentId = id)
  }
  else {
    getRecords(input, name, id)
  }
}
```

# Exploratory Data Analysis

``` r
#Comparing two teams:
#2020 Champions 
lightning <- getEndpoint("skater", name = "Tampa Bay Lightning")
```

    ## No encoding supplied: defaulting to UTF-8.

``` r
#NC Home team
hurricane <- getEndpoint("skater", name = "Carolina Hurricanes")
```

    ## No encoding supplied: defaulting to UTF-8.

## Contingency tables

## Numerical Summaries

## Bar plot

## Histogram

## Box plot

## Scatter plot
