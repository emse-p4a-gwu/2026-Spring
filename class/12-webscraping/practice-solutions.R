# install.packages("usethis")

library(tidyverse)
library(rvest)
library(usethis)

# Practice 1 ----

# Scrape data on famous quotes from
# http://quotes.toscrape.com/

# Your resulting data frame should have these fields:

# - `quote`: The quote
# - `author`: The author of the quote
# - `about_url`: The url to the "about" page

url <- "http://quotes.toscrape.com"
html <- read_html(url)

quote_nodes <- html %>% 
    html_elements(".quote")

df <- tibble(
    quote = quote_nodes %>%
        html_element(".text") %>%
        html_text(),
    author = quote_nodes %>%
        html_element(".author") %>%
        html_text(), 
    about_url = quote_nodes %>%
        html_element("a") %>% 
        html_attr("href")
) %>% 
    # Pasting on the root url since the scraped urls are only *relative* urls
    mutate(about_url = paste0(url, about_url))

head(df)



# Practice 2 ----

# Template code is provided to scrape data on F1 drivers for the 2024 season from
# https://www.formula1.com/en/results.html/2024/drivers.html
# 
# Your job is to extend it to scrape the data from seasons 2010 to 2024.

# Code to scrape data from a single page (the 2024 season):

url <- "https://www.formula1.com/en/results.html/2024/drivers.html"

# Get the data frame
df_list <- read_html(url) %>% 
    html_table()
df <- df_list[[1]]
df$year <- 2024 # Store the year (not in the scraped data)

# Some formatting
df <- df %>% 
    select(
        year, position = Pos, driver = Driver, nationality = Nationality, 
        team = Car, points = Pts
    ) %>% 
    separate(driver, into = c('first', 'last', 'abb'))
head(df)

# Now, extend this to scrape the data from seasons 2010 to 2024

# First, write a function to scrape data from one page

get_f1_data <- function(year) {
    
    # Build the url
    url_start <- "https://www.formula1.com/en/results.html/"
    url_end <- "/drivers.html"
    url <- paste(url_start, year, url_end, sep = "")
    
    # Get the data frame
    df_list <- read_html(url) %>% 
        html_table()
    df <- df_list[[1]]
    df$year <- year # Store the year (not in the scraped data)
    
    # Some formatting
    df <- df %>%
        select(
            year, position = Pos, driver = Driver, nationality = Nationality, 
            team = Car, points = Pts
        ) %>% 
        separate(driver, into = c('first', 'last', 'abb'))
    
    return(df)
}

# Now map the function onto the desired set of years

years <- 2010:2024
df <- map_df(years, \(x) get_f1_data(x))

head(df)

# Plot of most total points by team

df %>% 
    mutate(
        # Merge multiple different Red Bull Racing teams
        team = ifelse(str_detect(team, 'Red Bull Racing'), 'Red Bull Racing', team)
    ) %>% 
    group_by(team) %>% 
    summarise(total_points = sum(points)) %>% 
    arrange(desc(total_points)) %>% 
    # Just keep top 10 teams
    slice(1:10) %>% 
    ggplot() +
    geom_col(aes(x = total_points, y = reorder(team, total_points))) +
    theme_minimal()


# Practice 3 ----

# API Documentation: https://www.alphavantage.co/documentation/#dailyadj

# 1. Make your .env file:  

file.create(".env")

# 2. Edit your .env file:

file.edit(".env")

# 3. Register for a key: https://www.alphavantage.co/support/#api-key

# 4. Store your key, e.g. ALPHAVANTAGE_API_KEY=ZF33JCWPWWQDX4LW

# 5. Load your .env file: 

dotenv::load_dot_env()

# 6. Load your API key:

api_key <- Sys.getenv("ALPHAVANTAGE_API_KEY")

# 7. Build the url to request historical stock prices for a stock of your choice

symbol <- "NFLX" # Netflix

url <- paste0(
    "https://www.alphavantage.co/query", 
    "?function=TIME_SERIES_DAILY",
    "&symbol=", symbol, 
    "&apikey=", api_key, 
    "&datatype=csv"
)

# 8. Read in the data, then make this a stock plot with ggplot

df <- readr::read_csv(url)

df %>% 
    ggplot() + 
    geom_line(
        aes(
            x = timestamp, 
            y = close
        )
    ) + 
    theme_bw() +
    labs(
        x = "Date",
        y = "Closing Price ($USD)", 
        title = paste0("Stock Prices: ", symbol)
    )
