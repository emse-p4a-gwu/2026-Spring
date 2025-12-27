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
    
    # Write code here
}

# Now map the function onto the desired set of years

# Write code here




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

url <- paste0(
    # Write code here
)
    
# 8. Read in the data, then make this a stock plot with ggplot

df <- readr::read_csv(url)
