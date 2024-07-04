# Mempersiapkan libraries

library(shiny)
library(shinydashboard)

options(scipen = 99) # me-non-aktifkan scientific notation
library(dplyr) # data prep
library(lubridate) # date data prep
library(ggplot2) # visualisasi statis
library(plotly) # plot interaktif
library(glue) # setting tooltip
library(scales) # mengatur skala pada plot
library(shinyWidgets)
library(shinythemes)
library(sf)
library(RColorBrewer)
library(leaflet)

#membaca data
sales <- read.csv("Adidas US Sales Datasets.csv")

#merubah struktur data
sales <- 
  sales %>% 
  # fungsi untuk mengubah beberapa kolom secara bersamaan ke tipe data yang sama
  mutate_at(.vars = c("Retailer", "Retailer.ID", "Region", "State", "City", "Product", "Sales.Method"), # parameter untuk memilih kolom yang akan diubah tipe datanya
            .funs = as.factor) %>% # parameter untuk fungsi tipe data yang dituju
  mutate(Invoice.Date = dmy(Invoice.Date))

sales$day <-  day(sales$Invoice.Date)
sales$month <- month(sales$Invoice.Date)
sales$year <- year(sales$Invoice.Date)
convert_season <- function(x) {
  if (x == 1 | x == 2 | x == 12) 
  {x <- "Winter"}
  else if (x == 3 | x == 4 | x == 5 )
  {x <- "Spring"}
  else if (x == 6 | x == 7 | x == 8 )
  {x <- "Summer"}
  else 
  {x <- "Autumn"}
}
sales$season <- sapply(X = sales$month, # kolom angka yang mau diubah
                       FUN = convert_season) 
convert_product <- function(x) {
  if(x == "Men's Apparel") {x <- "MA"}
  else if ( x == "Men's Street Footwear"){ x <- "MSF"}
  else if ( x == "Men's Athletic Footwear"){ x <- "MAF"}
  else if ( x == "Women's Apparel"){ x <- "WA"}
  else if ( x == "Women's Athletic Footwear"){ x <- "WAF"}
  else {x <- "WSF"}}

sales$Product.ID <- sapply(X = sales$Product, # kolom angka yang mau diubah
                           FUN = convert_product)

# ubah ke factor
sales$Product.ID <- as.factor(sales$Product.ID)
sales$season <- as.factor(sales$season)
sales$day <- wday(sales$Invoice.Date)
sales$day <- as.factor(sales$day)
sales$month <- month(sales$Invoice.Date, label = T, abbr = F)
sales$month <- as.factor(sales$month)
sales$year <- as.factor(sales$year)

best_product <- 
  sales %>%  
  group_by(Product) %>% 
  summarise(Units.Sold = sum(Units.Sold)) %>% 
  arrange(-Units.Sold) %>% 
  head(1)

ave_product <- 
  sales %>%  
  group_by(season) %>% 
  summarise(Units.Sold = sum(Units.Sold)) %>% 
  arrange(-Units.Sold)

ave_sales <- 
  sales %>%  
  group_by(season) %>% 
  summarise(Total.Sales = sum(Total.Sales)) %>% 
  arrange(-Total.Sales) 

ave_op <- 
  sales %>%  
  group_by(season) %>% 
  summarise(Operating.Margin = mean(Operating.Margin)) %>% 
  arrange(-Operating.Margin)

#TAB MAPS
# Geospatial data available at the json format
#dari web https://github.com/PublicaMundi/MappingAPI/blob/master/data/geojson/us-states.json klik raw untuk mendapatkan link raw data
US_json <- tempfile(fileext = ".json")

download.file(
  "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json",
  US_json
)

my_sf <- read_sf(US_json)

# TAB DATA SET data for display in table
datadis <- sales

colnames(datadis) <- gsub(pattern = "[.]", replacement = " ", colnames(datadis))