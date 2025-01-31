#Load Packages:
#Various useful packs have been loaded to simplify processing of the data set.
pacman::p_load(
  rio, # importing data
  here, # relative file pathways
  janitor, # data cleaning and tables
  lubridate, # working with dates
  matchmaker, # dictionary-based cleaning
  epikit, # age_categories() function
  tidyverse, # data management and visualization
  styler, # source code formatting
  lintr, # detects bad code patterns, which are not errors
  skimr, # preview tibbles (aka data frames)
  todor # add TODO comments to your project
)

#Data import:
#The data set was improvised and checked to ensure that all columns were correctly recognized and named.
linelist_raw <- read_csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2_5455118.csv", col_names = TRUE) 

#Renaming the column:
#To improve readability and handling, the space has been replaced with a character.
#In addition, logical values have been converted into numerical values for easier processing.
linelist <- linelist_raw %>% 
            rename(country_name = `Country Name`,
                   country_code = `Country Code`) %>%
            mutate_if(is.logical, as.numeric)


#definition of valid country codes:
##The data set has summarized all countries and countries into groupings. As only the countries were wanted for further processing, the ISO 3166-1 Alpha-3 country code was defined.
iso_country_codes <- c(
  "AFG", "ALB", "DZA", "AND", "AGO", "ATG", "ARG", "ARM", "AUS", "AUT", "AZE", "BHS", "BHR", "BGD", "BRB", "BLR",
  "BEL", "BLZ", "BEN", "BTN", "BOL", "BIH", "BWA", "BRA", "BRN", "BGR", "BFA", "BDI", "CPV", "KHM", "CMR", "CAN",
  "CAF", "TCD", "CHL", "CHN", "COL", "COM", "COG", "COD", "CRI", "CIV", "HRV", "CUB", "CYP", "CZE", "DNK", "DJI",
  "DMA", "DOM", "ECU", "EGY", "SLV", "GNQ", "ERI", "EST", "SWZ", "ETH", "FJI", "FIN", "FRA", "GAB", "GMB", "GEO",
  "DEU", "GHA", "GRC", "GRD", "GTM", "GIN", "GNB", "GUY", "HTI", "HND", "HUN", "ISL", "IND", "IDN", "IRN", "IRQ",
  "IRL", "ISR", "ITA", "JAM", "JPN", "JOR", "KAZ", "KEN", "KIR", "PRK", "KOR", "KWT", "KGZ", "LAO", "LVA", "LBN",
  "LSO", "LBR", "LBY", "LIE", "LTU", "LUX", "MDG", "MWI", "MYS", "MDV", "MLI", "MLT", "MHL", "MRT", "MUS", "MEX",
  "FSM", "MDA", "MCO", "MNG", "MNE", "MAR", "MOZ", "MMR", "NAM", "NRU", "NPL", "NLD", "NZL", "NIC", "NER", "NGA",
  "MKD", "NOR", "OMN", "PAK", "PLW", "PAN", "PNG", "PRY", "PER", "PHL", "POL", "PRT", "QAT", "ROU", "RUS", "RWA",
  "KNA", "LCA", "VCT", "WSM", "SMR", "STP", "SAU", "SEN", "SRB", "SYC", "SLE", "SGP", "SVK", "SVN", "SLB", "SOM",
  "ZAF", "SSD", "ESP", "LKA", "SDN", "SUR", "SWE", "CHE", "SYR", "TJK", "THA", "TLS", "TGO", "TON", "TTO", "TUN",
  "TUR", "TKM", "TUV", "UGA", "UKR", "ARE", "GBR", "USA", "URY", "UZB", "VUT", "VEN", "VNM", "YEM", "ZMB", "ZWE"
)

#Filter invalid country codes:
#All lines that do not match one of the country codes have been removed.
linelist <- linelist[linelist$country_code %in% iso_country_codes, ]

#Remove the country code column
#Since the country code is no longer needed for the following editing, it has been removed.
linelist <- linelist %>% 
            select(-country_code)

#Remove columns with only missing values:
#NA values have been removed using the filter function. This minimizes the data set and simplifies the analysis.
linelist <- Filter(function(x) !all(is.na(x)), linelist)

#Transformation to a long format:
#The previous data set had a wide format, to simplify the analysis it was transformed into a long format. This makes it easier to visualize the data.
#In addition, the long data set has been split into three columns and renamed.
linelist <- linelist %>%
            pivot_longer(cols = where(is.numeric),
             names_to = "Year", 
             values_to = "Value")

#Grouping of data by year:
#To analyze the values over the years, separate calculations were carried out for each year. The median, mean, minimum and maximum were calculated.
aggregated_data <- linelist %>%
  group_by(Year) %>%
  summarize(
    Median = median(Value, na.rm = TRUE),
    Mean = mean(Value, na.rm = TRUE),
    Minimum = min(Value, na.rm = TRUE),
    Maximum = max(Value, na.rm = TRUE)
  ) %>%

#Restructuring of the aggregated data:
#Here too, the data was put into a long format to make it easier to work with
  pivot_longer(cols = c(Median, Mean, Minimum, Maximum), 
               names_to = "Statistic", 
               values_to = "Value")

#Rounding of the values:
#All values in the columns have been rounded to two decimal places after the decimal point. This improves readability and clarity.
linelist$Value = round(linelist$Value,2)
aggregated_data$Value = round(aggregated_data$Value,2)

#Saving the results:
#The cleaned and aggregated data was saved again as a csv file so that it could be used for further visualizations.
write_csv(linelist, "clean_data.csv")
write_csv(aggregated_data, "statistics.csv")