#READ THE DATASET

funding_data <- read.csv("dataset_raw.csv", stringsAsFactors = FALSE)

#VIEW THE FIRST FEW ROWS
head(funding_data)

#check structure of data
str(funding_data)

#CLEANING DATA

#view column names
colnames(funding_data)

#checking amount values (few)

head(funding_data$Amount.in.USD,10)

#clean AmountInUSD column

funding_data$Amount.in.USD<- gsub(",","", funding_data$Amount.in.USD)  #remove commas
funding_data$Amount.in.USD<-as.numeric(funding_data$Amount.in.USD)  #convert to numeric

#check again
summary(funding_data$Amount.in.USD)

#remove NAs
sum(is.na(funding_data$Amount.in.USD))

clean_funding_data <- funding_data[!is.na(funding_data$Amount.in.USD),]


#1ST ANALYSIS- TOTAL FUNDING DONE BY TOP CITIES

#check top cities by total funding amount

top_cities <- aggregate(Amount.in.USD ~ City..Location, data = clean_funding_data, sum)

#sort in descending order
top_cities <- top_cities[order(top_cities$Amount.in.USD, decreasing = TRUE),]

#View top 10 cities
head(top_cities,10)

#Barplot of top 10 cities by funding

png("top_10_cities_funding.png", width = 800, height = 600)
barplot(
  top_cities$Amount.in.USD[1:10],
  names.arg = top_cities$City..Location[1:10],
  las = 2,
  col = "steelblue",
  main = "Top 10 cities by Total Startup Funding",
  ylab = "Total Funding (in USD)"
)
dev.off()

install.packages("stringr")

#TOP INVESTORS BY NUMBER OF INVESTMENTS

#Load required package
library(stringr)

#create a vector of all investors (split by comma)
all_investors <- unlist(strsplit(clean_funding_data$Investors.Name,","))

#remove leading/trailing whitespace
all_investors<- str_trim(all_investors)

#create a frequency table
investor_freq<- sort(table(all_investors), decreasing = TRUE)

#View top 10 investors
head(investor_freq,10)

#visulaise top 10 investors
png("top_10_investors.png", width = 800, height = 600)
barplot(
  head(investor_freq,10),
  las = 2,
  col = "darkgreen",
  main = "Top 10 Investors by number of investments",
  ylab = "Number of Investments"
)
dev.off()

#MOST POPULAR SECTORS (BASED ON NUMBER OF STRATUPS)

#count frequency of each sector
sector_freq<- sort(table(clean_funding_data$Industry.Vertical),decreasing = TRUE)

#View top 10 sectors
head(sector_freq, 10)

#Barplot


barplot(
  head(sector_freq,10),
  las = 2,
  col = "darkorange",
  main = "Top 10 sectors by Number of fundings",
  ylab = "Number of fundings"
)


#CLEANING THE INDUSTRY VERTICAL

clean_funding_data$Industry.Vertical<- tolower(clean_funding_data$Industry.Vertical)

clean_funding_data$Industry.Vertical <- gsub("e-commerce", "ecommerce", clean_funding_data$Industry.Vertical)
clean_funding_data$Industry.Vertical <- gsub("e commerce", "ecommerce", clean_funding_data$Industry.Vertical)
clean_funding_data$Industry.Vertical <- gsub("health care", "healthcare", clean_funding_data$Industry.Vertical)
clean_funding_data$Industry.Vertical <- gsub("nan", NA, clean_funding_data$Industry.Vertical)  
# replace "nan" with NA

clean_funding_data <- clean_funding_data[!is.na(clean_funding_data$Industry.Vertical), ]

#rerun the data

sector_freq<- sort(table(clean_funding_data$Industry.Vertical), decreasing = TRUE)
head(sector_freq,10)

#re-plot

png("top_10_sectors_by_number_of_fundings.png", width = 800, height = 600)
barplot(
  head(sector_freq,10),
  las = 2,
  col = "darkorange",
  main = "Top 10 sectors by Number of fundings(cleaned)",
  ylab = "Number of fundings"
)
dev.off()

#trend of fundings per year

head(clean_funding_data$Date)

clean_funding_data$Date <- as.Date(clean_funding_data$Date, format = "%d-%m-%Y")

clean_funding_data$Year <- format(clean_funding_data$Date, "%Y")
clean_funding_data$Year <- as.numeric(clean_funding_data$Year)

# aggregate total funding by year
funding_per_year <- aggregate(Amount.in.USD ~ Year, data = clean_funding_data, sum)

# plot
library(ggplot2)


png("total_startup_funding_by_year.png", width = 800, height = 600)
ggplot(funding_per_year, aes(x = Year, y = Amount.in.USD)) +
  geom_col(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Total Startup Funding by Year",
       x = "Year",
       y = "Total Funding (USD)")
dev.off()

#final analysis top 10 funded startups

# Aggregate total funding by startup
top_startups <- aggregate(Amount.in.USD ~ Startup.Name, data = clean_funding_data, sum)

# Sort in descending order
top_startups <- top_startups[order(-top_startups$Amount.in.USD), ]

# Take top 10
top_10_startups <- head(top_startups, 10)

# Bar plot
library(ggplot2)
library(scales)

png("top_10_startups.png", width = 800, height = 600)
ggplot(top_10_startups, aes(x = reorder(Startup.Name, Amount.in.USD), y = Amount.in.USD)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Top 10 Funded Startups",
       x = "Startup Name",
       y = "Total Funding (USD)")
dev.off()




