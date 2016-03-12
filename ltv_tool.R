# Config for displaying numbers
options(scipen = 999)

# Load libraries
library(gridExtra)
library(lattice)

# Download and unzip data
data_file <- tempfile()
download.file("https://raw.githubusercontent.com/ilyavorobiev/products_analysis/master/data/products_data.tsv",data_file, mode="wb")

# Read data
products_data <- read.csv2(data_file, sep="\t", header=FALSE)
products_data <- setNames(products_data, c("User","Product","Channel","Lifetime","Before","After"))

# Preprocess data
products_data$Product <- as.character(products_data$Product)
products_data$Product <- as.character(lapply(strsplit(products_data$Product,"product="), function(x) x[2]))

products_data$Channel <- as.character(products_data$Channel)
products_data$Channel <- as.character(lapply(strsplit(products_data$Channel,"channel="), function(x) x[2]))
 
products_data$Lifetime <- as.character(products_data$Lifetime)
products_data$Lifetime <- as.character(lapply(strsplit(products_data$Lifetime,"lifetime="), function(x) x[2]))

products_data$Before <- as.character(products_data$Before)
products_data$Before <- as.character(lapply(strsplit(products_data$Before,"before="), function(x) x[2]))

products_data$After <- as.character(products_data$After)
products_data$After <- as.character(lapply(strsplit(products_data$After,"after="), function(x) x[2]))

products_data$Lifetime <- as.numeric(products_data$Lifetime)
products_data$Before <- as.numeric(products_data$Before)
products_data$After <- as.numeric(products_data$After)

# LTV calculation
products_data$LTV <- round((products_data$After - products_data$Before) * ( products_data$Lifetime / (3600*24*7)))

# Products perfomance (aggregated)
products_perf <- aggregate(LTV ~ Product, 
                          FUN = function(x) round(mean(x)),
                          data=products_data)
products_ltv_mean <- mean(products_perf$LTV)
products_perf$LTV_Comparison <- round(((products_perf$LTV/products_ltv_mean) - 1)*100, digits = 1)

installs <- aggregate(LTV ~ Product, 
                      FUN = function(x) length(x),
                      data=products_data)
names(installs)[names(installs)=="LTV"] <- "Installs"

products_perf <- merge(products_perf, installs, by = "Product")

products_installs_mean <- mean(products_perf$Installs)
products_perf$Installs_Comparison <- round(((products_perf$Installs/products_installs_mean) - 1)*100, digits = 1)

names(products_perf)[names(products_perf)=="LTV_Comparison"] <- "LTV Comparison, %"
names(products_perf)[names(products_perf)=="Installs_Comparison"] <- "Installs Comparison, %"

# Save chart function
save_chart <- function(filename,width,height){
  dev.copy(png, file = filename,height=height,width=width)
  dev.off()
  plot.new()
}

# Products perfomance charts
# table 
grid.table(products_perf)
save_chart("products_perf.png",600,200)
# installs
barplot(products_perf$Installs, names.arg = products_perf$Product, main = "Installs by Product")
save_chart("products_installs.png",480,480)
# ltv
barplot(products_perf$LTV, names.arg = products_perf$Product, main = "LTV by Product")
save_chart("products_ltv.png",480,480)

# Channels perfomance (aggregated)
channels_perf <- aggregate(LTV ~ Channel, 
                           FUN = function(x) round(mean(x)),
                           data=products_data)
channels_ltv_mean <- mean(channels_perf$LTV)
channels_perf$LTV_Comparison <- round(((channels_perf$LTV/channels_ltv_mean) - 1)*100, digits = 1)

installs <- aggregate(LTV ~ Channel, 
                      FUN = function(x) length(x),
                      data=products_data)
names(installs)[names(installs)=="LTV"] <- "Installs"

channels_perf <- merge(channels_perf, installs, by = "Channel")

channels_installs_mean <- mean(channels_perf$Installs)
channels_perf$Installs_Comparison <- round(((channels_perf$Installs/channels_installs_mean) - 1)*100, digits = 1)

names(channels_perf)[names(channels_perf)=="LTV_Comparison"] <- "LTV Comparison, %"
names(channels_perf)[names(channels_perf)=="Installs_Comparison"] <- "Installs Comparison, %"

# Channels perfomance charts
# table
grid.table(channels_perf)
save_chart("channels_perf.png",500,800)
# installs
barplot(channels_perf$Installs, names.arg = channels_perf$Channel, main = "Installs by Channel")
save_chart("channels_installs.png",3750,1000)
# ltv 
barplot(channels_perf$LTV, names.arg = channels_perf$Channel, main = "LTV by Channel")
save_chart("channels_ltv.png",3750,1000)
