
library(tidyverse)
library(tidyr)
library(readxl)
library(plotly)
library(RColorBrewer)
library(viridis)


# Location for your working directory. This is where you will keep your script and your files. 
# We need  explicitly put the working directory here because this script is not part of a project. 
# If you decide to make an R project the working directory will be automatically set to inside the project. 
#setwd("C:/Users/ScrutonM/Documents/Projects/R/Tsawout")

# Name of excel sheet to input. I encourage you in the future to work from your raw outputs to get it 
# into a joint format within R to eliminate a step outside of R and improve reproducability. 
#raw_tide_data <- read_excel("AggregateData_20250929_R1.xlsx",col_types = c("date",rep("numeric",10)))

folder_path <- "Data/"
files <- list.files(
  path = folder_path,
  full.names = TRUE,
  recursive = TRUE
)

# data_list_raw <- lapply(files, function(f) {
#   read.csv(f, header = FALSE)
# })

data_list_raw <- lapply(files, read.csv, header = FALSE)

serial_numbers <- sapply(data_list_raw, function(df) df[2,1])

#prefixed <- lapply(serial_numbers, function(x) {
#  paste0("logger_", x)
#})

data_list <- lapply(data_list_raw, function(df) {
  
  # find first row where column 1 contains "Date"
  start_row <- which(df[[1]] == "Date")[1]
  
  # keep everything from that row onward
  df <- df[start_row:nrow(df), ]
  
  # reset row numbers
  #rownames(df) <- NULL
  return(df)
})

file_list <- list()

for (file in 1:length(data_list)){
  
  current_file <- data_list[[file]]
  
  current_sn <- serial_numbers[[file]]
  
  current_file <- current_file %>%
    mutate(sn=current_sn)
  
  file_list[[file]] <- current_file
    }

# This is telling R to take the raw data and put it into long format.
tide_data_long <- pivot_longer(raw_tide_data, names_to = "col_name", values_to = "value", cols = 2:11 )

# The "mutate" function is for column creation. This new column "param_type" is getting its values 
# by locating key words within the col_name column and determining what kind of param type it is. 
tide_data_long <- tide_data_long %>%
  mutate(param_type = ifelse(grepl("Level", col_name), "Level (m)", 
                           ifelse(grepl("Air Temp", col_name), "Water Temp (°C)", 
                                  ifelse(grepl("Average Temp", col_name), "Water Temp (°C)", 
                                         ifelse(grepl("Salinity", col_name), "Salinity (PSU)", 
                                                ifelse(grepl("Observed Tides",col_name), "Level (m)","Other"))))))


# This is getting the number of variables in your data set. 
n_vars <- length(unique(tide_data_long$col_name))
#this brute forces the line type for each data point. Note it isn't the same order as the legend, go look at the "unique(tide_data_long$col_name)" by using CTRL+ENTER.
#could do this colours. See here https://sape.inf.usi.ch/quick-reference/ggplot2/colour.
line_list <- c("solid","dot","solid","solid","solid","dot","solid","solid","dot","dash")
colour_list <- c("green","green","turquoise","orange","blue","blue","purple","red","red","black")

# This is using the number of variables to automate a colour pallet. The viridis palette or RColourBrewer are good for palette creation. 
# You can also create your own palette by just listing colour  names/hex names in a list where the plasma(xx) code is. 
#pal <- setNames(viridis::plasma(n_vars, begin = 0, end = 0.92), unique(tide_data_long$col_name))
pal <- setNames(colour_list,unique(tide_data_long$col_name))
line <- setNames(line_list, unique(tide_data_long$col_name))



# This code is a little complicated but pretty much it's creating a plotly plot for each 
# param_type and then assigning colors based off the pals palette defined earlier.
plots <- tide_data_long %>%
  split(.$param_type) %>%
  lapply(function(df) {
    plot_ly(
      data = df,
      type = 'scatter',
      mode = 'lines',
      x = ~`Date & Time`,
      y = ~value,
      color = ~col_name,   # differentiates within facet
      colors = pal,
      linetype = ~col_name,
      linetypes = line
    ) %>%
      layout(
        showlegend = TRUE,
        xaxis = list(rangeslider = list(visible = TRUE), title = "Date"), 
        yaxis = list(title = unique(df$param_type),range = c(0,max(df$value)))
      )
  })

# This is grouping all the individual plots that were created in the last chunk of code and creating one cohesive plot.  
other_plots <-subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE)

#run this line to see the plot on the Viewer on the right. To see in big screen click on the Zoom button at the top of the Viewer panel. 
other_plots


