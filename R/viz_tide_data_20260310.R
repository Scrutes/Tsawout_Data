library(tidyverse)
library(tidyr)
library(readxl)
library(plotly)
library(RColorBrewer)
library(viridis)
library(lubridate)

# ── Load files ────────────────────────────────────────────────────────────────
folder_path <- "Data_max/"
files <- list.files(path = folder_path, full.names = TRUE, recursive = TRUE)

# Get serial numbers
data_list_raw_sn <- lapply(files, read.csv, header = FALSE)
serial_numbers   <- sapply(data_list_raw_sn, function(df) df[2, 1])

# Read each file from the "Date," header row onward
data_list_raw <- lapply(files, function(f) {
  lines    <- readLines(f, warn = FALSE)
  date_row <- which(grepl("^Date,", lines))[1]
  read.csv(text = lines[date_row:length(lines)], header = TRUE)
})

# ── Clean & type-cast each file ───────────────────────────────────────────────
data_list <- lapply(data_list_raw, function(df) {
  df$Date        <- as.Date(parse_date_time(df$Date, orders = c("mdy", "ymd")))
  df$Time        <- as.character(df$Time)
  df$ms          <- as.numeric(df$ms)
  df$LEVEL       <- as.numeric(df$LEVEL)
  df$TEMPERATURE <- as.numeric(df$TEMPERATURE)
  df$Salinity    <- if ("Salinity" %in% colnames(df)) as.numeric(df$Salinity) else NA_real_
  df
})

# ── Add serial number column & combine ────────────────────────────────────────
data_combined <- bind_rows(lapply(seq_along(data_list), function(i) {
  data_list[[i]] %>% mutate(sn = as.character(serial_numbers[[i]]))
}))

# ── Pivot to long format ──────────────────────────────────────────────────────
tide_data_long <- data_combined %>%
  pivot_longer(cols = c("LEVEL", "TEMPERATURE", "Salinity"),
               names_to  = "col_name",
               values_to = "value") %>%
  mutate(
    param_type = case_when(
      col_name == "LEVEL"       ~ "Level (m)",
      col_name == "TEMPERATURE" ~ "Water Temp (°C)",
      col_name == "Salinity"    ~ "Salinity (PSU)",
      TRUE                      ~ "Other"
    ),
    DateTime = ymd_hms(paste(Date, Time))
  ) %>%
  distinct()

# ── Hourly averages ───────────────────────────────────────────────────────────
tide_hourly <- tide_data_long %>%
  mutate(DateTime = floor_date(DateTime, unit = "hour")) %>%
  group_by(DateTime, sn, col_name, param_type) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

# ── !! EDIT READABLE NAMES HERE !! ───────────────────────────────────────────
# Add or remove rows to match your serial numbers exactly.
# Run unique(tide_hourly$sn) to see all your serial numbers first.
sn_labels <- c(
  "1093993" = "Station A",
  "2188289" = "Station B",
  "2190552" = "Station C", 
  "1094001" = "Station D"
  # add more as needed...
)

# Apply labels — any sn not in sn_labels will keep its raw serial number
tide_hourly <- tide_hourly %>%
  mutate(sn_label = ifelse(sn %in% names(sn_labels), sn_labels[sn], sn))

# ── Colour & line-type palettes ───────────────────────────────────────────────
valid_names <- unique(tide_hourly$sn_label)
n_vars      <- length(valid_names)

colour_list <- c("darkgreen", "turquoise", "orange", "blue", "purple", "red", "red", "black")
line_list   <- c("solid", "solid",     "solid",  "dot",  "solid",  "solid","dot", "dash")

pal  <- setNames(colour_list[1:n_vars], valid_names)
line <- setNames(line_list[1:n_vars],   valid_names)

# ── Build one plot per parameter ──────────────────────────────────────────────
plots <- tide_hourly %>%
  split(.$col_name) %>%
  lapply(function(df) {
    
    # Build traces manually so we can set opacity per trace
    p <- plot_ly()
    
    for (station in unique(df$sn_label)) {
      df_sub <- df %>% filter(sn_label == station)
      p <- add_trace(p,
                     data      = df_sub,
                     type      = "scatter",
                     mode      = "lines",
                     x         = ~DateTime,
                     y         = ~value,
                     name      = station,
                     line      = list(
                       color = pal[[station]],
                       dash  = line[[station]],
                       width = 1.5
                     ),
                     opacity   = 0.8,
                     legendgroup = station,
                     showlegend  = !duplicated(unique(df$sn_label))[which(unique(df$sn_label) == station)]
      )
    }
    
    p %>% layout(
      xaxis = list(title = "Date", rangeslider = list(visible = TRUE)),
      yaxis = list(title = unique(df$param_type),
                   range = c(0, max(df$value, na.rm = TRUE)))
    )
  })

# ── Combine into one stacked plot ─────────────────────────────────────────────
subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE) %>%
  layout(showlegend = TRUE)