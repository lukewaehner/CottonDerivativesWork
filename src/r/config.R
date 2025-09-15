# Configuration file for Cotton Derivatives Analysis
# Centralized parameters and settings

# Data parameters
DATA_START_YEAR <- 2018
DATA_END_YEAR <- 2022
TRADING_DAYS_PER_YEAR <- 252

# Simulation parameters
DEFAULT_SIMULATIONS <- 1000  # Increased from 100
DEFAULT_TIME_PERIOD <- 259
CONFIDENCE_LEVELS <- c(0.05, 0.25, 0.5, 0.75, 0.95)

# Threshold parameters
LOWER_THRESHOLD <- 1.16
UPPER_THRESHOLD <- 1.4

# File paths
RAW_DATA_PATH <- "../../data/raw/cotton-prices-historical-chart-data.csv"
ANNUAL_DATA_PATH <- "../../data/raw/Annual_Cotton_CSV.csv"
PROCESSED_DATA_PATH <- "../../data/processed/CottonFiveYearFrame.rda"
OUTPUT_PATH <- "../../results/simulations/"

# Visualization parameters
PLOT_WIDTH <- 12
PLOT_HEIGHT <- 8
PLOT_DPI <- 300
