# Cotton Derivatives Analysis - Project Overview

## Objective
This project analyzes cotton price derivatives using statistical modeling and simulation techniques to estimate contract price bounds and understand price movements over time.

## Methodology

### 1. Data Collection & Preprocessing
- **Historical Price Data**: Daily cotton prices from 2018-2022
- **Annual Summary Data**: Yearly cotton price statistics
- **Data Cleaning**: Type conversion, date formatting, outlier handling

### 2. Statistical Analysis
- **Descriptive Statistics**: Mean, standard deviation, price ranges
- **Time Series Analysis**: Price trends and volatility patterns
- **Visualization**: Line charts, candlestick plots, statistical overlays

### 3. Modeling Approaches

#### Linear Regression Model
- **Purpose**: Predict cotton prices based on time trends
- **Implementation**: Python with scikit-learn
- **Output**: Interactive visualizations with Plotly

#### Geometric Brownian Motion (GBM) Simulation
- **Purpose**: Estimate future price bounds for derivatives contracts
- **Parameters**: 
  - Drift (μ): Historical average return
  - Volatility (σ): Historical price volatility
  - Time horizon: Contract duration
- **Implementation**: Monte Carlo simulation in R
- **Output**: Price distribution and confidence intervals

### 4. Key Findings
- Price volatility patterns
- Seasonal trends in cotton prices
- Risk assessment for derivatives contracts
- Confidence intervals for future price movements

## File Structure
```
├── data/           # Raw and processed datasets
├── src/           # Analysis scripts (R & Python)
├── results/       # Outputs and visualizations
├── scripts/       # Utility scripts
└── docs/          # Documentation
```

## Usage
1. Run `./run_analysis.sh` for complete analysis
2. Individual scripts can be run from their respective directories
3. Check `results/` for outputs and visualizations

## Dependencies
- R packages: ggplot2, dplyr
- Python packages: pandas, scikit-learn, plotly
