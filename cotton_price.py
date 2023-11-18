import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from sklearn.linear_model import LinearRegression
from datetime import datetime

# Read the data from the file
cotton = pd.read_csv('~/Desktop/Cotton Stuff/cotton-prices-historical-chart-data.csv')

rsiData = pd.read_csv('~/Desktop/Cotton Stuff/Annual_Cotton_CSV.csv')
#strip dollar signs and percentage symbols from columns
rsiData['Average Closing Price'] = rsiData['Average Closing Price'].str.replace('$', '').astype(float)
rsiData['Annual %Change'] = rsiData['Annual %Change'].str.rstrip('%').astype(float) / 100
# Create a subplot
model = LinearRegression()
model.fit(rsiData[['Year']], rsiData['Average Closing Price'])
predicted_prices = model.predict(rsiData[['Year']])

# Create a list of dates for each year quarter
quarter_dates = []
for year in range(rsiData['Year'].min(), rsiData['Year'].max() + 1):
    for quarter in range(1, 5):
        if quarter == 1:
            quarter_dates.append(f"{year}-01-01")
        elif quarter == 2:
            quarter_dates.append(f"{year}-04-01")
        elif quarter == 3:
            quarter_dates.append(f"{year}-07-01")
        else:
            quarter_dates.append(f"{year}-10-01")

quarter_dates = [datetime.strptime(date, "%Y-%m-%d") for date in quarter_dates]

fig = make_subplots(rows=1, cols=1, shared_xaxes=True, subplot_titles=('Cotton Price'))

scatter_trace = go.Scatter(x=cotton['date'], y=cotton['value'], mode='markers', name='Price')
fig.add_trace(scatter_trace, row=1, col=1)

rolling_mean_trace = go.Scatter(x=cotton['date'], y=cotton['value'].rolling(365).mean(), marker_color='purple', name='Annual Mean Price')
fig.add_trace(rolling_mean_trace, row=1, col=1)

fig.add_trace(go.Scatter(x=rsiData['Year'], y=predicted_prices, mode='lines', name='Linear Regression Line'), row=1, col=1)

# Set X-axis ticks and labels for quarters
tickvals = quarter_dates
ticktext = [f'{date.year}' if i % 4 == 0 else '' for i, date in enumerate(quarter_dates)]

fig.update_xaxes(
    tickmode='array',
    tickvals=tickvals,
    ticktext=ticktext
)
# Add solid red lines at Y values of 1.00 and 0.60
fig.add_shape(
    type="line",
    x0=quarter_dates[0],
    x1=quarter_dates[-1],
    y0=1.00,
    y1=1.00,
    line=dict(color="red", width=2),
    row=1,
    col=1,
)

fig.add_shape(
    type="line",
    x0=quarter_dates[0],
    x1=quarter_dates[-1],
    y0=0.60,
    y1=0.60,
    line=dict(color="red", width=2),
    row=1,
    col=1,
)

# Show the figure
fig.show()
