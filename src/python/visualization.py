"""
Cotton Price Visualization and Analysis
Improved version with better statistical analysis and interactive features
"""

import pandas as pd
import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import plotly.express as px
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score, mean_squared_error
from scipy import stats
import warnings
warnings.filterwarnings('ignore')


class CottonPriceAnalyzer:
    """Cotton price analysis with improved visualizations and statistics"""

    def __init__(self, historical_data_path, annual_data_path):
        """Initialize with data paths"""
        self.historical_data_path = historical_data_path
        self.annual_data_path = annual_data_path
        self.historical_data = None
        self.annual_data = None
        self.model = None

    def load_data(self):
        """Load and preprocess data"""
        print("Loading data...")

        # Load historical data
        self.historical_data = pd.read_csv(self.historical_data_path)
        self.historical_data['date'] = pd.to_datetime(
            self.historical_data['date'])
        self.historical_data = self.historical_data.sort_values('date')

        # Load annual data
        self.annual_data = pd.read_csv(self.annual_data_path)

        # Clean annual data
        self.annual_data['Average Closing Price'] = (
            self.annual_data['Average Closing Price']
            .str.replace('$', '')
            .astype(float)
        )
        self.annual_data['Annual %Change'] = (
            self.annual_data['Annual %Change']
            .str.rstrip('%')
            .astype(float) / 100
        )

        print(f"Loaded {len(self.historical_data)} historical records")
        print(f"Loaded {len(self.annual_data)} annual records")

    def calculate_technical_indicators(self):
        """Calculate technical indicators"""
        print("Calculating technical indicators...")

        # Moving averages
        self.historical_data['MA_30'] = self.historical_data['value'].rolling(
            30).mean()
        self.historical_data['MA_90'] = self.historical_data['value'].rolling(
            90).mean()
        self.historical_data['MA_365'] = self.historical_data['value'].rolling(
            365).mean()

        # Volatility (rolling standard deviation)
        self.historical_data['volatility_30'] = (
            self.historical_data['value'].rolling(30).std()
        )

        # RSI (simplified)
        delta = self.historical_data['value'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(14).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(14).mean()
        rs = gain / loss
        self.historical_data['RSI'] = 100 - (100 / (1 + rs))

    def fit_linear_model(self):
        """Fit linear regression model with diagnostics"""
        print("Fitting linear regression model...")

        # Prepare data
        X = self.annual_data[['Year']].values
        y = self.annual_data['Average Closing Price'].values

        # Fit model
        self.model = LinearRegression()
        self.model.fit(X, y)

        # Predictions
        y_pred = self.model.predict(X)

        # Calculate metrics
        r2 = r2_score(y, y_pred)
        mse = mean_squared_error(y, y_pred)
        rmse = np.sqrt(mse)

        # Statistical tests
        residuals = y - y_pred
        shapiro_stat, shapiro_p = stats.shapiro(residuals)

        print(f"Model R²: {r2:.4f}")
        print(f"RMSE: ${rmse:.4f}")
        print(f"Shapiro-Wilk normality test p-value: {shapiro_p:.4f}")

        return {
            'r2': r2,
            'rmse': rmse,
            'shapiro_p': shapiro_p,
            'predictions': y_pred
        }

    def create_comprehensive_dashboard(self):
        """Create comprehensive interactive dashboard"""
        print("Creating comprehensive dashboard...")

        # Calculate technical indicators
        self.calculate_technical_indicators()

        # Fit model
        model_results = self.fit_linear_model()

        # Create subplots
        fig = make_subplots(
            rows=3, cols=2,
            subplot_titles=[
                'Historical Cotton Prices with Technical Indicators',
                'Annual Price Analysis with Linear Regression',
                'Price Distribution and Statistics',
                'Volatility Analysis',
                'RSI and Momentum Indicators',
                'Model Diagnostics'
            ],
            specs=[[{"secondary_y": True}, {"secondary_y": False}],
                   [{"secondary_y": False}, {"secondary_y": False}],
                   [{"secondary_y": False}, {"secondary_y": False}]]
        )

        # 1. Historical prices with technical indicators
        fig.add_trace(
            go.Scatter(
                x=self.historical_data['date'],
                y=self.historical_data['value'],
                mode='lines',
                name='Cotton Price',
                line=dict(color='blue', width=2)
            ),
            row=1, col=1
        )

        # Moving averages
        fig.add_trace(
            go.Scatter(
                x=self.historical_data['date'],
                y=self.historical_data['MA_30'],
                mode='lines',
                name='MA 30',
                line=dict(color='orange', width=1)
            ),
            row=1, col=1
        )

        fig.add_trace(
            go.Scatter(
                x=self.historical_data['date'],
                y=self.historical_data['MA_90'],
                mode='lines',
                name='MA 90',
                line=dict(color='red', width=1)
            ),
            row=1, col=1
        )

        # 2. Annual analysis with regression
        fig.add_trace(
            go.Scatter(
                x=self.annual_data['Year'],
                y=self.annual_data['Average Closing Price'],
                mode='markers+lines',
                name='Annual Average Price',
                marker=dict(size=8, color='blue')
            ),
            row=1, col=2
        )

        # Regression line
        years_range = np.arange(self.annual_data['Year'].min(),
                                self.annual_data['Year'].max() + 1)
        regression_line = self.model.predict(years_range.reshape(-1, 1))

        fig.add_trace(
            go.Scatter(
                x=years_range,
                y=regression_line,
                mode='lines',
                name=f'Linear Regression (R²={model_results["r2"]:.3f})',
                line=dict(color='red', width=2, dash='dash')
            ),
            row=1, col=2
        )

        # 3. Price distribution
        fig.add_trace(
            go.Histogram(
                x=self.historical_data['value'],
                nbinsx=50,
                name='Price Distribution',
                marker_color='lightblue'
            ),
            row=2, col=1
        )

        # 4. Volatility analysis
        fig.add_trace(
            go.Scatter(
                x=self.historical_data['date'],
                y=self.historical_data['volatility_30'],
                mode='lines',
                name='30-Day Volatility',
                line=dict(color='purple', width=2)
            ),
            row=2, col=2
        )

        # 5. RSI
        fig.add_trace(
            go.Scatter(
                x=self.historical_data['date'],
                y=self.historical_data['RSI'],
                mode='lines',
                name='RSI',
                line=dict(color='green', width=2)
            ),
            row=3, col=1
        )

        # RSI overbought/oversold lines
        fig.add_hline(y=70, line_dash="dash", line_color="red",
                      annotation_text="Overbought (70)", row=3, col=1)
        fig.add_hline(y=30, line_dash="dash", line_color="red",
                      annotation_text="Oversold (30)", row=3, col=1)

        # 6. Model residuals
        residuals = self.annual_data['Average Closing Price'] - \
            model_results['predictions']
        fig.add_trace(
            go.Scatter(
                x=model_results['predictions'],
                y=residuals,
                mode='markers',
                name='Residuals',
                marker=dict(size=8, color='red')
            ),
            row=3, col=2
        )

        # Update layout
        fig.update_layout(
            title={
                'text': 'Comprehensive Cotton Price Analysis Dashboard',
                'x': 0.5,
                'xanchor': 'center',
                'font': {'size': 20}
            },
            height=1200,
            showlegend=True,
            template='plotly_white'
        )

        # Update axes
        fig.update_xaxes(title_text="Date", row=1, col=1)
        fig.update_yaxes(title_text="Price ($)", row=1, col=1)
        fig.update_xaxes(title_text="Year", row=1, col=2)
        fig.update_yaxes(title_text="Price ($)", row=1, col=2)
        fig.update_xaxes(title_text="Price ($)", row=2, col=1)
        fig.update_yaxes(title_text="Frequency", row=2, col=1)
        fig.update_xaxes(title_text="Date", row=2, col=2)
        fig.update_yaxes(title_text="Volatility", row=2, col=2)
        fig.update_xaxes(title_text="Date", row=3, col=1)
        fig.update_yaxes(title_text="RSI", row=3, col=1)
        fig.update_xaxes(title_text="Predicted Price ($)", row=3, col=2)
        fig.update_yaxes(title_text="Residuals", row=3, col=2)

        return fig

    def generate_statistical_summary(self):
        """Generate comprehensive statistical summary"""
        print("Generating statistical summary...")

        # Basic statistics
        basic_stats = {
            'Mean Price': self.historical_data['value'].mean(),
            'Median Price': self.historical_data['value'].median(),
            'Standard Deviation': self.historical_data['value'].std(),
            'Min Price': self.historical_data['value'].min(),
            'Max Price': self.historical_data['value'].max(),
            'Skewness': stats.skew(self.historical_data['value']),
            'Kurtosis': stats.kurtosis(self.historical_data['value'])
        }

        # Volatility statistics
        returns = self.historical_data['value'].pct_change().dropna()
        volatility_stats = {
            'Daily Volatility': returns.std(),
            'Annualized Volatility': returns.std() * np.sqrt(252),
            'Sharpe Ratio': returns.mean() / returns.std() * np.sqrt(252),
            'Max Drawdown': self.calculate_max_drawdown()
        }

        return {
            'basic_statistics': basic_stats,
            'volatility_statistics': volatility_stats,
            'model_performance': self.fit_linear_model()
        }

    def calculate_max_drawdown(self):
        """Calculate maximum drawdown"""
        cumulative = (1 + self.historical_data['value'].pct_change()).cumprod()
        running_max = cumulative.expanding().max()
        drawdown = (cumulative - running_max) / running_max
        return drawdown.min()

    def save_results(self, output_dir='../../results/plots/'):
        """Save all results"""
        import os

        if not os.path.exists(output_dir):
            os.makedirs(output_dir)

        # Save dashboard
        fig = self.create_comprehensive_dashboard()
        fig.write_html(os.path.join(
            output_dir, 'cotton_analysis_dashboard.html'))
        fig.write_image(os.path.join(output_dir, 'cotton_analysis_dashboard.png'),
                        width=1400, height=1200)

        # Save statistical summary
        stats = self.generate_statistical_summary()
        stats_df = pd.DataFrame([
            {**stats['basic_statistics'], **stats['volatility_statistics']}
        ]).T
        stats_df.columns = ['Value']
        stats_df.to_csv(os.path.join(output_dir, 'statistical_summary.csv'))

        print(f"Results saved to {output_dir}")


def main():
    """Main execution function"""
    # Initialize analyzer
    analyzer = CottonPriceAnalyzer(
        '../../data/raw/cotton-prices-historical-chart-data.csv',
        '../../data/raw/Annual_Cotton_CSV.csv'
    )

    # Load data
    analyzer.load_data()

    # Generate and save results
    analyzer.save_results()

    # Show dashboard
    fig = analyzer.create_comprehensive_dashboard()
    fig.show()


if __name__ == "__main__":
    main()
