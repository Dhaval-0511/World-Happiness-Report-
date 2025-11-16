🌍 World Happiness Report Data Analysis in R
📊 Overview
This project provides a comprehensive, reproducible analysis of global happiness trends using historical data from the World Happiness Report. Leveraging R, the workflow covers data cleaning, transformation, exploratory data analysis, impactful visualizations, and predictive modeling.

The repository enables users to analyze how various socio-economic factors influence happiness scores across countries and years.

🚀 Features
Data cleaning: Handle missing values and duplicates

Data transformation: Reshape wide-format annual country reports to a tidy long format

Exploratory Data Analysis: Average happiness by year, top happiest countries

Visualization: Trend lines, bar charts, and country comparison plots

Predictive Modeling: Linear and polynomial regression models to estimate happiness scores from key factors

🗂️ Project Structure
text
.
├── RFinal.R                  # Main analysis script
├── RP.R                      # Alternative/Extended analysis script
├── R-Final.txt               # Workflow and code documentation
├── R-code.txt                # Stepwise code and explanations
├── R-complete.txt            # Full project example walkthrough
├── world-happiness-report.xlsx    # Raw data file (Excel)
├── world-happiness-report-clean.csv # Cleaned & processed data (CSV)
📚 Dataset
The primary data source is the World Happiness Report, which aggregates country-level happiness scores and related socio-economic variables (GDP per capita, social support, health, freedom to make life choices, generosity, perceptions of corruption).

🧰 Tech Stack
Language: R

Main Libraries: readxl, dplyr, tidyr, ggplot2, caret

📝 Usage Guide
Clone the Repo
git clone <your-repository-url>

Set Working Directory
Place all files in your R working directory.

Install Dependencies
Use:

r
install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "caret"))
Run Analysis
Open RFinal.R or RP.R in RGui/RStudio and run the script line by line or using source().

Visualize Trends and Results
Plots and model summaries are generated automatically.

💡 Key Steps Explained
Data Loading: Import world happiness data and inspect structure.

Cleaning: Address missing values (median/mode), remove duplicates.

Reshape: Convert data for year-wise and country-wise comparison.

EDA & Visualization: Show global trends and highlight happiest countries.

Modeling: Train and compare linear and polynomial regression for happiness prediction.

Diagnostics: Visualize model accuracy, actual vs predicted, and residuals.

🎯 Project Goals
Uncover insights into global happiness patterns

Facilitate reproducible, flexible happiness data modeling

Enable users to extend analyses or apply models to new years/countries

🧑‍💻 Contributions
Pull requests and issues are welcome for improvements, fixes, or new features!

📄 License
Open-source, MIT License.
