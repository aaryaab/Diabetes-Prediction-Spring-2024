# Diabetes Prediction Project

## Overview
This project aims to predict diabetes using behavioral and lifestyle data. The dataset used for this project is the 2015 CDC Diabetes Health Indicators dataset, created by the Centers for Disease Control through their annual Behavioral Risk Factor Surveillance System (BRFSS). The project was completed as part of the ISTM 650 Business Data Mining course. Reference Link: https://archive.ics.uci.edu/dataset/891/cdc+diabetes+health+indicators

## Dataset
The dataset is collected from the BRFSS, which conducts an annual telephone survey to gather health-related data from over 400,000 Americans. The project includes two datasets:

- **Unbalanced Dataset**: 253,680 rows and 22 columns
- **Balanced Dataset**: 70,693 rows and 22 columns

Both datasets include one outcome column indicating the presence of diabetes.

## Files
- `diabetes4.r`: Final file containing the trained Decision Tree model.
- `diabetes.r`, `diabetes2.r`, `diabetes2_indivi.r`,`diabetes3_try.r`: Files documenting the work and analysis done prior to the final model.

## Project Structure
- `data/`: Directory containing the balanced and unbalanced datasets.
- `scripts/`: Directory containing the R scripts used for data analysis and model training.
  - `diabetes.r`: Initial analysis and model development.
  - `diabetes2.r`: Further refinement and experimentation.
  - `diabetes2_indivi.r`: Individual analysis and contributions.
  - `diabetes4.r`: Final model training and evaluation.
- `models/`: Directory to save trained models and related outputs.
- `results/`: Directory to store results, including plots and evaluation metrics.

## Model
The primary model used for prediction is a Decision Tree, trained on both the balanced and unbalanced datasets to ensure comprehensive evaluation. During the course of the semester, Decision Tree, Naive Bayes & Convolutional Neural Network model was trained for this project.

## Usage
1. Clone the repository:
   ```sh
   git clone https://github.com/yourusername/diabetes-prediction.git
