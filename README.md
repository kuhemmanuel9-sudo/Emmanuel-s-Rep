
# Credit Default Prediction Using Machine Learning

## Overview

This project develops machine learning models to predict customer credit default using financial and demographic characteristics. The objective is to compare traditional statistical learning methods with neural networks and evaluate their performance using multiple classification metrics.

The project demonstrates an end-to-end machine learning workflow, including data preprocessing, exploratory data analysis, feature engineering, model development, hyperparameter tuning, model evaluation, and business interpretation.

---

## Dataset

**Dataset:** Default Dataset

### Features

- Account Balance
- Income
- Student Status

### Target Variable

- Default (Yes / No)

---

## Objectives

- Predict customer credit default.
- Compare Logistic Regression and Neural Network classifiers.
- Evaluate predictive performance using multiple metrics.
- Identify key factors associated with default risk.
- Provide business insights for credit risk management.

---

## Machine Learning Workflow

- Data Cleaning
- Exploratory Data Analysis (EDA)
- Feature Engineering
- Train-Test Split
- Feature Standardization
- Logistic Regression
- Neural Network (MLPClassifier)
- Hyperparameter Optimization (GridSearchCV)
- Model Comparison
- Business Interpretation

---

## Evaluation Metrics

Models were evaluated using:

- Accuracy
- Precision
- Recall
- F1-Score
- ROC-AUC
- Confusion Matrix
- Precision-Recall Curve

---

## Technologies

- Python
- Pandas
- NumPy
- Matplotlib
- Seaborn
- Scikit-learn

---

## Results

- Logistic Regression achieved excellent predictive performance while remaining highly interpretable.
- Neural Network produced similar overall classification accuracy.
- Account balance was the strongest predictor of default risk.
- Because default cases are relatively rare, precision, recall, and ROC-AUC provided more informative performance measures than accuracy alone.

---

## Business Insights

- Customers with higher account balances were substantially more likely to default.
- Income alone was a weaker predictor than account balance.
- Credit default prediction is an imbalanced classification problem.
- Financial institutions should evaluate recall and ROC-AUC in addition to overall accuracy when identifying high-risk customers.
- Future improvements could include SMOTE, cost-sensitive learning, and probability threshold optimization.

---

## Repository Structure

```
Credit-Default-Prediction
│
├── credit_default_prediction.py
├── README.md
├── requirements.txt
└── figures
```

---

## Future Improvements

- Random Forest
- XGBoost
- LightGBM
- SHAP Explainability
- Feature Importance Analysis
- Model Deployment using Streamlit
- Interactive Dashboard (Power BI)

---

## Author

**Emmanuel Kuh**

M.S. Econometrics & Quantitative Economics

Towson University

LinkedIn:
https://www.linkedin.com/in/emmanuel-kuh45

GitHub:
https://github.com/kuhemmanuel9-sudo