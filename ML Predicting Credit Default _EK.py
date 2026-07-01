
# Credit Default Prediction Using Machine Learning

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LogisticRegression
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import (
    classification_report,
    confusion_matrix,
    ConfusionMatrixDisplay,
    roc_auc_score,
    RocCurveDisplay,
    PrecisionRecallDisplay,
    accuracy_score
)

# 1. Load data
df = pd.read_csv("C:/Users/JB/Downloads/Default.csv")

print(df.head())
print(df.info())
print(df["default"].value_counts(normalize=True))

# 2. Prepare features and target
X = df[["balance", "income"]].copy()
X["student"] = (df["student"] == "Yes").astype(int)

y = (df["default"] == "Yes").astype(int)

# 3. Train-test split
X_train, X_test, y_train, y_test = train_test_split(
    X, y,
    test_size=0.30,
    random_state=42,
    stratify=y
)

# 4. Logistic Regression
logit_pipe = Pipeline([
    ("scaler", StandardScaler()),
    ("model", LogisticRegression(max_iter=1000, class_weight="balanced"))
])

logit_pipe.fit(X_train, y_train)
logit_pred = logit_pipe.predict(X_test)
logit_prob = logit_pipe.predict_proba(X_test)[:, 1]

print("\n==============================")
print("LOGISTIC REGRESSION")
print("==============================")
print("Accuracy:", accuracy_score(y_test, logit_pred))
print("ROC-AUC:", roc_auc_score(y_test, logit_prob))
print(classification_report(y_test, logit_pred))

# 5. Neural Network
nn_pipe = Pipeline([
    ("scaler", StandardScaler()),
    ("model", MLPClassifier(
        hidden_layer_sizes=(10,),
        activation="relu",
        solver="adam",
        alpha=0.01,
        max_iter=1000,
        random_state=42
    ))
])

nn_pipe.fit(X_train, y_train)
nn_pred = nn_pipe.predict(X_test)
nn_prob = nn_pipe.predict_proba(X_test)[:, 1]

print("\n==============================")
print("NEURAL NETWORK")
print("==============================")
print("Accuracy:", accuracy_score(y_test, nn_pred))
print("ROC-AUC:", roc_auc_score(y_test, nn_prob))
print(classification_report(y_test, nn_pred))

# 6. Model comparison table
results = pd.DataFrame({
    "Model": ["Logistic Regression", "Neural Network"],
    "Accuracy": [
        accuracy_score(y_test, logit_pred),
        accuracy_score(y_test, nn_pred)
    ],
    "ROC_AUC": [
        roc_auc_score(y_test, logit_prob),
        roc_auc_score(y_test, nn_prob)
    ]
})

print("\n==============================")
print("MODEL COMPARISON")
print("==============================")
print(results)

# 7. Confusion matrices
for name, pred in [
    ("Logistic Regression", logit_pred),
    ("Neural Network", nn_pred)
]:
    cm = confusion_matrix(y_test, pred)
    disp = ConfusionMatrixDisplay(confusion_matrix=cm)
    disp.plot()
    plt.title(f"Confusion Matrix - {name}")
    plt.show()

# 8. ROC curves
RocCurveDisplay.from_estimator(logit_pipe, X_test, y_test)
plt.title("ROC Curve - Logistic Regression")
plt.show()

RocCurveDisplay.from_estimator(nn_pipe, X_test, y_test)
plt.title("ROC Curve - Neural Network")
plt.show()

# 9. Precision-recall curves
PrecisionRecallDisplay.from_estimator(logit_pipe, X_test, y_test)
plt.title("Precision-Recall Curve - Logistic Regression")
plt.show()

PrecisionRecallDisplay.from_estimator(nn_pipe, X_test, y_test)
plt.title("Precision-Recall Curve - Neural Network")
plt.show()

# 10. Interpretation
print("\n==============================")
print("INTERPRETATION")
print("==============================")
print(
    "This project compares Logistic Regression and Neural Network models "
    "for credit default prediction. Because default cases are rare, accuracy "
    "alone is not sufficient. Precision, recall, F1-score, ROC-AUC, confusion "
    "matrices, and precision-recall curves provide a more complete evaluation. "
    "The results show that both models perform well overall, but class imbalance "
    "makes default detection more difficult."
)