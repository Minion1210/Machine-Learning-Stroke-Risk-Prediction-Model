# Machine-Learning-Stroke-Risk-Prediction-Model

Objective: To establish a predictive model to effectively assess stroke risk, helping patients prevent stroke in advance (e.g., adjusting diet, controlling hypertension, hyperlipidemia, and hyperglycemia), thereby reducing medical expenses and economic burden after the onset of stroke.

1. Key Input Variables:
* Physiological Indicators: Age, gender, history of hypertension, history of heart disease, average blood glucose level, BMI.
* Lifestyle: Marital status, occupation, living environment (urban/suburban), smoking habits.
* Predictive Output: Stroke status (1: Yes / 0: No).

2. Data Preprocessing
* Missing Value Handling: For missing BMI values, imputation is performed using the mean, and the data is visualized.
* Variable Transformation: Categorical data (e.g., gender, occupation, smoking habits) are converted into numerical codes.

3. Research Methods and Model Evaluation
* Validation Mechanism: An 80/20 split is used, with 80% of the data used for model training and 20% for testing and validation.

The report compares several machine learning algorithms, including:

Logistic Regression

Support Vector Machine (SVM)

K-Nearest Neighbors (KNN)

Decision Tree

Analogous Neural Network (ANN)

4. Research Results and Findings

Model Performance: In the tests of various models, ANN (Analogous Neural Network) demonstrated good predictive ability on the test set.

Key Influencing Factors: According to model analysis age, hypertension, and average blood glucose levels are the most important factors for predicting stroke risk.

Confusion Matrix and ROC Curve: The report comprehensively evaluates the performance of each model through the confusion matrix, precision, recall, and AUC curve.

5. Conclusions and Application Value

Technical Conclusion: Big data and machine learning can effectively identify high-risk groups for stroke.
