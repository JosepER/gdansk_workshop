import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import RepeatedKFold
from sklearn.linear_model import LassoCV, Lasso


# 1- Read clean LIS sample data
file = pd.read_csv("clean_data/it14i.csv")

# 2- Apply Lasso with Cross Validation
# dependent variable: pilabour
# independent variables: sex, marital
# 2.1- Subset columns
file = file[["pilabour", "sex", "marital"]]

# 2.2- Split into input and output elements
# Convert categorical variables to dummy variables
X = pd.get_dummies(file[['sex', 'marital']], drop_first=True)
y = file.values[:, 0]

# 2.3- Define model
# Grid of lambdas to try
lambda_try = [30**(x / 100.0) for x in range(1, 200, 10)]

# Initialize Lasso model
model = LassoCV(alphas=lambda_try, cv=5, verbose=True)

# 3. Fit the model
model.fit(X, y)

# Perform cross-validation and calculate RMSE for each lambda
mse_means = []
for alpha in lambda_try:
    model.alpha = alpha
    scores = cross_val_score(
        model, X, y, cv=5, scoring='neg_mean_squared_error')
    mse_means.append(-scores.mean())


# Plotting
plt.figure(figsize=(10, 6))
plt.plot(lambda_try, mse_means, marker='o')
plt.xscale('log')
plt.xlabel('Lambda (Log Scale)')
plt.ylabel('Mean Squared Error')
plt.title('Mean Squared Error for different Lambda values')
plt.grid(True)
plt.show()

# Calculate RMSE for the optimal lambda
optimal_lambda = model.alpha_
rmse = np.sqrt(min(mse_means))
print(
    f"The RMSE of this model, with an optimal lambda of {optimal_lambda}, is: {round(rmse, 2)}")

# Display a model with a given lambda

model_with_desired_lambda = Lasso(alpha=optimal_lambda)

# Fit the model with the desired lambda
model_with_desired_lambda.fit(X, y)

# Get the coefficients
coefficients = model_with_desired_lambda.coef_

# Display the coefficients
for feature, coef in zip(X.columns, coefficients):
    print(f"{feature}: {coef}")
