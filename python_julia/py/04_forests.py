import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import LabelEncoder

# 1- Read clean LWS sample data
file = pd.read_csv("clean_data/us16w.csv")


# Encode categorical variables
categorical_vars = ['marital', 'health_c',
                    'educlev', 'status1', 'ind1_c', 'occ1_c', 'sex']
for var in categorical_vars:
    le = LabelEncoder()
    file[var] = le.fit_transform(file[var])
file["marital"]

X = file[['age', 'sex', 'marital', 'health_c',
          'educlev', 'status1', 'ind1_c', 'occ1_c']]
y = file['saves']

# Define the model
forest = RandomForestClassifier(
    n_estimators=100, max_features=5, random_state=42)
forest.fit(X, y)

# Calculate and print the relative importance of the variables
importances = forest.feature_importances_
rel_imp = 100 * (importances / max(importances))
sorted_idx = rel_imp.argsort()[::-1]
rel_imp = rel_imp[sorted_idx]
features = X.columns[sorted_idx]

rel_imp_df = pd.DataFrame(
    {'Feature': features, 'Relative Importance': rel_imp})
print(rel_imp_df)
