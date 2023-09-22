import pandas as pd
from sklearn.model_selection import GridSearchCV
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score

# let's do the same as in 01_read_process.py, but putting it into a function


def read_and_clean_lws_data(ccyy, path_to_data="data/"):
    """Read and clean LWS sample data files

    Args:
        ccyyd (str): The file name in 'ccyy' format and 'yy' is the year.
        path_to_data (str, optional): _description_. Defaults to "data/".
    """

    print(f"Reading and cleaning {ccyy} data...")

    # 1- Read and clean LWS sample data
    file_h = pd.read_stata("data/us16wh.dta")
    file_p = pd.read_stata("data/us16wp.dta")

    # 2 - subset variables
    # * subset variables for file_h
    file_h = file_h[["hid", "hpopwgt", "hhtype", "inum"]]

    # * subset variables for file_p
    file_p = file_p[["hid", "pid", "sex", "relation", "inum", "age", "marital",
                    "health_c", "educlev", "status1", "ind1_c", "occ1_c", "basb", "bafr1_c"]]

    # 3- filter rows
    file_h = file_h[(file_h["inum"] == 1)]

    file_p = file_p[(file_p["inum"] == 1) & (
        file_p["age"] >= 25) & (file_p["age"] <= 75) & (file_p["relation"] == "[1000]head")]

    # 4- merge datasets
    file = file_p.merge(file_h, on="hid", how="left")

    # 5- delete rows with NAs
    file = file.dropna()

    # 6- compute 'saves' variable
    file["saves"] = [int(x) for x in file["basb"] == "[20]saves"]

    print(
        f"The processed file has {file.shape[0]} observations in the dataset.")

    return file


us16w = read_and_clean_lws_data("us16w")

# create dummy variables for 'educlev'
us16w['educlev'] = us16w['educlev'].astype('category')
# One-hot encode 'educlev'
X = pd.get_dummies(us16w['educlev'], drop_first=True)
y = us16w['saves']


# Define grid of (1-alpha) used to tune the algorithm.
param_grid = {'min_impurity_decrease': [i/1000 for i in range(900, 1000, 5)]}

# Set up the cross-validation and grid search
model = DecisionTreeClassifier(
    min_samples_split=300, min_samples_leaf=100, max_depth=6)
clf = GridSearchCV(model, param_grid, cv=5, verbose=0)
clf.fit(X, y)

# Get the results
min_criterion = round(clf.best_params_['min_impurity_decrease'], 2)
acc = round(clf.best_score_, 2)


print(
    f"The Accuracy of this tree, with an alpha of {1 - min_criterion}, is: {acc}")

# Train the final model with the best parameters
final_model = DecisionTreeClassifier(
    min_samples_split=300, min_samples_leaf=100, max_depth=6, min_impurity_decrease=min_criterion)
final_model.fit(X, y)

# Predict income and groups
us16w['y_tilde'] = final_model.predict(X)
us16w['groups'] = final_model.apply(X)

us16w['saves'].value_counts()  # 0: 229, 1: 442
us16w['y_tilde'].value_counts()  # all predicted to 1
print(us16w['groups'].value_counts())

us16w.to_csv("clean_data/us16w.csv", index=False)
