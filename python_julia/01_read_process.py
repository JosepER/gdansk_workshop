# .\env_gdansk\Scripts\activate
import pandas as pd

# 1- read .dta datasets in /data
file_h = pd.read_stata("data/it14ih.dta")
file_p = pd.read_stata("data/it14ip.dta")

# 2 - subset variables
file_h.columns.values  # list of variables
file_h = file_h[["hid", "hilabour", "nhhmem", "hpopwgt"]]

file_p.columns.values  # list of variables
file_p = file_p[["hid", "pid", "pilabour", "sex", "age", "marital",
                 "disabled", "educlev", "lfs", "status1", "ind1_c", "occ1_c"]]

# 3- merge datasets
file = file_p.merge(file_h, on="hid", how="left")

# 4- Arrange the data
file = file[(file["age"] >= 30) & (file["age"] <= 60) & (file["pilabour"] > 0)]

# * print number of NAs in each variable
file.shape
file.isnull().sum()

# * delete rows with NAs
file = file.dropna()

# write file to csv
file.to_csv("clean_data/it14i.csv", index=False)
