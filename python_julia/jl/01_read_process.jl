using DataFrames, StatFiles

# Read data
current_dir = dirname(@__FILE__)
parent_dir = dirname(current_dir)

df_h = DataFrame(load(parent_dir * "/data/it14ih.dta"))
df_p = DataFrame(load(parent_dir * "/data/it14ip.dta"))

# Subset variables
df_h = df_h[:,["hid", "hilabour", "nhhmem", "hpopwgt"]]
df_p = df_p[:,["hid", "pid", "pilabour", "sex", "age", "marital", "disabled", 
    "educlev", "lfs", "status1", "ind1_c", "occ1_c"]]

# Merge df_h and df_p
df = leftjoin(df_h, df_p, on = :hid)

# Filter rows
df = df[(df[:,:age] .>= 30) .& (df[:,:age] .<= 60) .& (df[:,:pilabour] .> 0), :]

# * delete rows with NAs
df = dropmissing(df)