clear

import delimited "C:\Users\moham\My Drive\Projects\Structural Econometrics Projects\Topics in Advanced Econometrics\2- Logit\commute_binary.csv"

gen output = (mode == "car")
gen Married = (marital_status == "married")

mlogit output timecar costcar timebus, vce(robust)