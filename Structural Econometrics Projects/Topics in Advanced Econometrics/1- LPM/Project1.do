clear

import delimited "C:\Users\moham\My Drive\Course\Structural Econometrics Projects\Topics in Advanced Econometrics\LPM\commute_binary.csv"

generate output = 0
replace output = 1 if mode == "car"

reg output timecar costcar timebus, vce(robust)

predict predicted_output
sort predicted_output

generate counter = _n

twoway line predicted_output counter

generate Marital = 0
replace Marital = 1 if marital_status == "married"

reg output timecar costcar timebus income Marital, vce(robust)