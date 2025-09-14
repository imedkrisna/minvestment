cd "G:\My Drive\IP"
use "G:\My Drive\IP\wbes\Indonesia-2023-full-data.dta",clear

// filter big and foreign
// ownership b2a=dom, b2b=for, b2c=gov
//b6 no of people
//K6 checking account
gen foreign=0
replace foreign=1 if b2b>10
gen large=0
replace large=1 if b6>=100
gen kind="lf"
replace kind="sf" if foreign==1 & large==0
replace kind="sd" if foreign==0 & large==0
replace kind="ld" if foreign==0 & large==1

/* ijin
	c4 listrik
	c13 air
	c33 cost to get air
	G3 construction-related permit, G4 corruption
	K14a-e collateral requirement
	G30 access to land as obstacle
 biz obstacles
	j30a tax rates
	j30b tax administration
	j30c business licensing and permits
	j30e political instability
	j30f corruption
	h30 Courts
	m1a biggest obstacles
*/

tab2xl m1a using "lf.xlsx" if kind=="lf",col(1) row(1) replace

tab2xl m1a using "ld.xlsx" if kind=="ld",col(1) row(1) replace

tab2xl m1a using "sf.xlsx" if kind=="sf",col(1) row(1) replace

tab2xl m1a using "sd.xlsx" if kind=="sd",col(1) row(1) replace
save data.dta,replace
keep if kind=="lf"


putexcel set sumlf,replace
putexcel A1 = "`v'"
putexcel B1 = "Mean"
putexcel D1 = "Std. Dev."
putexcel C1 = "Median"
putexcel E1 = "n"
putexcel F1 = "min"
putexcel G1 = "max"


sum c4 if c4>=0,d
return list
putexcel A2 = "c4"
putexcel B2 = `r(mean)', nformat(number_d2)
putexcel C2 = `r(p50)', nformat(number_d2)
putexcel D2 = `r(sd)', nformat(number_d2)
putexcel E2 = `r(N)', nformat(number_d2)
putexcel F2 = `r(min)',nformat(number_d2)
putexcel G2 = `r(max)',nformat(number_d2)
sum c13 if c13>=0,d
return list
putexcel A3 = "c13"
putexcel B3 = `r(mean)', nformat(number_d2)
putexcel C3 = `r(p50)', nformat(number_d2)
putexcel D3 = `r(sd)', nformat(number_d2)
putexcel E3 = `r(N)', nformat(number_d2)
putexcel F3 = `r(min)',nformat(number_d2)
putexcel G3 = `r(max)',nformat(number_d2)
/*sum c33 if c33>=0,d
return list
putexcel A4 = "c33"
putexcel B4 = `r(mean)', nformat(number_d2)
putexcel C4 = `r(p50)', nformat(number_d2)
putexcel D4 = `r(sd)', nformat(number_d2)
putexcel E4 = `r(N)', nformat(number_d2)
*/
sum g3 if g3>=0,d
return list
putexcel A5 = "g3"
putexcel B5 = `r(mean)', nformat(number_d2)
putexcel C5 = `r(p50)', nformat(number_d2)
putexcel D5 = `r(sd)', nformat(number_d2)
putexcel E5 = `r(N)', nformat(number_d2)
putexcel F5 = `r(min)',nformat(number_d2)
putexcel G5 = `r(max)',nformat(number_d2)
sum g30 if g30>=0,d
return list
putexcel A6 = "g30"
putexcel B6 = `r(mean)', nformat(number_d2)
putexcel C6 = `r(p50)', nformat(number_d2)
putexcel D6 = `r(sd)', nformat(number_d2)
putexcel E6 = `r(N)', nformat(number_d2)
putexcel F6 = `r(min)',nformat(number_d2)
putexcel G6 = `r(max)',nformat(number_d2)
sum j30a if j30a>=0,d
return list
putexcel A7 = "j30a"
putexcel B7 = `r(mean)', nformat(number_d2)
putexcel C7 = `r(p50)', nformat(number_d2)
putexcel D7 = `r(sd)', nformat(number_d2)
putexcel E7 = `r(N)', nformat(number_d2)
putexcel F7 = `r(min)',nformat(number_d2)
putexcel G7 = `r(max)',nformat(number_d2)
sum j30b if j30b>=0,d
return list
putexcel A8 = "j30b"
putexcel B8 = `r(mean)', nformat(number_d2)
putexcel C8 = `r(p50)', nformat(number_d2)
putexcel D8 = `r(sd)', nformat(number_d2)
putexcel E8 = `r(N)', nformat(number_d2)
putexcel F8 = `r(min)',nformat(number_d2)
putexcel G8 = `r(max)',nformat(number_d2)
sum j30c if j30c>=0,d
return list
putexcel A9 = "j30c"
putexcel B9 = `r(mean)', nformat(number_d2)
putexcel C9 = `r(p50)', nformat(number_d2)
putexcel D9 = `r(sd)', nformat(number_d2)
putexcel E9 = `r(N)', nformat(number_d2)
putexcel F8 = `r(min)',nformat(number_d2)
putexcel G8 = `r(max)',nformat(number_d2)

sum j30e if j30e>=0,d
return list
putexcel A10 = "j30e"
putexcel B10 = `r(mean)', nformat(number_d2)
putexcel C10 = `r(p50)', nformat(number_d2)
putexcel D10 = `r(sd)', nformat(number_d2)
putexcel E10 = `r(N)', nformat(number_d2)
putexcel F10 = `r(min)',nformat(number_d2)
putexcel G10 = `r(max)',nformat(number_d2)
sum j30f if j30f>=0,d
return list
putexcel A11 = "j30f"
putexcel B11 = `r(mean)', nformat(number_d2)
putexcel C11 = `r(p50)', nformat(number_d2)
putexcel D11 = `r(sd)', nformat(number_d2)
putexcel E11 = `r(N)', nformat(number_d2)
putexcel F11 = `r(min)',nformat(number_d2)
putexcel G11 = `r(max)',nformat(number_d2)
sum h30 if h30>=0,d
return list
putexcel A12 = "h30a"
putexcel B12 = `r(mean)', nformat(number_d2)
putexcel C12 = `r(p50)', nformat(number_d2)
putexcel D12 = `r(sd)', nformat(number_d2)
putexcel E12 = `r(N)', nformat(number_d2)
putexcel F12 = `r(min)',nformat(number_d2)
putexcel G12 = `r(max)',nformat(number_d2)

putexcel save


use data.dta,clear
keep if kind=="ld"
putexcel set sumld,replace
putexcel A1 = "`v'"
putexcel B1 = "Mean"
putexcel D1 = "Std. Dev."
putexcel C1 = "Median"
putexcel E1 = "n"


sum c4 if c4>=0,d
return list
putexcel A2 = "c4"
putexcel B2 = `r(mean)', nformat(number_d2)
putexcel C2 = `r(p50)', nformat(number_d2)
putexcel D2 = `r(sd)', nformat(number_d2)
putexcel E2 = `r(N)', nformat(number_d2)
putexcel F2 = `r(min)',nformat(number_d2)
putexcel G2 = `r(max)',nformat(number_d2)
sum c13 if c13>=0,d
return list
putexcel A3 = "c13"
putexcel B3 = `r(mean)', nformat(number_d2)
putexcel C3 = `r(p50)', nformat(number_d2)
putexcel D3 = `r(sd)', nformat(number_d2)
putexcel E3 = `r(N)', nformat(number_d2)
putexcel F3 = `r(min)',nformat(number_d2)
putexcel G3 = `r(max)',nformat(number_d2)
sum c33 if c33>=0,d
return list
putexcel A4 = "c33"
putexcel B4 = `r(mean)', nformat(number_d2)
putexcel C4 = `r(p50)', nformat(number_d2)
putexcel D4 = `r(sd)', nformat(number_d2)
putexcel E4 = `r(N)', nformat(number_d2)
putexcel F4 = `r(min)',nformat(number_d2)
putexcel G4 = `r(max)',nformat(number_d2)

sum g3 if g3>=0,d
return list
putexcel A5 = "g3"
putexcel B5 = `r(mean)', nformat(number_d2)
putexcel C5 = `r(p50)', nformat(number_d2)
putexcel D5 = `r(sd)', nformat(number_d2)
putexcel E5 = `r(N)', nformat(number_d2)
putexcel F5 = `r(min)',nformat(number_d2)
putexcel G5 = `r(max)',nformat(number_d2)
sum g30 if g30>=0,d
return list
putexcel A6 = "g30"
putexcel B6 = `r(mean)', nformat(number_d2)
putexcel C6 = `r(p50)', nformat(number_d2)
putexcel D6 = `r(sd)', nformat(number_d2)
putexcel E6 = `r(N)', nformat(number_d2)
putexcel F6 = `r(min)',nformat(number_d2)
putexcel G6 = `r(max)',nformat(number_d2)
sum j30a if j30a>=0,d
return list
putexcel A7 = "j30a"
putexcel B7 = `r(mean)', nformat(number_d2)
putexcel C7 = `r(p50)', nformat(number_d2)
putexcel D7 = `r(sd)', nformat(number_d2)
putexcel E7 = `r(N)', nformat(number_d2)
putexcel F7 = `r(min)',nformat(number_d2)
putexcel G8 = `r(max)',nformat(number_d2)
sum j30b if j30b>=0,d
return list
putexcel A8 = "j30b"
putexcel B8 = `r(mean)', nformat(number_d2)
putexcel C8 = `r(p50)', nformat(number_d2)
putexcel D8 = `r(sd)', nformat(number_d2)
putexcel E8 = `r(N)', nformat(number_d2)
putexcel F8 = `r(min)',nformat(number_d2)
putexcel G8 = `r(max)',nformat(number_d2)
sum j30c if j30c>=0,d
return list
putexcel A9 = "j30c"
putexcel B9 = `r(mean)', nformat(number_d2)
putexcel C9 = `r(p50)', nformat(number_d2)
putexcel D9 = `r(sd)', nformat(number_d2)
putexcel E9 = `r(N)', nformat(number_d2)
putexcel F9 = `r(min)',nformat(number_d2)
putexcel G9 = `r(max)',nformat(number_d2)

sum j30e if j30e>=0,d
return list
putexcel A10 = "j30e"
putexcel B10 = `r(mean)', nformat(number_d2)
putexcel C10 = `r(p50)', nformat(number_d2)
putexcel D10 = `r(sd)', nformat(number_d2)
putexcel E10 = `r(N)', nformat(number_d2)
putexcel F10 = `r(min)',nformat(number_d2)
putexcel G10 = `r(max)',nformat(number_d2)
sum j30f if j30f>=0,d
return list
putexcel A11 = "j30f"
putexcel B11 = `r(mean)', nformat(number_d2)
putexcel C11 = `r(p50)', nformat(number_d2)
putexcel D11 = `r(sd)', nformat(number_d2)
putexcel E11 = `r(N)', nformat(number_d2)
putexcel F11 = `r(min)',nformat(number_d2)
putexcel G11 = `r(max)',nformat(number_d2)
sum h30 if h30>=0,d
return list
putexcel A12 = "h30a"
putexcel B12 = `r(mean)', nformat(number_d2)
putexcel C12 = `r(p50)', nformat(number_d2)
putexcel D12 = `r(sd)', nformat(number_d2)
putexcel E12 = `r(N)', nformat(number_d2)
putexcel F12 = `r(min)',nformat(number_d2)
putexcel G12 = `r(max)',nformat(number_d2)


putexcel save


use data.dta,clear
keep if kind=="sd"
putexcel set sumsd,replace
putexcel A1 = "`v'"
putexcel B1 = "Mean"
putexcel D1 = "Std. Dev."
putexcel C1 = "Median"
putexcel E1 = "n"


sum c4 if c4>=0,d
return list
putexcel A2 = "c4"
putexcel B2 = `r(mean)', nformat(number_d2)
putexcel C2 = `r(p50)', nformat(number_d2)
putexcel D2 = `r(sd)', nformat(number_d2)
putexcel E2 = `r(N)', nformat(number_d2)
putexcel F2 = `r(min)',nformat(number_d2)
putexcel G2 = `r(max)',nformat(number_d2)
sum c13 if c13>=0,d
return list
putexcel A3 = "c13"
putexcel B3 = `r(mean)', nformat(number_d2)
putexcel C3 = `r(p50)', nformat(number_d2)
putexcel D3 = `r(sd)', nformat(number_d2)
putexcel E3 = `r(N)', nformat(number_d2)
putexcel F3 = `r(min)',nformat(number_d2)
putexcel G3 = `r(max)',nformat(number_d2)
sum c33 if c33>=0,d
return list
putexcel A4 = "c33"
putexcel B4 = `r(mean)', nformat(number_d2)
putexcel C4 = `r(p50)', nformat(number_d2)
putexcel D4 = `r(sd)', nformat(number_d2)
putexcel E4 = `r(N)', nformat(number_d2)
putexcel F4 = `r(min)',nformat(number_d2)
putexcel G4 = `r(max)',nformat(number_d2)
sum g3 if g3>=0,d
return list
putexcel A5 = "g3"
putexcel B5 = `r(mean)', nformat(number_d2)
putexcel C5 = `r(p50)', nformat(number_d2)
putexcel D5 = `r(sd)', nformat(number_d2)
putexcel E5 = `r(N)', nformat(number_d2)
putexcel F5 = `r(min)',nformat(number_d2)
putexcel G5 = `r(max)',nformat(number_d2)
sum g30 if g30>=0,d
return list
putexcel A6 = "g30"
putexcel B6 = `r(mean)', nformat(number_d2)
putexcel C6 = `r(p50)', nformat(number_d2)
putexcel D6 = `r(sd)', nformat(number_d2)
putexcel E6 = `r(N)', nformat(number_d2)
putexcel F6 = `r(min)',nformat(number_d2)
putexcel G6 = `r(max)',nformat(number_d2)
sum j30a if j30a>=0,d
return list
putexcel A7 = "j30a"
putexcel B7 = `r(mean)', nformat(number_d2)
putexcel C7 = `r(p50)', nformat(number_d2)
putexcel D7 = `r(sd)', nformat(number_d2)
putexcel E7 = `r(N)', nformat(number_d2)
putexcel F7 = `r(min)',nformat(number_d2)
putexcel G7 = `r(max)',nformat(number_d2)
sum j30b if j30b>=0,d
return list
putexcel A8 = "j30b"
putexcel B8 = `r(mean)', nformat(number_d2)
putexcel C8 = `r(p50)', nformat(number_d2)
putexcel D8 = `r(sd)', nformat(number_d2)
putexcel E8 = `r(N)', nformat(number_d2)
putexcel F8 = `r(min)',nformat(number_d2)
putexcel G8 = `r(max)',nformat(number_d2)
sum j30c if j30c>=0,d
return list
putexcel A9 = "j30c"
putexcel B9 = `r(mean)', nformat(number_d2)
putexcel C9 = `r(p50)', nformat(number_d2)
putexcel D9 = `r(sd)', nformat(number_d2)
putexcel E9 = `r(N)', nformat(number_d2)
putexcel F9 = `r(min)',nformat(number_d2)
putexcel G9 = `r(max)',nformat(number_d2)

sum j30e if j30e>=0,d
return list
putexcel A10 = "j30e"
putexcel B10 = `r(mean)', nformat(number_d2)
putexcel C10 = `r(p50)', nformat(number_d2)
putexcel D10 = `r(sd)', nformat(number_d2)
putexcel E10 = `r(N)', nformat(number_d2)
putexcel F10 = `r(min)',nformat(number_d2)
putexcel G10 = `r(max)',nformat(number_d2)
sum j30f if j30f>=0,d
return list
putexcel A11 = "j30f"
putexcel B11 = `r(mean)', nformat(number_d2)
putexcel C11 = `r(p50)', nformat(number_d2)
putexcel D11 = `r(sd)', nformat(number_d2)
putexcel E11 = `r(N)', nformat(number_d2)
putexcel F11 = `r(min)',nformat(number_d2)
putexcel G11 = `r(max)',nformat(number_d2)
sum h30 if h30>=0,d
return list
putexcel A12 = "h30a"
putexcel B12 = `r(mean)', nformat(number_d2)
putexcel C12 = `r(p50)', nformat(number_d2)
putexcel D12 = `r(sd)', nformat(number_d2)
putexcel E12 = `r(N)', nformat(number_d2)
putexcel F12 = `r(min)',nformat(number_d2)
putexcel G12 = `r(max)',nformat(number_d2)


putexcel save


use data.dta,clear
keep if kind=="sf"
putexcel set sumsf,replace
putexcel A1 = "`v'"
putexcel B1 = "Mean"
putexcel D1 = "Std. Dev."
putexcel C1 = "Median"
putexcel E1 = "n"


sum c4 if c4>=0,d
return list
putexcel A2 = "c4"
putexcel B2 = `r(mean)', nformat(number_d2)
putexcel C2 = `r(p50)', nformat(number_d2)
putexcel D2 = `r(sd)', nformat(number_d2)
putexcel E2 = `r(N)', nformat(number_d2)
putexcel F2 = `r(min)',nformat(number_d2)
putexcel G2 = `r(max)',nformat(number_d2)
sum c13 if c13>=0,d
return list
putexcel A3 = "c13"
putexcel B3 = `r(mean)', nformat(number_d2)
putexcel C3 = `r(p50)', nformat(number_d2)
putexcel D3 = `r(sd)', nformat(number_d2)
putexcel E3 = `r(N)', nformat(number_d2)
putexcel F3 = `r(min)',nformat(number_d2)
putexcel G3 = `r(max)',nformat(number_d2)
/*
sum c33 if c33>=0,d
return list
putexcel A4 = "c33"
putexcel B4 = `r(mean)', nformat(number_d2)
putexcel C4 = `r(p50)', nformat(number_d2)
putexcel D4 = `r(sd)', nformat(number_d2)
putexcel E4 = `r(N)', nformat(number_d2)
putexcel F4 = `r(min)',nformat(number_d2)
putexcel G4 = `r(max)',nformat(number_d2)
*/
sum g3 if g3>=0,d
return list
putexcel A5 = "g3"
putexcel B5 = `r(mean)', nformat(number_d2)
putexcel C5 = `r(p50)', nformat(number_d2)
putexcel D5 = `r(sd)', nformat(number_d2)
putexcel E5 = `r(N)', nformat(number_d2)
putexcel F5 = `r(min)',nformat(number_d2)
putexcel G5 = `r(max)',nformat(number_d2)
sum g30 if g30>=0,d
return list
putexcel A6 = "g30"
putexcel B6 = `r(mean)', nformat(number_d2)
putexcel C6 = `r(p50)', nformat(number_d2)
putexcel D6 = `r(sd)', nformat(number_d2)
putexcel E6 = `r(N)', nformat(number_d2)
putexcel F6 = `r(min)',nformat(number_d2)
putexcel G6 = `r(max)',nformat(number_d2)
sum j30a if j30a>=0,d
return list
putexcel A7 = "j30a"
putexcel B7 = `r(mean)', nformat(number_d2)
putexcel C7 = `r(p50)', nformat(number_d2)
putexcel D7 = `r(sd)', nformat(number_d2)
putexcel E7 = `r(N)', nformat(number_d2)
putexcel F7 = `r(min)',nformat(number_d2)
putexcel G7 = `r(max)',nformat(number_d2)
sum j30b if j30b>=0,d
return list
putexcel A8 = "j30b"
putexcel B8 = `r(mean)', nformat(number_d2)
putexcel C8 = `r(p50)', nformat(number_d2)
putexcel D8 = `r(sd)', nformat(number_d2)
putexcel E8 = `r(N)', nformat(number_d2)
putexcel F8 = `r(min)',nformat(number_d2)
putexcel G8 = `r(max)',nformat(number_d2)
sum j30c if j30c>=0,d
return list
putexcel A9 = "j30c"
putexcel B9 = `r(mean)', nformat(number_d2)
putexcel C9 = `r(p50)', nformat(number_d2)
putexcel D9 = `r(sd)', nformat(number_d2)
putexcel E9 = `r(N)', nformat(number_d2)
putexcel F9 = `r(min)',nformat(number_d2)
putexcel G9 = `r(max)',nformat(number_d2)

sum j30e if j30e>=0,d
return list
putexcel A10 = "j30e"
putexcel B10 = `r(mean)', nformat(number_d2)
putexcel C10 = `r(p50)', nformat(number_d2)
putexcel D10 = `r(sd)', nformat(number_d2)
putexcel E10 = `r(N)', nformat(number_d2)
putexcel F10 = `r(min)',nformat(number_d2)
putexcel G10 = `r(max)',nformat(number_d2)
sum j30f if j30f>=0,d
return list
putexcel A11 = "j30f"
putexcel B11 = `r(mean)', nformat(number_d2)
putexcel C11 = `r(p50)', nformat(number_d2)
putexcel D11 = `r(sd)', nformat(number_d2)
putexcel E11 = `r(N)', nformat(number_d2)
putexcel F11 = `r(min)',nformat(number_d2)
putexcel G11 = `r(max)',nformat(number_d2)
sum h30 if h30>=0,d
return list
putexcel A12 = "h30a"
putexcel B12 = `r(mean)', nformat(number_d2)
putexcel C12 = `r(p50)', nformat(number_d2)
putexcel D12 = `r(sd)', nformat(number_d2)
putexcel E12 = `r(N)', nformat(number_d2)
putexcel F12 = `r(min)',nformat(number_d2)
putexcel G12 = `r(max)',nformat(number_d2)


putexcel save
/*
estpost 
summarize c4 c13 c33 g3 g30 j30a j30b j30c j30e j30f h30 if kind=="lf"
summarize c4 c13 c33 g3 g30 j30a j30b j30c j30e j30f h30 if kind=="sf"
summarize c4 c13 c33 g3 g30 j30a j30b j30c j30e j30f h30 if kind=="ld"
summarize c4 c13 c33 g3 g30 j30a j30b j30c j30e j30f h30 if kind=="sd"
*/

