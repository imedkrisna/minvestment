use "C:\github\minvestment\data\pengelompokkan_sektor.dta",clear
keep year sector_group inv_real
bys sector_group year: egen fdi=total(inv_real)
drop inv_real

duplicates drop year sector_group,force

rename sector_group Economy
rename year Year

export excel using "inves_group", firstrow(variables) replace