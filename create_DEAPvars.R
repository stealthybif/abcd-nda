#This script will recode variables found in the DEAP portal and save an RDS file.
#Most of this script is identical to steps outlined here: https://github.com/ABCD-STUDY/analysis-nda17/blob/master/notebooks/derived/core_demographic.md
# Just cleaned up minor naming issues that have not been updated yet

readRDS("nda18_NDA.RDS")

nda18$abcd_site = nda18$site_id_l
nda18$subjectid = nda18$src_subject_id
nda18$age = nda18$interview_age
nda18$female = factor(as.numeric(nda18$gender == "F"), levels = 0:1, labels = c("no", "yes") ) 
nda18$sex = nda18$gender


household.income = nda18$demo_comb_income_v2b
household.income[nda18$demo_comb_income_v2b == "1"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "2"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "3"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "4"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "5"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "6"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "7"] = 2 # "[>=50K & <100K]"
household.income[nda18$demo_comb_income_v2b == "8"] = 2 # "[>=50K & <100K]"
household.income[nda18$demo_comb_income_v2b == "9"] = 3 # "[>=100K]"
household.income[nda18$demo_comb_income_v2b == "10"] = 3 # "[>=100K]"
household.income[nda18$demo_comb_income_v2b == "777"] = NA
household.income[nda18$demo_comb_income_v2b == "999"] = NA
household.income[household.income %in% c(NA, "999", "777")] = NA
nda18$household.income = factor( household.income, levels= 1:3, labels = c("[<50K]", "[>=50K & <100K]", "[>=100K]") )

highest.education = rep("999", length(nda18$demo_prnt_ed_v2b))
highest.education[nda18$demo_prnt_ed_v2b == "0"] = 1
highest.education[nda18$demo_prnt_ed_v2b == "1"] = 4
highest.education[nda18$demo_prnt_ed_v2b == "2"] = 5
highest.education[nda18$demo_prnt_ed_v2b == "3"] = 6
highest.education[nda18$demo_prnt_ed_v2b == "4"] = 7
highest.education[nda18$demo_prnt_ed_v2b == "5"] = 8
highest.education[nda18$demo_prnt_ed_v2b == "6"] = 9
highest.education[nda18$demo_prnt_ed_v2b == "7"] = 10
highest.education[nda18$demo_prnt_ed_v2b == "8"] = 11
highest.education[nda18$demo_prnt_ed_v2b == "9"] = 12
highest.education[nda18$demo_prnt_ed_v2b == "10"] = 13
highest.education[nda18$demo_prnt_ed_v2b == "11"] = 14
highest.education[(nda18$demo_prnt_ed_v2b == "12") | (nda18$demo_prnt_ed_v2b == "13")] = 16
highest.education[nda18$demo_prnt_ed_v2b == "14"] = 17
highest.education[nda18$demo_prnt_ed_v2b == "15"] = 18
highest.education[(nda18$demo_prnt_ed_v2b == "16") | (nda18$demo_prnt_ed_v2b == "17")] = 20
highest.education[nda18$demo_prnt_ed_v2b == "18"] = 21
highest.education[nda18$demo_prnt_ed_v2b == "19"] = 22
highest.education[nda18$demo_prnt_ed_v2b == "20"] = 23
highest.education[nda18$demo_prnt_ed_v2b == "21"] = 24
highest.education[nda18$demo_prnt_ed_v2b == "777"] = 999
highest.education[highest.education == 999] = NA

highest.education2 = rep("999", length(nda18$demo_prtnr_ed_v2b))
highest.education2[nda18$demo_prtnr_ed_v2b == "0"] = 1
highest.education2[nda18$demo_prtnr_ed_v2b == "1"] = 4
highest.education2[nda18$demo_prtnr_ed_v2b == "2"] = 5
highest.education2[nda18$demo_prtnr_ed_v2b == "3"] = 6
highest.education2[nda18$demo_prtnr_ed_v2b == "4"] = 7
highest.education2[nda18$demo_prtnr_ed_v2b == "5"] = 8
highest.education2[nda18$demo_prtnr_ed_v2b == "6"] = 9
highest.education2[nda18$demo_prtnr_ed_v2b == "7"] = 10
highest.education2[nda18$demo_prtnr_ed_v2b == "8"] = 11
highest.education2[nda18$demo_prtnr_ed_v2b == "9"] = 12
highest.education2[nda18$demo_prtnr_ed_v2b == "10"] = 13
highest.education2[nda18$demo_prtnr_ed_v2b == "11"] = 14
highest.education2[(nda18$demo_prtnr_ed_v2b == "12") | (nda18$demo_prtnr_ed_v2b == "13")] = 16
highest.education2[nda18$demo_prtnr_ed_v2b == "14"] = 17
highest.education2[nda18$demo_prtnr_ed_v2b == "15"] = 18
highest.education2[(nda18$demo_prtnr_ed_v2b == "16") | (nda18$demo_prtnr_ed_v2b == "17")] = 20
highest.education2[nda18$demo_prtnr_ed_v2b == "18"] = 21
highest.education2[nda18$demo_prtnr_ed_v2b == "19"] = 22
highest.education2[nda18$demo_prtnr_ed_v2b == "20"] = 23
highest.education2[nda18$demo_prtnr_ed_v2b == "21"] = 24
highest.education2[nda18$demo_prtnr_ed_v2b == "777"] = 999
highest.education2[highest.education2 == 999] = NA
nda18$highest.education = factor( as.character(pmax(as.numeric(highest.education), as.numeric(highest.education2),na.rm=T)), levels=c(9,10,11,12,13,14,15,16,17,18,20,21,22,23,24), labels=c("9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "20", "21", "22", "23", "24") )

high.educ1 = nda18$demo_prnt_ed_v2b
high.educ2 = nda18$demo_prtnr_ed_v2b
high.educ1[which(high.educ1 == "999")] = NA
high.educ2[which(high.educ2 == "999")] = NA
high.educ1[which(high.educ1 == "777")] = NA
high.educ2[which(high.educ2 == "777")] = NA
high.educ = pmax(as.numeric(as.character(high.educ1)), as.numeric(as.character(high.educ2)), na.rm=T)
idx <- which(high.educ %in% 0:12, arr.ind = TRUE)
high.educ[idx] = 1 # "< HS Diploma"
idx <- which(high.educ %in% 13:14, arr.ind = TRUE)
high.educ[idx] = 2 # "HS Diploma/GED"
idx <- which(high.educ %in% 15:17, arr.ind = TRUE)
high.educ[idx] = 3 # "Some College"
idx <- which(high.educ == 18, arr.ind = TRUE)
high.educ[idx] = 4 # "Bachelor"
idx <- which(high.educ %in% 19:21, arr.ind = TRUE)
high.educ[idx] = 5 # "Post Graduate Degree"
high.educ[which(high.educ == "999")]=NA
high.educ[which(high.educ == "777")]=NA
nda18$high.educ = factor( high.educ, levels= 1:5, labels = c("< HS Diploma","HS Diploma/GED","Some College","Bachelor","Post Graduate Degree") )

married = rep("0", length(nda18$demo_prnt_marital_v2b))
married[as.numeric(nda18$demo_prnt_marital_v2b) == 1] = 1
married[as.numeric(nda18$demo_prnt_marital_v2b) == 7] = NA
nda18$married = factor( married, levels= 0:1, labels = c("no", "yes") )

married.livingtogether = rep("0", length(nda18$demo_prnt_marital_v2b))
married.livingtogether[as.numeric(nda18$demo_prnt_marital_v2b) == 1 | as.numeric(nda18$demo_prnt_marital_v2b) == 6] = 1
married.livingtogether[as.numeric(nda18$demo_prnt_marital_v2b) == 7] = NA
nda18$married.livingtogether = factor( married.livingtogether, levels= 0:1, labels = c("no", "yes") )

nda18$anthro_bmi_calc = as.numeric(as.character(nda18$anthro_weight_calc)) / as.numeric(as.character(nda18$anthro_height_calc))^2 * 703

nda18$demo_race_white= (nda18$demo_race_a_p___10 == 1)*1
nda18$demo_race_black= (nda18$demo_race_a_p___11 == 1)*1
nda18$demo_race_asian = 0
nda18$demo_race_asian[nda18$demo_race_a_p___18 == 1 | nda18$demo_race_a_p___19 == 1 | 
                        nda18$demo_race_a_p___20 == 1 | nda18$demo_race_a_p___21 == 1 | 
                        nda18$demo_race_a_p___22 == 1 | nda18$demo_race_a_p___23 == 1 |
                        nda18$demo_race_a_p___24==1] = 1
nda18$demo_race_aian = 0
nda18$demo_race_aian[nda18$demo_race_a_p___12 == 1 | nda18$demo_race_a_p___13 == 1] = 1
nda18$demo_race_nhpi = 0
nda18$demo_race_nhpi[nda18$demo_race_a_p___14 == 1 | nda18$demo_race_a_p___15 == 1 | 
                       nda18$demo_race_a_p___16 == 1 | nda18$demo_race_a_p___17 == 1] = 1
nda18$demo_race_other = 0
nda18$demo_race_other[nda18$demo_race_a_p___25 == 1] = 1
nda18$demo_race_mixed = nda18$demo_race_white + nda18$demo_race_black + nda18$demo_race_asian + 
  nda18$demo_race_aian + nda18$demo_race_nhpi + nda18$demo_race_other

nda18$demo_race_mixed[ nda18$demo_race_mixed <= 1] =  0
nda18$demo_race_mixed[ nda18$demo_race_mixed > 1] =  1

nda18$race.eth = NA
nda18$race.eth[ nda18$demo_race_white == 1] = 2
nda18$race.eth[ nda18$demo_race_black == 1] = 3
nda18$race.eth[ nda18$demo_race_asian == 1] = 4
nda18$race.eth[ nda18$demo_race_aian == 1]  = 5
nda18$race.eth[ nda18$demo_race_nhpi == 1]  = 6
nda18$race.eth[ nda18$demo_race_other == 1] = 7
nda18$race.eth[ nda18$demo_race_mixed == 1] = 8

nda18$race.eth[nda18$demo_ethn_p == 1] = 1
nda18$demo_race_hispanic = 0
nda18$demo_race_hispanic[nda18$demo_ethn_p == 1] = 1

nda18$race.eth <- factor(nda18$race.eth,
                         levels = c(2,1,3,4,5,6,7,8),
                         labels = c("White", "Hispanic", "Black", "Asian", "AIAN", "NHPI", "Other", "Mixed") ) 

nda18$race.ethnicity = nda18$race.eth
nda18$race.ethnicity[!(nda18$race.eth=="White" | nda18$race.eth=="Black" |
                         nda18$race.eth=="Asian" | nda18$race.eth=="Hispanic")] = "Other"
nda18$race.ethnicity = droplevels(nda18$race.ethnicity)

saveRDS(nda18, "nda18_DEAP.RDS")
