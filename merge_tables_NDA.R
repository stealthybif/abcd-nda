#This script will read ABCD data tables downloaded from NDAR and merge on src_subject_id, gender, eventname.
# The merged tables are output as a RDS file.

require(dplyr)

#Assumes ABCD data was downloaded from NDAR. Path to download folder containing .txt tables.
script.dir <- "~/ABCDstudyDEAP/"
setwd(script.dir)
input_list  <- Sys.glob(paths=c(paste(script.dir,"*.txt", sep="")))

#This is a place to define txt tables to exclude from the merge. 
#Grep will do partial name matches. Take care to inspect the input_list object afterwards.
exc_list <- c("package_info.txt", "omics_experiments.txt", "aurora01.txt", "fmriresults01.txt", "genomics_sample03.txt", "dti", "mid", "nback", "sst", "mrirs", "rsi", "tfa", "tfn", "tnbase")
for (i in 1:length(exc_list)) { 
  input_list <- input_list[-grep(exc_list[i], input_list)]
}


#Load all tables into memory in one big list object. 
# There is an issue with the field "interview_age". It is possible they are different values between tables
# if participant has a birthday between visit dates. This causes duplicate incomplete entries after merge.
# This script uses interview_age only from pdem02.txt and drops it from all other tables.
tables <- list()
for (p in 1:length(input_list)) {
  print(p)
  input <- input_list[p]
  print(paste("import: ", input, " [", p, "/",length(input_list), "]", sep=""))
  
  if(! grepl("pdem02.txt", input)) {
  #read data and remove descriptive line in 2nd row. 
  f <- readLines(input)
  dt <- tbl_df(read.table(textConnection(f[-2]), header=T, sep="\t"))
  # Keep only keep baseline and drop columns introduced by NDA. Drop interview_age due to merge issues
  dt <- dt %>% filter(eventname == "baseline_year_1_arm_1") %>% select(-1, -interview_age, -collection_id, -collection_title, -dataset_id, -subjectkey, -study_cohort_name, -interview_date)
  
  tables[[p]] <- dt
  } else {
    #read data and remove descriptive line in 2nd row. 
    f <- readLines(input)
    dt <- tbl_df(read.table(textConnection(f[-2]), header=T, sep="\t"))
    # Keep only keep baseline and drop columns introduced by NDA.
    dt <- dt %>% filter(eventname == "baseline_year_1_arm_1") %>% select(-1, -collection_id, -collection_title, -dataset_id, -subjectkey, -study_cohort_name, -interview_date)

    tables[[p]] <- dt
    }
  
}

t2 = tables

#Clear the workspace
rm( tables, dt, f, input)


while ( length(t2) > 1 ) {
  print("iteration")
  access= seq(1,length(t2)-1,2)
  for (i in access) {
    bm = dim(t2[[i]])
       t2[[i]] = merge(t2[[i]], t2[[i+1]], by=c("src_subject_id","eventname","gender"), all=TRUE)
    # debugging output, 4,521 rows should survive the merge
    print(paste("rows before: ", bm[1], dim(t2[[i+1]])[1], " rows after: ",dim(t2[[i]])[1], "indices: ",i,i+1," columns: ",bm[2],"+",dim(t2[[i+1]])[2], " = ",dim(t2[[i]])[2]))
  }
  # for odd number of instruments add the last spreadsheet back to the list
  if (length(t2) %% 2 != 0) access = append(access,length(t2))
  # reduce the list
  t2 = t2[access]
}

nda18 <- t2[[1]]
saveRDS(nda18, "nda18_NDA.RDS")