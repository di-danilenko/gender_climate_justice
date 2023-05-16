library(utils)
library(gender)
library(genderdata)
library(genderize)
library(sys)
# load a vector of names from a txt file, '\n' separator
# "Users/dianadanilenko/Desktop/methods/python_scripts/data/data_names.txt"
# read the text file into a character vector
lines <- readLines("/Users/dianadanilenko/Desktop/methods/python_scripts/data/data_names.txt")

# split the lines into individual elements
names_list <- unlist(strsplit(lines, "\n"))
  
# assume years distribution between 1900 and 2005
# run this first on the default database and then try additional 
gender(names_list, years=c(1900,2005))
# default setting only has values for 10,723 names

gender(names_list,method = c("genderize"))
# this results in too many requests -> figure out a workaround

# for (i in 1:length(names_list)) {
#   # Make the request
#   response <- gender(names_list[i],method = c("genderize"))
#   
#   # Pause for 1 second
#   Sys.sleep(2)
# }

gender_ssa <- gender(names_list,method = c("ssa"))
gender_ipums <- gender(names_list,method = c("ipums"))
gender_napp<- gender(names_list,method = c("napp"))
gender_kantrowitz <- gender(names_list,method = c("kantrowitz"))
gender_genderize <- gender(names_list,method = c("genderize"))

print(gender(names_list,method = c("kantrowitz")),n = 35000)
sum(!is.na(gender_napp['gender']))

write.csv(gender_ssa, file = "gender_ssa.csv")
write.csv(gender_ipums, file = "gender_ipums.csv")
write.csv(gender_napp, file = "gender_napp.csv")
write.csv(gender_kantrowitz, file = "gender_kantrowitz.csv")