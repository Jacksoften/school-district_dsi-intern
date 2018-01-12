# date: 11/15/2017
# yunzhe li

library(googlesheets)
gs_ls()
sheet = gs_title("LCAP\ Project\ sec3.csv")
sd = gs_read(ss = sheet)
saveRDS(sd, file="LCAP_Project_sec3.csv")


table(sd$percentOk,sd$supplementOk)
table(sd$supplementOk)
table(sd$scanned)

subsd = sd[which(sd$scanned == FALSE), ]  
table(sd$supplementOk)
table(subsd$supplementOk)

table(sd$percentOk)
table(subsd$percentOk)

head(subsd)
set.seed(100)
chosen_set = sample(1:length(subsd$doc), length(subsd$doc)/10, replace = FALSE)
for_checking = subsd[chosen_set,]

table(for_checking$supplementOk)
table(for_checking$percentOk)

for_checking$doc
