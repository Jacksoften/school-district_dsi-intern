# date: 11/15/2017

library(googlesheets)
gs_ls()
sheet = gs_title("LCAP\ Project\ sec3.csv")
sd = gs_read(ss = sheet)

table(sd$percentOk,sd$supplementOk)
table(sd$supplementOk)
table(sd$scanned)

subsd = sd[which(sd$scanned == FALSE), ]  
table(subsd$supplementOk)

table(sd$percentOk)
table(subsd$percentOk)
