## get words from scanned images
## We can use multiple tools and do cross-validation and get confidence of
##   if we get the correct information.
library(Rtesseract)

a = tesseract(".jpg")
bb = GetBoxes(a)
