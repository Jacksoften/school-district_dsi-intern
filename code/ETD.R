## Extracting tabular data from PDF by using ReadPDF and XML
## Author: Jack
## Begin Date: Mar 25
## Expected Complition Date: April 25
library(XML)
library(ReadPDF)

chosenFile = '../LCAP/xml/BearValleyUnified_LCAP_2015.2016.xml'
xmlFile = xmlParse(chosenFile)
rootNode = xmlRoot(xmlFile)
xmlSummary = xmlElementSummary(chosenFile)

# locate the section 2
firstString = sprintf(
    # By Nick
    # Fixed for Magnolia Elementary
    "//page[
      (
        text[contains(normalize-space(text()), '%s')]
        and not(text[contains(normalize-space(text()), '%s')])
      )
      or text[contains(normalize-space(text()), '%s')]
    ][1]/@number"
    , "Related State and/or Local"
    , "Identify the state and/or local"
    , "Related State and /or Local"
  )

lastString = sprintf(
    "//page[
      text[contains(normalize-space(text()), '%s')]
      or text[contains(normalize-space(text()), '%s')]
      or text[contains(normalize-space(text()), '%s')]
    ][1]/@number"
    , "Annual Update Instructions"
    , "Original"
    , "Section 3: Use of Supplemental"
  )

# finding the first page and last page of section2.
firstPage = as.numeric(getNodeSet(rootNode, firstString))
lastPage = as.numeric(getNodeSet(rootNode, lastString))

# get all nodes within target range
pageRange = sprintf("//page[%d <= @number and @number <= %d]", firstPage, lastPage)

allPages = getNodeSet(rootNode, pageRange)

# choose one page to deal with
library(stringr)
texts = getNodeSet(allPages[[1]], "text")
rects = getNodeSet(allPages[[1]], "rect")
attrs = sapply(texts, xmlAttrs)
charbbox = apply(attrs, 2, function(x){
			 vc = as.numeric(x)
			 # delete extreme small bbox
			 if(vc[3] <= 5) {
			   result = c(0,0,0,0)
			 }else{
			   result = c(vc[2], vc[1], vc[2] + vc[3], vc[1] + vc[4])
			 }
			 names(result) = c('x0', 'y0', 'x1', 'y1')
			 return(result)
  })
charbbox = charbbox[, charbbox[1,] != 0]

# cat("charbbox range: ", apply(charbbox, 1, range), "\n")
values = sapply(texts, xmlValue)
bboxes = sapply(rects, function(node) {
			bbox = xmlAttrs(node)[1]
			bbox = as.numeric(strsplit(bbox, ',')[[1]])
			names(bbox) = c('x0', 'y0', 'x1', 'y1')
			return(bbox)

  })
# cat("bboxes range: ", apply(bboxes, 1, range), "\n")

pdf_plot = function(x, resetplot = TRUE, color = 'black') {
  # plot rects and characters in the format of pdfs
  if(!is.matrix(x)) stop("Input is not a matrix")
  if(dim(x)[1] == 0) stop("Input is empty")
  if(resetplot) plot(c(0,1200), c(-850,-50) , type = 'n')
  sleeptime = 1/nrow(x)
  for(i in 1:nrow(x)){
    Sys.sleep(sleeptime)
    if(dim(x)[1] != 0) rect(x[i,1],-x[i,2],x[i,3],-x[i,4],border=color)
  }
}

pdf_plot(t(bboxes))
pdf_plot(t(charbbox), resetplot=FALSE, color='red')

# TODO:
# 1. Merge cells (bboxes)
# 2. Delete Empty words cells (charbbox)
