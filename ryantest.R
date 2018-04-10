library(dplyr)
library(stringr)

library(tidyr)
library(xml2)
setwd('..')
setwd('/Users/ryanbrady/Desktop/DSI LCAP Research/')
list.files()
setwd('LCAP-master/')
list.files()
source("R/geometry.R")
source("R/main.R")
source("R/pdf_xml.R")
source("R/plot_geometry.R")
source("R/utils.R")
setwd('..')
setwd('xml2/')
# assume default working directory is at /xml
file = "BearValleyUnified_LCAP_2015.2016.xml"
xml = read_xml(file)
first = sec2_locate_first(xml)
last = sec2_locate_last(xml)
keywords = sprintf("//page[%s <= @number and @number <= %s]", first, last)
pages = xml_find_all(xml, keywords)
tol_x = 5
tol_y = 5


keywords


rects

pages[1]
pages
page = pages[1]
page_no = as.integer(xml_attr(page, 'number'))

lines = pdf_bbox(xml_find_all(page, './line'))
rects = pdf_bbox(xml_find_all(page, './rect'))
rects
linesr

lines
plot(c(0,1200), c(-850,-50) , type = 'n')

rect(xleft = 100,ybottom = -300,xright = 200,ytop = -200)
rect(rects[1,1],-rects[1,2],rects[1,3],-rects[1,4]) 
for(i in 1:nrow(rects)){
  rect(rects[i,1],-rects[i,2],rects[i,3],-rects[i,4]) 
}

lines






page[1]
find_rect_color = function(nodes) {
  color = sapply(nodes, function(x)
    {
    xml_attr(x, "fill.color")
  })
  colors = strsplit(color, ',')
  result = sapply(colors, as.integer)
  return(t(result))
}
rect_color = find_rect_color(xml_find_all(page, './rect'))

if (length(lines) == 0 && length(rects) == 0) {
	print(sprintf(" Page %s does not have line or rect tags. \n", 
		      page_no))
}
nrow(rects)
rect_color
plot(c(0,1200), c(-850,850) , type = 'n')
for(i in 1:nrow(rects)){
  if(sum(rect_color[i,]) != 0){
    rect(rects[i,1],-rects[i,2],rects[i,3],-rects[i,4], col = 'gray')
  }else{
    rect(rects[i,1],-rects[i,2],rects[i,3],-rects[i,4]) 
  }
}
for(i in 1:nrow(lines22)){
  if(dim(lines)[1] != 0) rect(lines[i,1],-lines[i,2],lines[i,3],-lines[i,4])
}


lines = rbind(lines, rects_to_lines(rects))

head(lines)
cells = lines_to_cells(lines, tol_x, tol_y, plot = TRUE)
cells = cells_to_rows(cells, tol_x)
# we care more for columns.
cells
texts = pdf_text(page)
texts$page   = page_no
texts$cell   = which_rect(texts[1:2], cells)
texts$row    = cells[texts$cell, 5]
texts$c_left = cells[texts$cell, 1]
dim(lines)
# Associate orphaned text nodes with an x-adjacent cell.
is_orphan = is.na(texts$cell)
orphan_loc = apply(texts[is_orphan, 1:2], 1,
                   # Compute (cell, row, c_left)
                   function(orphan) {
                     in_row = which(cells[, 2] <= orphan[2] & orphan[2] < cells[, 4])
                     if (length(in_row) == 0)
                       return (rep(NA, 3)) # no cells found
                     
                     dst = cells[in_row, 3] - orphan[1]
                     to_left = dst <= 0
                     if (any(to_left)) {
                       # found cell to left
                       idx = in_row[which.max(dst[to_left])]
                       return (c(idx + 0.1, cells[idx, 5], cells[idx, 3]))
                     } else {
                       # found cell to right
                       idx = in_row[which.min(dist_left)]
                       return (c(idx - 0.1, cells[idx, 5], orphan[1]))
                     }
                   })

texts$cell[is_orphan]   = orphan_loc[1, ]
texts$row[is_orphan]    = orphan_loc[2, ]
texts$c_left[is_orphan] = orphan_loc[3, ]

cell_ids = unique(texts$cell)
combine_text = lapply(cell_ids, function(x) 
  {
  paste(texts$text[texts$cell == x])
})
tol_join = 10
lines = lines[lines[3, ] - lines[1, ] > tol_join, ]                                           
lines

lines = rbind(lines,c(far_lt,far_tp,far_rt,far_tp),c(far_lt, far_bt, far_rt, far_bt))
lines
lines2 = split_lines_hv(lines)
lines2
head(lines)
dim(lines2$hz)
lines2$vt
vt = simplify_lines_vt(lines2$vt, tol_line = 5, tol_join = tol_x * 2)
vt
simplify_lines_hz
dim(lines2$hz)
lines2$hz
lines[,3]
lines[,1]
lines2$hz[c(1,3),]
lines2$hz[lines2$hz[,3]] - lines2$hz[lines2$hz[,1]]
lines2$hz[lines2$hz[,3 ] - lines2$hz[,1 ] ,]
lines22 = lines2$hz[lines2$hz[,3 ] - lines2$hz[,1 ] > tol_join, ]

rect(lines[1,])
hz
newcells = lines_to_cells(lines)

newrows = cells_to_rows(newcells,tol = 5)
which_segment()
lines22
