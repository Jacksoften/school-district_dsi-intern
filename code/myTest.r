library(dplyr)
library(stringr)
library(tidyr)
library(xml2)

setwd("~/Work/school-district_dsi-intern/LCAP/xml/")
source("../R/geometry.R")
source("../R/main.R")
source("../R/pdf_xml.R")
source("../R/plot_geometry.R")
source("../R/utils.R")

# assume default working directory is at /xml
file = "BearValleyUnified_LCAP_2015.2016.xml"
xml = read_xml(file)
first = sec2_locate_first(xml)
last = sec2_locate_last(xml)
keywords = sprintf("//page[%s <= @number and @number <= %s]", first, last)
pages = xml_find_all(xml, keywords)
tol_x = 5
tol_y = 5

page = pages[4]
page_no = as.integer(xml_attr(page, 'number'))

lines = pdf_bbox(xml_find_all(page, './line'))
rects = pdf_bbox(xml_find_all(page, './rect'))
if (length(lines) == 0 && length(rects) == 0) {
	print(sprintf(" Page %s does not have line or rect tags. \n", 
		      page_no))
}

lines = rbind(lines, rects_to_lines(rects))
head(lines)
cells = lines_to_cells(lines, tol_x, tol_y, plot = TRUE)
cells = cells_to_rows(cells, tol_x)
# we care more for columns.
texts = pdf_text(page)
texts$page   = page_no
texts$cell   = which_rect(texts[1:2], cells)
texts$row    = cells[texts$cell, 5]
texts$c_left = cells[texts$cell, 1]

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

