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

paper_plot = function(x, resetplot = TRUE) {
  if(!is.matrix(x)) stop("Input is not a matrix")
  if(dim(x)[1] == 0) stop("Input is empty")
  if(resetplot) plot(c(0,1200), c(-850,-50) , type = 'n')
  sleeptime = 1/nrow(x)
  for(i in 1:nrow(x)){
    Sys.sleep(sleeptime)
    if(dim(x)[1] != 0) rect(x[i,1],-x[i,2],x[i,3],-x[i,4])
  }
}

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

paper_plot(rects)
paper_plot(lines)

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

paper_plot(rects)


lines = rbind(lines, rects_to_lines(rects))
paper_plot(lines)
head(lines)


cells = lines_to_cells(lines, tol_x, tol_y)
paper_plot(cells)

cells = cells_to_rows(cells, tol_x)
paper_plot(cells)

# cells_to_rows merges two too near rows

# TODO
# cells_to_cols: merging two too near columns
cells_to_cols = function(cells, tol = 5) {
  # modified from cells_to_rows
  # NOTE: the row detection below might fail if there are nested cells.
  cells = cells[!is_nested(cells), ]
  
  cells = cells[order(cells[, 1], cells[, 2], decreasing = FALSE), ]
  
  # Cols are determined by the heights of the topmost cells.
  is_topmost = cells[, 2] <= min(cells[, 2]) + tol
  col_id = findInterval(cells[, 1], cells[is_topmost, 1])
  
  cells = cbind(cells, col_id)
  
  return (cells)
}

cells = cells_to_cols(cells, tol_y)
head(cells)
paper_plot(cells)

newlines = rects_to_lines(cells)
paper_plot(newlines)

newrows = newlines[which(newlines[,2] == newlines[,4]),]
newcols = newlines[which(newlines[,1] == newlines[,3]),]
# plot rows
paper_plot(newrows)
# plot cols
paper_plot(newcols)

unicols = unique(newcols[,1]) # the location of column is controlled by row coordinate (x, y1), (x, y2)
mergecols = t(vapply(unicols, function(x) {
  chosen = newcols[which(newcols[,1] == x),,drop = FALSE]
  matrix(c(x,min(chosen[,2]), x, max(chosen[,4])), nrow = 1)
}, numeric(4)))
paper_plot(mergecols)

unirows = unique(newrows[,2]) # the location of column is controlled by row coordinate (x, y1), (x, y2)
mergerows = t(vapply(unirows, function(x) {
  chosen = newrows[which(newrows[,2] == x),,drop = FALSE]
  matrix(c(min(chosen[,1]), x, max(chosen[,3]), x), nrow = 1)
}, numeric(4)))
keeprows = !abs(mergerows[,1] - min(mergerows[,1])) > tol_x
mergerows = mergerows[keeprows,]
paper_plot(mergerows) # TODO delete the short row
paper_plot(mergecols, resetplot = FALSE)

coldiff = mergecols[-nrow(mergecols),] - mergecols[-1,]
mergeCol_id = which(coldiff[,2]<tol_y & coldiff[,4]<tol_y & abs(coldiff[,1]) < tol_y) # merge two close ones
mergecols = mergecols[-mergeCol_id, ]
paper_plot(mergecols)
mergecols


# getCols = function(cells) {
#   # Merge coloumn from different cells to longer lines.
#   urows = unique(c(cells[,1])) #, cells[,2]))
#   cols = list()
#   for(i in seq_along(urows)){
#     cols[[i]] = c(urows[i],
#                   min(cells[cells[,1] == urows[1],2]), 
#                   urows[i], 
#                   max(cells[cells[,1] == urows[1],4]))
#   }
#   do.call(rbind, cols)
# }
# testcols = getCols(cells)
# plot(c(0,1200), c(-850,-50) , type = 'n')
# for(i in 1:nrow(testcols)){
#   if(dim(testcols)[1] != 0) rect(testcols[i,1],-testcols[i,2],testcols[i,3],-testcols[i,4])
# }


# we care more for columns.
texts = pdf_text(page)
texts$page   = page_no

### which_rect ###
which_rects2 = function(x, rects) {
  # how to locate cell not only according to row_id, but also col_id
  rowCheck = apply(x, 1, function(x_) {
    match(TRUE, pt_in_rects2(x_, rects, open_rb = TRUE))
  })
  colCheck = apply(x, 2, function(x_) {
    match(TRUE, pt_in_rects2(x_, rects, open_rb = TRUE))
  })
  c(rowCheck, colCheck)
}
###------------###

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

