# April 17

library(XML)
library(ReadPDF)

file_to_xml <- function(filename) {
  xmlFile = xmlParse(filename)
  xmlRoot(xmlFile)
}

sec2_nodes <- function(Node){
  # Arg: xmlNode
  # Return: all pages of section 2 in XML nodeset form
  # Modified from Nick's code
  
  firstString = sprintf(
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
  firstPage = as.numeric(getNodeSet(Node, firstString))
  lastPage = as.numeric(getNodeSet(Node, lastString))

  pageRange = sprintf("//page[%d <= @number and @number <= %d]", firstPage, lastPage)

  if(length(pageRange) == 0) stop("Something is Wrong, we do not get pageRange")
  
  getNodeSet(Node, pageRange)
}

get_attrs <- function(node) {
  # put different info from XML nodeset to corresponding categories.
  texts = getNodeSet(node, 'text')
  rects = getNodeSet(node, 'rect')
  is_unfilled = sapply(rects, xmlAttrs)[3,] == '0,0,0'
  rects0 = rects[is_unfilled]
  attrs = sapply(texts, xmlAttrs)
  charbbox = apply(attrs, 2, function(x){
    vc = as.numeric(x)
    # delete extreme small bbox
    if(vc[3] <= 5) {
      result = c(0,0,0,0)
    }else{
      result = c(vc[2], vc[1], vc[2] + vc[
        3], vc[1] + vc[4])
    }
    names(result) = c('x0', 'y0', 'x1', 'y1')
    return(result)})
  selected = charbbox[1,] != 0
  charbbox = t(charbbox[, selected])
  texts0 = texts[selected]
  values0 = sapply(texts0, xmlValue)
  
  cleaned_rects = clean_structure(rects0)
  
  list("rect" = cleaned_rects, "charbbox" = charbbox, "text" = texts0, "value" = values0)
}

clean_structure <- function(rects){
  # Args: XML Node of a page
  # Return: lines of cells
  bbox0 = sapply(rects, 
                 function(node) {
                   bbox = xmlAttrs(node)[1]
                   bbox = as.numeric(strsplit(bbox, ',')[[1]])
                   names(bbox) = c('x0', 'y0', 'x1', 'y1')
                   return(bbox)
                   })
  hz0 = matrix(apply(bbox0, 1, 
                     function(rects) {
                       rects[c(1,2,3,2,
                               1,4,3,4)]
                     }), ncol=4, byrow=TRUE)
              
  vt0 = matrix(apply(bbox0, 1, 
                     function(rects) {
                       rects[c(1,2,1,4,
                               3,2,3,4)]
                     }), ncol=4, byrow=TRUE)
  
  uni_horozontal = unique(cbind(bbox0[,1],bbox0[,3]))
  uni_vertical = unique(cbind(bbox0[,2], bbox0[,4]))
  
  group0 = lapply(unique(hz0[,2]),function(x) {
    result = hz0[hz0[,2] == x,,drop=FALSE]
    result[order(result[,1]),,drop=FALSE]
  })
  
  group1 = lapply(unique(vt0[,1]),function(x) {
    result = vt0[vt0[,1] == x,,drop=FALSE]
    result = result[order(result[,2]),,drop=FALSE]
  })
   
  merge0 = do.call(rbind, lapply(group0, function(x) merge(x, 'hz')))
  merge1 = do.call(rbind, lapply(group1, function(x) merge(x, 'vt')))
  
  # getting rid of extreme small ones
  mergehz = merge0[abs(merge0[,1]-merge0[,3])>20,]
  mergevt = merge1[abs(merge1[,2]-merge1[,4])>20,]
  
  list("hz" = mergehz, "vt" = mergevt)
}

merge = function(lines, direction) {
  count = 1
  if(is.null(nrow(lines)) | nrow(lines) == 1) return(lines)
  
  n = nrow(lines)
  new = NULL
  
  if(direction == 'hz')
  {
    other = lines[1,2]
    begin = lines[1,1]
    end = lines[1,3]
    while(count < n){
      if(end >= lines[count+1,1]){  end <- lines[count+1,3]}
      else{
        new <- c(new, begin, other, end, other)
        begin = lines[count+1,1]
        end = lines[count+1,3]
      }
      count <- count + 1
    }
    new <- c(new, begin, other, end, other)
  }
  else if(direction == 'vt')
  {
    other = lines[1,1]
    begin = lines[1,4]
    end = lines[1,2]
    while(count < n){
      if(end >= lines[count+1,4]){
        end <- lines[count+1,2]
      }
      else{
        new <- c(new, other, end, other, begin)
        begin = lines[count+1,4]
        end = lines[count+1,2]
      }
      count <- count + 1
    }
    new <- c(new, other, end, other, begin)
  }
  matrix(new, ncol = 4, byrow = TRUE)
}

get_goal <- function(list){
  # Args: list "rect", "charbbox", "text", "value"
  # Return: texts belongs to Goal section
  
  hzloc = findInterval(list$charbbox[,2], sort(list$rect$hz[,2]))
  vtloc = findInterval(list$charbbox[,1], sort(list$rect$vt[,1]))
  hz_vt = cbind(hzloc, vtloc)
  
  uni_hz_vt = unique(hz_vt)
  uni_hz_vt0 = cbind(uni_hz_vt[order(uni_hz_vt[,1]),], seq_along(
    uni_hz_vt[,1]))
  
  locs = apply(hz_vt, 1,
               function(x){
                 eq = apply(uni_hz_vt0[,1:2], 1, function(i) all(i==x))
                 uni_hz_vt0[eq,3]
               })
  
  
  df = as.data.frame(cbind(list$charbbox, hz_vt, list$value, locs),
                     stringsAsFactors=FALSE)
  
  names(df) = c("x0", "y0", "x1", "y1", "hzloc", "vtloc", "value", "locs" )
  
  
  df$value[df$locs == df$locs[grep("GOAL",df$value)]]
} 
