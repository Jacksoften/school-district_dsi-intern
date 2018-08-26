merge = function(lines) {
  count = 1
  other = lines[1,2]
  n = nrow(lines)
  new = NULL
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
  matrix(new, ncol = 4, byrow = TRUE)
}
