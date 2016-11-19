remove_out <- function(param,index,min_v,max_v){
  val = NULL
  val = param[index]
  if(val < min_v | val > max_v | is.na(val)){
    if(index-1 >= 1){
      val = param[index-1]
    } else if (index-1 <= 0){
      val = param[index+1]
    } 
    return(val)
  } else{
    print("Nothing changed")
    return(val)  #Normal Value return
  }
}