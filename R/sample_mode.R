sample_mode <- function(x){
  input_type <- typeof(x)
  frequence <- table(x)
  res <- names(frequence)[which.max(frequence)]

  str_res <- paste("as.",input_type,"(res)",sep = "")
  return(eval(parse(text = str_res)))
}
