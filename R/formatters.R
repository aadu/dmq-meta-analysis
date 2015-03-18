#' CFormat Shorcuts
#'
#' @export
fC = function(x, digits=2){
  x = formatC(x=x, digits=digits, format="f")
  x = gsub("0([.])", "\\1", x)
  if(as.numeric(x) >= 0)
    x = paste("~", x, sep="")
  return(x)
}


#' @describeIn fC
#' @export
fC2 = function(x, digits=2){
  x = formatC(x=x, digits=digits, format="f")
  return(gsub("0([.])", "\\1", x))
}
