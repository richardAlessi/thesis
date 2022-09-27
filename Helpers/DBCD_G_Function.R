DBCD_G = function(x,y,gam){
  if (x!=0 & x != 1) {
    top = y*(y/x)^gam 
    bottom = top + (1-y)*((1-y)/(1-x))^gam
    result = top/bottom
  }else{
    result = 1-x
  }
  return(result)
}
