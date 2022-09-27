DBCD_G_Multi = function(X,Y,gam){
    L = 1000
    top = pmin(Y*(Y/X)^gam ,L)
    bottom = sum(top)
    result = top/bottom
    return(result)
}
