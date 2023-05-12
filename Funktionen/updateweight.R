## Funcions to balance weight
#Updates weights if changed

updateweight = function(oldweight, new, i) {
  if (new==oldweight[i]) {
    oldweight
  } else if (new==1){
    newweight = rep(0,6)
    oldweight = oldweight
    new = 0.9999
    newweight[-i] = oldweight[-i]/(sum(oldweight[-i]) + 1e-10)*(1-new)
    newweight[i] = new
    newweight
  } else {
    newweight = rep(0,6)
    oldweight = oldweight
    newweight[-i] = oldweight[-i]/(sum(oldweight[-i]) + 1e-10)*(1-new)
    newweight[i] = new
    newweight
  }
}
