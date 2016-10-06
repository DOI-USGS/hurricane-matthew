process.classify <- function(viz){
  #this is just what was left over from my script that wasn't fetch
  
  #classify current discharge values
  finalJoin$class <- NA
  finalJoin$class <- ifelse(is.na(finalJoin$p25), 
                            ifelse(finalJoin$Flow > finalJoin$p50_va, "darkorange1","greenyellow"),
                            ifelse(finalJoin$Flow < finalJoin$p25_va, "cyan",
                                   ifelse(finalJoin$Flow > finalJoin$p75_va, "red2","green4")))
  return(finalJoin)
}