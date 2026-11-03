# code/functions/rem_dup.R (formerly texas/FUN_rem_dup.R)
rem_dup <- function(x){
  paste(unique(trimws(unlist(strsplit(x,split=" ",fixed=F,perl=T)))),collapse = 
          " ")
}

rem_dup <- Vectorize(rem_dup)