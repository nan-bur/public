
if(!require(pacman)) {install.packages("pacman")}

# 
pacman::p_load(dplyr,
               tictoc, 
               stringr)

speedtest <- function(input_df, fx, min, max, steps){
  tic.clearlog()
  for(n in seq(from = min, to = max, length.out = steps)){
    slice <- input_df %>% 
      slice_sample(n = n)
    tic(paste(n))
    out <- fx(slice)
    toc(log = T, quiet = T)
    rm(out)
  }
  tlog <- unlist(tic.log(format = T))
  
  result <-   as_tibble(str_split_fixed(str_replace_all(tlog, "[[:alpha:]]|\\s",""), ":", 2)) %>%
    mutate(records = as.numeric(V1), seconds = as.numeric(V2)) %>%
    select(records, seconds)
  
  return(result)
  tic.clearlog()
  
}
