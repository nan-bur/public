
#
if(!require(pacman)) {install.packages("pacman")}

# 
pacman::p_load(dplyr,
               sf
)


batch_intersection <- function(cookie_mp, sf_df, b=10000){
  # Returns the intersection of cookie_mp (a multipolygon sfc) and sf_df (a sf dataframe),
  # cycles through in batches of b polygons and combining the results. This is much faster than intersecting the whole object.  
  # The result is a sf dataframe containing all the attributes of sf_df plus the calculated area of the intersection in each polygon. 
  
  remaining <- st_cast(cookie_mp, "POLYGON")
  r <- length(remaining)
  r_orig <- r
  
  combined <- NULL
  
  while(r > 0){
    
    if(r <= b){b <- r}   
    slice <- remaining[1:b]
    if(r > b){remaining <- remaining[(b+1):r]} else remaining <- NULL
    
    temp <- st_filter(sf_df, slice, .predicate = st_intersects)
    
    slice_i <- st_intersection(temp, slice) %>% 
      mutate(area_m2 = as.numeric(st_area(.)))
    
    # n.b. this intersection results in POINTS for some self-intersecting geoms. 
    combined <- rbind(combined, slice_i)
    
    r <- length(remaining)
    print(paste0(r, " remaining of ", r_orig))
  }
  
  return(combined)
}