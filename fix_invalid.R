

#
if(!require(pacman)) {install.packages("pacman")}

# 
pacman::p_load(dplyr,
               sf
)


fix_invalid <- function(input, quiet = T){

  # function to "fix" geometries which are invalid in R due to convention differences between ESRI and OGS.

  # applies two fixes:

  # FIX 1.
  # uses postGIS ST_makevalid to make invalid geometries valid (effectively applies a buffer of length zero, forcing the polygon to be re-drawn using OGS conventions)

  # FIX 2. 
  # uses gdal_utils vectortranslate to reproject any polygons that are "non-valid" (postGIS ST_isvalid = FALSE).
  # NB this step involves writing a temporary file and reading it back in.

  if(!is.logical(quiet)){quiet_ = TRUE} else {quiet_ = quiet}
  
  geomname <- attr(input, "sf_column")

  # FIX 1: ST_MakeValid to fix invalid polygons
  invalid_fix <- input %>% 
    mutate(is_valid = st_is_valid(.)) %>%
    filter(is_valid == FALSE) %>% 
    st_make_valid(.[,geomname]) %>% # NB: This applies postGIS function STmake_valid which "validifies" self-intersecting rings by applying a buffer of threshold 0. This is necessary to convert from ESRI to OGD standards for drawing polygons with holes touching the edge.
    mutate(is_valid = st_is_valid(.)) %>% 
    select(-is_valid)
  
  # FIX 2. gdals vectortranslate to fix non-valid polygons
  nonvalid <- input %>% filter(is.na(st_is_valid(.[,geomname])))
  nonvalid$row <- row.names(nonvalid)
  names_r <- names(nonvalid) 
  tf1 <- tempfile("temp_nonvalid_", fileext = ".shp")
  tf2 <- tempfile("temp_revalid_", fileext = ".shp")
  
  st_write(nonvalid, tf1, append = F, quiet = quiet_)
  gdal_utils(util = "vectortranslate", source = tf1, destination = tf2, option = "-overwrite")
  revalid <- st_read(tf2, quiet = quiet_)
  revalid <- st_set_geometry( revalid , geomname) %>% st_make_valid
  names(revalid)[names(revalid) != geomname] <- names_r[names_r != geomname]

  # Add the now valid polygons back in:
  result <- input %>%
    filter(st_is_valid(.[,geomname]) == TRUE) %>%
    rbind(invalid_fix, revalid %>% select(-"row"))

  # Return a warning if any non-valid polygons were lost during vectortranslate
  na_re <- length(unique(revalid$row))
  na_n <- sum(is.na(st_is_valid(input[,geomname])))
  if(na_n > na_re){warning(paste(na_n - na_re, " non-valid polygons were lost."))}
  
  file.remove(c(tf1, tf2))
  return(result)
}
