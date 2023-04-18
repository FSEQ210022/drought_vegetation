# Function's script to get the dates that have to be updated
# by Francisco Zambrano Bigiarini
# January 2020

# inputs:
# dirsTif: the folder where are the tif files "*.tif"
# dirsHDF: folder wheere are the HDF files "*.hdf"

dates2Update <- function(dirsTif,dirsHDF){
  datesTif <- gsub('-','.',as.Date(substr(list.files(dirsTif),5,11),"%Y%j"))
  n <- length(strsplit(dirsHDF,'/')[[1]])
  datesHDF <- sapply(strsplit(dirsHDF,'/'),"[[",n)
  datesUpdate <- datesHDF[!datesHDF %in% datesTif]
  return(datesUpdate)
}
