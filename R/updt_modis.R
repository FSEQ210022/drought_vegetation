# Script to download MODIS data from htps
# by Francisco Zambrano Bigiarini (frzambra@gmail.com)
# July 2018

updt_modis <- function(type,product,dir_out,token = NULL){
  
  if (is.null(token)) token <- Sys.getenv('token_nasa')
  
  URL <- paste0("https://e4ftl01.cr.usgs.gov/",type,"/",product,'/')

  htmlContent <- RCurl::getURL(URL)
  htmlTree <- XML::htmlParse(htmlContent)
  links <- XML::xpathSApply(htmlTree, "//a/@href")

  datesWeb <- as.character(unlist(regmatches(links,gregexpr("[0-9]{4}.[0-9]{2}.[0-9]{2}",links))))

  if (dir.exists(file.path(dir_out,product))){
    datesDir <- list.files(file.path(dir_out,product))
    } else {
      dir.create(file.path(dir_out,product))
    datesDir <- character(0)
  }

  datesUpdate <- datesWeb[!datesWeb %in% datesDir]
  
  pb <- progress::progress_bar$new(
    total = length(datesUpdate),
    format = " Downloading data for date :date [:bar] :percent eta: :eta"
  )
  
  purrr::map(datesUpdate, function(dir){
    pb$tick(tokens = list(date = dir))
    htmlContent2 <- RCurl::getURL(paste0(URL,dir,'/'))
    htmlTree2 <- XML::htmlParse(htmlContent2)
    links2 <- XML::xpathSApply(htmlTree2, "//a/@href")
    
    files <- unlist(regmatches(links2,gregexpr(paste0(substr(product,1,7),".*hdf$"),links2)))
    filesInd <- grep(files,pattern='.*(h11v10|h11v11|h11v12|h12v12|h12v13|h13v13|h13v14|h14v14).*.hdf$')
    files <- files[filesInd]
    dir.create(paste0(dir_out,product,'/',dir,'/'))
    
    out <- purrr::map(files,function(name){
        tryCatch(
        paste0(URL,dir,'/',name) |> 
          httr2::request() |> 
          httr2::req_auth_bearer_token(token) |> 
          httr2::req_perform(path = paste0(dir_out,product,'/',dir,'/',name),
                             verbosity = 0)
      )
    }, .progress = list(
      type = "iterator", 
      format = "Downloading {cli::pb_bar} {cli::pb_percent}",
      clear = TRUE))
  })
}

