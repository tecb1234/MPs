library(httr)
library(purrr)
library(dplyr)




# "http://lda.data.parliament.uk/edms.json?_sort=-dateTabled&_properties=title,motionText,signature.member.twitter,signature.member.fullName,signature.member.party&_view=basic&_page=0&_pageSize=10"


download_edms <- function(page, page_size){
  #page <- 0
  #page_size <- 10
  api_return_edm <- GET(url = paste0("http://lda.data.parliament.uk",
                                     "/edms.json",
                                     "?_sort=-dateTabled",
                                     "&_properties=",
                                     #"?_properties=",
                                     "dateTabled,",
                                     "edmNumber,",
                                     "title,",
                                     "primarySponsorPrinted,",
                                     "signature.member.fullName,",
                                     "signature.member.party",
                                     "&_view=basic",
                                     "&_page=", page,
                                     "&_pageSize=", page_size))
  
  raw_edm <- content(api_return_edm)
  
  return(raw_edm$result$items)
  
}



process_edm_list <- function(raw_edm_list){
  edm_df <- raw_edm_list %>%
    map_df(process_edm)
  
  return(edm_df)
}

process_edm <- function(raw_edm_object){
  date_tabled <- raw_edm_object$dateTabled$`_value`
  title <- raw_edm_object$title
  edm_number <- raw_edm_object$edmNumber
  primary_sponsor <- raw_edm_object$primarySponsorPrinted[[1]] #sometimes there are more
  
  if(is.null(primary_sponsor)) primary_sponsor <- "No Primary Sponsor"
  
  signatures_df <- process_signatures(raw_edm_object$signature)
  
  
  
  meta_df <- data.frame(date_tabled, 
                        title,
                        edm_number,
                        primary_sponsor,
                        stringsAsFactors = FALSE)
  
  meta_df <- meta_df[rep(seq_len(nrow(meta_df)), each = nrow(signatures_df)),]
  
  edm_df <- bind_cols(meta_df, signatures_df)
  return(edm_df)
}

process_signatures <- function(edm_signature_object){
  signatures_df <- edm_signature_object %>%
    map_df(process_signer)
  return(signatures_df)
}


process_signer <- function(edm_signer_object){
  
  
    
    signer_name <- tryCatch(edm_signer_object$member[[1]]$fullName$`_value`,
                            error = function(c) "No Name")
    signer_party <- tryCatch(edm_signer_object$member[[1]]$party$`_value`,
                             error = function(c) "No Party")
    
    if(is.null(signer_party)) signer_party <- "No Party"
    
    edm_signer_df <- data.frame(signer_name, signer_party, stringsAsFactors = FALSE)

  
  return(edm_signer_df)
}


edm_df <- 0:200  %>%
  map(download_edms, 100) %>%
  map_df(process_edm_list)

save(edm_df, file = "edms.Rda")
