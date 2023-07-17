
# KEGG pathway  -----------------------------------------------------------
#' @title MDA_name2kegg
#' @description Get kegg cid by kegg website
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param query 'character' ; compound names.
#' @param type "single','multiple';  For one compound or multiple compounds.
#' @param core_num 'numeric'; how many cores you want to use
#' @return KEGGID compound name 2 kegg cid.
#' @importFrom magrittr %>%
#' @importFrom stringr str_split str_extract
#' @importFrom RCurl getURL
#' @importFrom XML htmlParse getNodeSet xmlValue
#' @importFrom dplyr mutate select
#' @importFrom future plan
#' @importFrom furrr future_map_dfr
#' @importFrom crayon green bold italic red yellow
#' @export
mda_name2kegg <- function(query,type = "multiple",core_num = 1){
  ## func
  name2kegg <- function(c_name) {
    c_name_encode = URLencode(c_name)
    html_page <- getURL(paste0("https://www.kegg.jp/dbget-bin/www_bfind_sub?mode=bfind&max_hit=1000&locale=en&serv=kegg&dbkey=compound&keywords=",c_name_encode,"&page=1"))
    res_kegg_name2cid = htmlParse(html_page) %>% getNodeSet(.,"//div") %>% xmlValue() 
    if(isTRUE(grepl(pattern = "Total 0 hits",x = res_kegg_name2cid[1]))) {
      out = data.frame(
        Compound_name = c_name %>% URLdecode(),
        KEGG = ""
      )
    } else {
      out = data.frame(
        Compound_name = c_name %>% URLdecode(),
        KEGG = res_kegg_name2cid[2] %>% str_extract(.,"^C\\d++")
      )
    }
    return(out)
  }
  
  #> message setting
  msg_yes = green$bold$italic;
  msg_no = red$bold$italic;
  msg_warning = yellow$bold$italic;
  message(msg_yes("Start analysis... \nConvert compound name to cid by KEGG website "))
  input <- query %>% URLencode(reserved = TRUE,repeated = T)
  batch_name2cid.kegg = function(input){
    p <- progressr::progressor(steps = length(input));
    b = furrr::future_map_dfr(.x = input,.f = function(.x) {
      Sys.sleep(0.5)
      p()
      a <- tryCatch({
        name2kegg(c_name = .x)
      },error = function(e) {
        message(msg_no('Failed'))
      })
      return(a)
    })
    return(b)
  }
  
  future::plan("multisession",workers = core_num)
  with_progress({
    KEGGID = batch_name2cid.kegg(input = input)
  })
  message(msg_yes("Done!"))
  return(KEGGID)
  }