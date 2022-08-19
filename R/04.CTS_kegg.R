
# KEGG pathway  -----------------------------------------------------------


#' @title mda_CTS_kegg.
#' @description Get kegg cid by CTS
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param query 'character' or 'data.frame'; For single InChiKey, query equals the InChiKey, For multiple InChiKeys, query is a data frame from mda_pubchem_crawler.
#' @param type "single','multiple';  For one compound or multiple compounds.
#' @param core_num 'numeric'; how many cores you want to use
#' @return KEGGID InChikey 2 kegg cid.
#' @importFrom magrittr %>%
#' @importFrom stringr str_split str_extract
#' @importFrom rvest read_html html_nodes html_text
#' @importFrom dplyr mutate select
#' @importFrom future plan
#' @importFrom furrr future_map_dfr
#' @importFrom crayon green bold italic red yellow
#' @export

mda_CTS_kegg = function(query,type = "multiple",core_num = 8) {
  #> message setting
  msg_yes = green$bold$italic;
  msg_no = red$bold$italic;
  msg_warning = yellow$bold$italic;
  message(msg_yes("Start analysis... Get KEGG annotation via InChIKey. "))
  CTS_url = "https://cts.fiehnlab.ucdavis.edu/rest/convert/InChIKey/KEGG/"
  ## get kegg via InChikey
  InChikey2kegg = function(url){
    a = read_html(url) %>%
      html_nodes("p") %>% html_text() %>%
      gsub(pattern = '\"',replacement = "",x = .) %>%
      str_split(.,pattern = ",",n = 4,simplify = T) %>%
      as.data.frame() %>%
      mutate(
        InChIkey = gsub("searchTerm:","",V3),
        KEGG = str_extract(V4,pattern = "C\\d+")
      ) %>%
      select(InChIkey,KEGG)
    return(a)
  }
  InChikey2kegg_run = function(para_url) {
    p <- progressr::progressor(steps = length(para_url));
    b = furrr::future_map_dfr(.x = para_url,.f = function(.x) {
      Sys.sleep(0.5)
      p()
      a <- tryCatch({
        InChikey2kegg(url = .x)
      },error = function(e) {
        message(msg_no('Failed'))
      })
      return(a)
    })
    return(b)
  }
  if (type == "multiple") {
    Inchikey = query %>%
      mutate(url = paste0(CTS_url,InChIKey)) %>%
      filter(InChIKey != "NA") %>% 
      select(url) %>% unique()
  } else if (type == "single"){
    Inchikey = data.frame(
      url = paste0(url,query)
    )
  }
  Inchikey = Inchikey$url
  future::plan("multisession",workers = core_num)
  with_progress({
    KEGGID = InChikey2kegg_run(para_url = Inchikey)
  }
  )
  message(msg_yes("Done!"))
  return(KEGGID)
}

#' @title InChikey2kegg
#' @description Get kegg cid by CTS
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param query 'character' or 'data.frame'; For single InChiKey, query equals the InChiKey, For multiple InChiKeys, query is a data frame from mda_pubchem_crawler.
#' @return KEGGID InChikey 2 kegg cid.
#' @importFrom magrittr %>%
#' @importFrom stringr str_split str_extract
#' @importFrom rvest read_html html_nodes html_text
#' @importFrom dplyr mutate select


InChikey2kegg = function(url){
  Sys.sleep(0.5)
  a = read_html(url) %>%
    html_nodes("p") %>% html_text() %>%
    gsub(pattern = '\"',replacement = "",x = .) %>%
    str_split(.,pattern = ",",n = 4,simplify = T) %>%
    as.data.frame() %>%
    mutate(
      InChIkey = gsub("searchTerm:","",V3),
      KEGG = str_extract(V4,pattern = "C\\d+")
    ) %>%
    select(InChIkey,KEGG)
  return(a)
}
