
# KEGG pathway  -----------------------------------------------------------


#' @title mda_CTS_kegg.
#' @description Get kegg cid by CTS
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param query 'character' or 'data.frame'; For single InChiKey, query equals the InChiKey, For multiple InChiKeys, query is a data frame from mda_pubchem_crawler.
#' @param type "single','multiple';  For one compound or multiple compounds.
#' @return KEGGID InChikey 2 kegg cid.
#' @importFrom magrittr %>%
#' @importFrom stringr str_split str_extract
#' @importFrom rvest read_html html_nodes html_text
#' @importFrom dplyr mutate select
#' @importFrom future plan
#' @importFrom furrr future_map_dfr
#' @export
#' @examples 
#' \dontrun{
#' # import file
#' xx <- read.csv("cd_result.csv")
#' # data orgnazation
#' orz_data = mda_data_org(compound_info = xx, source = "CD")
#' # Get cid (batch)
#' name2cid = mda_get_cid(data_info = orz_data)
#' # please wait for a while
#' pubchem_detail = mda_pubchem_crawler(cid_info = name2cid,type = "multiple",multi_core = T)
#' pubchem_class = mda_classfire(query = pubchem_detail,type = "multiple")
#' final_tbl = mda_merge_info(orz_data,name2cid,pubchem_detail,pubchem_class)
#' kegg = mda_CTS_kegg(query = final_tbl,type = "multiple)
#' }

mda_CTS_kegg = function(query,type = "multiple") {
  CTS_url = "https://cts.fiehnlab.ucdavis.edu/rest/convert/InChIKey/KEGG/"
  ## get kegg via InChikey
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
  future::plan("multisession")
  KEGGID = future_map_dfr(Inchikey,tryCatch({InChikey2kegg},error = function(e) { print("failed")}))
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
