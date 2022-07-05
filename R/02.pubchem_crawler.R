
# pubchem crawler -----------------------------------------------------------

#' @title mda_pubchem_crawler.
#' @description Get compound information according the cid by webchem.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param cid_info 'character' or 'data.frame'; For single cid, cid_info equals the pubchem CID, For multiple cid, cid_info is a dataframe generated from mda_get_cid.
#' @param type "single','multiple';  For one compound or multiple compounds.
#' @param core_num 'numeric'; how many cores you want to use
#' @return pubchem_item An data frame about stored the pubchem information of input cids.
#' @importFrom webchem pc_prop
#' @importFrom dplyr mutate 
#' @importFrom magrittr %>%
#' @importFrom future plan
#' @importFrom furrr future_map_dfr
#' @importFrom crayon green bold italic red yellow
#' @references See webchem https://github.com/ropensci/webchem.
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
#' }

mda_pubchem_crawler = function(cid_info,type = "multiple",core_num = 8) {
  #> message setting
  msg_yes = green$bold$italic;
  msg_no = red$bold$italic;
  msg_warning = yellow$bold$italic;
  message(msg_yes("Start analysis....\n Get information from pubchem by 'CID'. "))
  #> functions
  ## rebulid pc_prop for purrr or furrr
  pc_prop_progress = function(cid) {
    tryCatch({
      a =  webchem::pc_prop(cid = cid,properties = c("MolecularFormula", "MolecularWeight","InChIKey", 
                                                     "IUPACName","ExactMass"))
      return(a)
    },error = function(e){
      message(msg_no(cid," | failed, check your net connection!"))
    })
  }
  #> multirun
  multi_run_pubchem <- function(tbl) {
    a.cid <- tbl %>% pull(cid);
    p <- progressr::progressor(steps = length(a.cid));
    b = furrr::future_map_dfr(.x = a.cid,.f = function(.x) {
      Sys.sleep(0.1);
      p()
      a <- pc_prop_progress(cid = .x)
      return(a)
    })
    return(b)
  }
  
  ## run
  if (type == "single" & length(cid_info) == 1) {
    tmp_cid_char = cid_info
    tmp_pubchem = webchem::pc_prop(cid = tmp_cid_char,properties = c("MolecularFormula", "MolecularWeight","InChIKey", 
                                                                     "IUPACName","ExactMass"))
    return(tmp_pubchem)
  } else if (type == "multiple") {
    future::plan("multisession",workers = core_num )
    with_progress({
      tmp_pubchem = multi_run_pubchem(tbl = cid_info)
    })
    return(tmp_pubchem)
  } else {
    message(msg_no("Wrong parameters, please check carefully!"))
  }
}


#' @title mda_pubchem_crawler_sh.
#' @description Get compound information according the cid by webchem.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param cid 'character'; pubchem cid
#' @return pubchem_item An data frame about stored the pubchem information of input cids.
#' @importFrom  webchem pc_prop
#' @references See webchem https://github.com/ropensci/webchem.


mda_pubchem_crawler_sh = function(cid) {
  a = tibble(
    CID = cid,
    MolecularFormula = NA,
    MolecularWeight = NA,
    InChIKey = NA,
    IUPACName = NA,
    ExactMass =NA,
  )
  tryCatch({
    a = webchem::pc_prop(cid = cid,properties = c("MolecularFormula", "MolecularWeight","InChIKey", 
                                                  "IUPACName","ExactMass"))},error = function(e) {
    }
  )
  if (is.na(a$MolecularFormula[1])) {message(paste(a$CID[1],"failed to match pubchem!\n"))} else {
    message(paste(a$query[1],"success to match pubchem!\n"))
  }
  return(a)
}


