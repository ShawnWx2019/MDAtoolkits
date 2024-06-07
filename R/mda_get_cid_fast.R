#' @title mda_get_cid_fast.
#' @description Data organization of input data.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param query A single compound name or multiple compounds
#' @param probe 'character', form of query, name or inchikey.
#' @param core_num 'numeric'; how many cores you want to use
#' @return cid_tbl contains cid and InChIKey.
#' @importFrom magrittr %>%
#' @importFrom dplyr distinct
#' @importFrom tidyr drop_na
#' @importFrom RCurl getURL
#' @importFrom jsonlite fromJSON
#' @importFrom future plan
#' @importFrom progressr progressor
#' @importFrom furrr future_map2_dfr
#' @importFrom purrr map_df map2_df
#' @importFrom crayon green bold italic red yellow
#' @importFrom tibble tibble
#' @references See webchem https://github.com/ropensci/webchem
#' @export
#' @examples  
#' \dontrun{
#' # import file
#' xx <- read.csv("cd_result.csv")
#' # data orgnazation
#' orz_data = mda_data_org(compound_info = xx, source = "CD")
#' # Get cid (batch)
#' name2cid = mad_get_cid(query = orz_data)
#' # please wait for a while
#' }
#' 

mda_get_cid_fast = function(query,probe = "name",core_num = 1) {
  #> message setting
  msg_yes = green$bold$italic;
  msg_no = red$bold$italic;
  msg_warning = yellow$bold$italic;
  message(msg_yes("Start analysis....\n convert your query id to cid and InChIKey"))
  #> functions
  #> name2cid
  name2cid_fun = function(x) {
    cid_tbl = tibble(
      query = x %>% URLdecode(),
      cid = NA,
      formula = NA,
      mw = NA,
      InChIKey = NA
    )
    tryCatch(
      {
        url = paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/",probe,"/",
                     x, "/property/MolecularFormula,InChIKey,MolecularWeight/json")
        response <- getURL(url)
        tmp = fromJSON(response)
        tmp2 = tmp$PropertyTable$Properties
        cid_tbl = data.frame(
          query = x %>% URLdecode(),
          cid = tmp2[1,1],
          Formula = tmp2[1,2],
          mw = tmp2[1,3],
          InChIKey = tmp2[1,4]
        )
        
      },error = function(e) {
        message(msg_no(x," no match results\n"))
      }
    )
    return(cid_tbl)
  }
  #> multicore webspider
  multi_get_cid_fun = function(x) {
    x <- x 
    p <- progressr::progressor(along = x);
    round_n = length(x);
    b = furrr::future_map2_dfr(.x = x,.y = c(1:round_n),.f = function(.x,.y) {
      Sys.sleep(0.1);
      p(sprintf("%s",paste0(.x,"(",.y,"/",round_n,")")))
      a <- name2cid_fun(
        x = .x
      )
      return(a)
    });
    b <- b %>% filter(!is.na(cid))
    return(b)
  }
  #> check format
  Name_clean = URLencode(query)
  #> run
  future::plan("multisession", workers = core_num)
  with_progress({
    cid_tbl = multi_get_cid_fun(x = Name_clean)
  })
  message(msg_yes("Done!"))
  return(cid_tbl)
}

