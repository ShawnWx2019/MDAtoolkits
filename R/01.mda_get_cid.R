
# Data cleaning -----------------------------------------------------------


#' @title mda_data_org.
#' @description Data organization of input data.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param compound_info A compound table input. contains at least five columns. 1. Customized Compound_ID, 2. compound name, 3. mz, 4. mf, 5.RT, for CD result add Judge.
#' @param source Input compound information from Compound discovery or other software.
#' @return cid_tbl An organized data for following steps.
#' @export
#' @examples  
#' \dontrun{
#' # import file
#' xx <- read.csv("cd_result.csv")
#' # data orgnazation
#' orz_data = mda_data_org(compound_info = xx, source = "CD")
#' }

mda_data_org = function(compound_info, source = "CD") {
  compound_info = as.data.frame(compound_info)
  ## org
  tryCatch({
    if(source == "CD") {
      data_info = data.frame(
        Compound_ID = compound_info$Compound_ID,
        name = compound_info$Name,
        mf = gsub(" ","",compound_info$Formula),
        mw = compound_info$`Calc  MW`,
        RT = compound_info$`RT [min]`,
        Judge = compound_info$Judge
      )
    } else {
      data_info = data.frame(
        Compound_ID = compound_info[,1],
        name = compound_info[,2],
        mf = compound_info[,3],
        mw = compound_info[,4],
        RT = compound_info[,5],
        Judge = ""
      )
    }
  }, error = function(e){
      cat("Error in input data, please check the input data!")
    } 
  )
  return(data_info)
}



# get_cid -----------------------------------------------------------------


#' @title mda_get_cid
#' @description Get cid by compound name, single or multiple.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param data_info A vector or data.frame; For single compound, just set the compound name as data_info, for multipule compounds, use organized data by mda_data_org.
#' @param match 'all','first'; Check webchem::get_cid match, return all matched cid or the first match, default 'all' .
#' @param type 'multiple','single', For one compound or multiple compounds.
#' @param multi_core 'TRUE' or 'FALSE'; Run web crawler with multicore or not. 
#' @return cid_tbl A table which webchem::get_cid generated.
#' @importFrom magrittr %>%
#' @importFrom webchem get_cid
#' @importFrom dplyr distinct
#' @importFrom tidyr drop_na
#' @importFrom future plan
#' @importFrom progressr handlers progressor
#' @importFrom furrr future_map_dfr
#' @importFrom purrr map_df
#' @references See webchem https://github.com/ropensci/webchem
#' @export
#' @examples  
#' \dontrun{
#' # import file
#' xx <- read.csv("cd_result.csv")
#' # data orgnazation
#' orz_data = mda_data_org(compound_info = xx, source = "CD")
#' # Get cid (batch)
#' name2cid = mad_get_cid(data_info = orz_data)
#' # please wait for a while
#' }

mda_get_cid = function(data_info,type = "multiple",match = "all",multi_core = TRUE) {
  ## get cid
  cat("Start analysis")
  shawn_get_cid = function(name) {
    Sys.sleep(0.2)
    p(sprintf("name=%s", name))
    a = tibble(
      query = name,
      cid = NA
    )
    tryCatch({
      a = get_cid(
        query = name,
        from = "name",
        domain = "compound",
        match = match 
      )
    },error=function(e) {
      cat(paste0(name," no match results\n"))
    }
    )
    return(a)
  }
  ## run
  if (type == "single") {
    name = data_info
    cid_tbl = shawn_get_cid(name)
    return(cid_tbl)
  } else if (type == "multiple" ){
    if (colnames(data_info) == c("Compound_ID","name","mf","mw","RT","Judge") ) {
      Name_clean = data_info %>% 
        select(name) %>% 
        distinct() %>% 
        drop_na()
    } else {
      return()
      cat("Error in input data infomation, please organization your data by MDAtoolkits::mda_data_org()!")
    }
    if (isTRUE(multi_core)) { 
      future::plan("multisession")
      handlers(global = T)
      handlers("progress")
      multi_cor_cid = function(xs){
        p<- progressor(along = xs)
        future_map_dfr(xs,shawn_get_cid) %>% 
          filter(!is.na(cid))
      }
      cid_tbl = multi_cor_cid(Name_clean)
    } else {
      single_cor_cid = function(xs){
        p<- progressor(along = xs)
        purrr::map_df(xs,shawn_get_cid) %>% 
          filter(!is.na(cid))
      }
      cid_tbl = single_cor_cid(Name_clean)
    }
    return(cid_tbl)
  } else {
    return()
    cat("Wrong type, Just accept 'single' or 'multiple'!")
  }
  cat("Finish!")
}


#' @title shawn_get_cid_sh
#' @description get cid for shinyapp
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param name compound name.
#' @param mod match model; see webchem .
#' @return cid_tbl A table which webchem::get_cid generated.
#' @importFrom webchem get_cid
#' @references See webchem https://github.com/ropensci/webchem
#' @export
#' @examples  
#' \dontrun{
#' # import file
#' xx <- read.csv("cd_result.csv")
#' # data orgnazation
#' orz_data = mda_data_org(compound_info = xx, source = "CD")
#' # Get cid (batch)
#' name2cid = mad_get_cid(data_info = orz_data)
#' # please wait for a while
#' }

shawn_get_cid_sh = function(name,mod) {
  a = tibble(
    query = name,
    cid = NA
  )
  tryCatch({
    a = get_cid(
      query = name,
      from = "name",
      domain = "compound",
      match = mod
    )},error = function(e) {
    }
  )
  if (is.na(a$cid[1])) {message(paste(a$query[1],"failed to match pubchem!\n"))} else {
    message(paste(a$query[1],"success to match pubchem!\n"))
  }
  return(a)
}

