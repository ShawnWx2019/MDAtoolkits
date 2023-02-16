
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
#' @param core_num 'numeric'; how many cores you want to use
#' @param from 'character'; see webchem::get_cid
#' @return cid_tbl A table which webchem::get_cid generated.
#' @importFrom magrittr %>%
#' @importFrom webchem get_cid
#' @importFrom dplyr distinct
#' @importFrom tidyr drop_na
#' @importFrom future plan
#' @importFrom progressr  progressor
#' @importFrom furrr future_map_dfr
#' @importFrom purrr map_df
#' @importFrom crayon green bold italic red yellow
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

mda_get_cid = function(data_info,type = "multiple",match = "all",core_num = 8,from="name") {
  #> message setting
  msg_yes = green$bold$italic;
  msg_no = red$bold$italic;
  msg_warning = yellow$bold$italic;
  message(msg_yes("Start analysis....\n convert compound names to cid "))
  #> functions
  #> name2cid
  name2cid_fun = function(x,y) {
    cid_tbl = tibble(
      query = x,
      cid = NA
    )
    tryCatch(
      {
        cid_tbl = get_cid(
          query = x,
          from = from,
          domain = "compound",
          match = y
        )
      },error = function(e) {
        message(msg_no(x," no match results\n"))
      }
    )
  }
  #> multicore webspider
  multi_get_cid_fun = function(name_list) {
    name <- name_list %>% pull(name)
    p <- progressr::progressor(steps = length(name));
    b = furrr::future_map_dfr(.x = name,.f = function(.x) {
      Sys.sleep(0.1);
      p()
      a <- name2cid_fun(
        x = .x,
        y = match
      )
      return(a)
    });
    b <- b %>% filter(!is.na(cid))
    return(b)
  }
  #> run single search
  if(type == 'single') {
    cid_tbl <- name2cid_fun(
      x = data_info,
      y = match
    )
    message(msg_yes("Done!"))
  }
  #> run multiple search
  if(type == 'multiple') {
    #> check format
    if ("name" %in% colnames(data_info) ) {
      Name_clean = data_info %>% 
        select(name) %>% 
        distinct() %>% 
        drop_na() %>% 
        mutate(order = seq(1:nrow(.)))
    } else {
      return();
      message(msg_no('Error in input data infomation, please organization your data by MDAtoolkits::mda_data_org()!'))
    } 
    #> run
    future::plan("multisession", workers = core_num)
    with_progress({
      cid_tbl = multi_get_cid_fun(name_list = Name_clean)
    })
    message(msg_yes("Done!"))
    return(cid_tbl)
  }
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


