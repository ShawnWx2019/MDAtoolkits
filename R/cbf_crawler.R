#' @title cbf_crawler
#' @description Compound classyfication based on cbf of Fiehn lab.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param query 'character', "A vector consisting of one or more InChIKeys. 
#' @param delay_max 'number', "crawl delay time, defult = 1, To prevent excessive load on the website, we set the minimum value of the crawl delay to 0.5 seconds, and the maximum value represents a random variation in the crawl delay within the range of 0.5 seconds to that value.
#' @return A dataframe, contains InChIKey, kingdom, superclass, class, subclass parent_levels and description.
#' @importFrom magrittr %>%
#' @importFrom RCurl getURL
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate case_when
#' @importFrom progressr progressor handlers with_progress
#' @importFrom purrr map2_dfr
#' @importFrom crayon green bold italic red yellow
#' @references See https://cfb.fiehnlab.ucdavis.edu/
#' @export
#' @examples  
#' \dontrun{
#' # demo
#' xx <- c("RVQCZHZIMZMGAD-IDFNXBDGNA-N","UKMSUNONTOPOIO-UHFFFAOYSA-N","DPUOLQHDNGRHBS-KTKRTIGZSA-N")
#' library(progressr)
#' handlers(global = FALSE)
#' handlers(handler_pbcol(
#'  adjust = 1.0,
#'  complete = function(s) cli::bg_red(cli::col_black(s)),
#'  incomplete = function(s) cli::bg_cyan(cli::col_black(s))
#' ))
#' # run cbf_crawler
#' classyfire_tbl = cbf_crawler(query = xx, delay_max = 2)
#' }
cbf_crawler <- function(query,delay_max = 1) {
  #> message setting
  msg_yes = green$bold$italic;
  msg_no = red$bold$italic;
  msg_warning = yellow$bold$italic;
  message(msg_yes("---------------------------------------------------------\nStart analysis....\nGet compound classyfication from cbf database, please wait...\n--------------------------------------------------------- "))
  #> functions
  #> Get information
  get_cbf <- function(InChIKey) {
    InChIKey.raw <- InChIKey
    url_raw = "https://cfb.fiehnlab.ucdavis.edu/entities/"
    url = paste0(url_raw,InChIKey,".json")
    xx <- RCurl::getURL(url = url)
    #> transfer josn to dataframe, but the result is, all elements were stored at a list.
    xxx <- jsonlite::fromJSON(xx,simplifyDataFrame = T) 
    #> assignment variables.
    InChIKey <- xxx$inchikey %>% str_remove("InChIKey=")
    CTS_kindom <- xxx$kingdom$name 
    CTS_superC <- xxx$superclass$name 
    CTS_Class <- xxx$class$name 
    CTS_SubC <- xxx$subclass$name
    CTS_desc <- xxx$description 
    CTS_inter_node <- xxx$intermediate_nodes$name 
    CTS_dir_parent <- xxx$direct_parent$name
    #> make a fake list for parent_levels judge.
    tmp_x = list(
      InChIKey,
      CTS_kindom,
      CTS_superC,
      CTS_Class,
      CTS_SubC,
      CTS_inter_node,
      CTS_dir_parent,
      CTS_desc
    )
    df_out = map(.x = tmp_x,function(.x){
      if(length(.x) > 0) 
      {return(.x)} else
      {return("NA")}
    })
    x_df <- data.frame(
      InChIKey = InChIKey.raw,
      kingdom = df_out[[2]],
      superclass = df_out[[3]],
      class = df_out[[4]],
      subclass = df_out[[5]],
      parent_levels = paste(c(df_out[[6]],df_out[[7]]),collapse = " | "),
      description = df_out[[8]]
    ) %>% 
      mutate(parent_levels = case_when(
        is.na(superclass) ~ NA,
        is.na(class) ~ NA,
        is.na(subclass) ~ NA,
        TRUE ~ parent_levels
      )) %>% 
      mutate(parent_levels = case_when(
        str_detect(parent_levels,"NA") ~ NA,
        TRUE ~ parent_levels
      ))
    return(x_df)
  }
  #> functional iteration
  run_cbf <- function(query,delay_max) {
    p <- progressr::progressor(along = query);
    round_n = length(query);
    result = purrr::map2_dfr(.x = query,.y = c(1:round_n),.f = function(.x,.y) {
      #> set delay time
      s.time = sample(runif(n = 50,0.5,delay_max) %>% round(.,digits = 2),1)
      Sys.sleep(s.time);
      #> progress bar
      p(sprintf("%s",paste0(.x,"(",.y,"/",round_n,")")))
      #> Default output
      out =  data.frame(
        InChIKey = .x,
        kingdom = NA,
        superclass = NA,
        class = NA,
        subclass = NA,
        parent_levels = NA,
        description = NA
      ) 
      tryCatch({
        out = get_cbf(InChIKey = .x)
        return(out)
      },error = function(e) {
        message(msg_no("Error occurred: ", .x, "failed!\n"))
        NULL
      })
      return(out)
    })
  }
  #> run
  with_progress({
    output_file = run_cbf(query = query,delay_max = delay_max)
  })
  message(msg_yes("Done!"))
  return(output_file)
}


