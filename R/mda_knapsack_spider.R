#' @title mda_knapsack_spider
#' @description Get compound information by CID of KNApSAcK database.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param input_data A vector or data.frame; a data.frame with one of the column named knapsack.
#' @param type 'multiple','single', For one compound or multiple compounds.
#' @param core_num 'numeric'; how many cores you want to use
#' @return A dataframe, contains C_ID, C_name, formula, mw, InChIKey SMILE, synonyms and organism.
#' @importFrom magrittr %>%
#' @importFrom RCurl getURL
#' @importFrom XML getNodeSet xpathApply xmlValue readHTMLTable htmlParse
#' @importFrom tidyr drop_na  
#' @importFrom dplyr select mutate distinct pull filter case_when
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
#' xx <- data.frame(knapsack=c(""))
#' # data orgnazation
#' orz_data = mda_data_org(compound_info = xx, source = "CD")
#' # Get cid (batch)
#' name2cid = mad_get_cid(data_info = orz_data)
#' # please wait for a while
#' }
mda_knapsack_spider <- function(input_data,type = "multiple",core_num = 8) {
  #> message setting
  msg_yes = green$bold$italic;
  msg_no = red$bold$italic;
  msg_warning = yellow$bold$italic;
  message(msg_yes("---------------------------------------------------------\nStart analysis....\nGet compound information from KNApSAcK database. please wait...\n--------------------------------------------------------- "))
  #> functions
  #> name2cid
  name2cid_fun = function(x) {
    tbl <- 
      data.frame(
        Lab.ID = x,
        CAS.ID = NA,
        Compound_name = NA,
        Formula = NA,
        mw = NA,
        organism = NA,
        synonyms = NA,
        Judge_plant = FALSE,
        InChIKey = NA,
        SMILE = NA,
        Species_all = NA
      )
    tryCatch({
      ID = x
      url <- "http://www.knapsackfamily.com/knapsack_core/result.php?sname=all&word="
      
      # fetch the HTML content of the page
      html <- RCurl::getURL(paste0(url,ID))
      
      # parse the HTML using XML package
      doc <- XML::htmlParse(html)
      
      # find the table on the page
      table <- XML::getNodeSet(doc, "//table")[[1]]
      
      cell <- XML::xpathApply(table,"//td")
      metabolite <- XML::xpathApply(table, "//td[3]/node()")[[1]]
      
      Lab.ID <- XML::xmlValue(cell[[1]])
      CAS.ID <- XML::xmlValue(cell[[2]])
      Compound_name <- XML::xpathApply(table, "//td[3]/node()")[[1]] %>% XML::xmlValue()
      Formula <- XML::xmlValue(cell[[4]])
      mw <- XML::xmlValue(cell[[5]])
      organism <- XML::xmlValue(cell[[6]])
      synonyms <- XML::xpathApply(table, "//td[3]/node()") %>% 
        sapply(., function(x) ifelse(XML::xmlName(x)=="br", NA, XML::xmlValue(x))) %>% 
        na.omit %>% 
        paste(.,collapse = "|") 
      
      url2 <- "http://www.knapsackfamily.com/knapsack_core/information.php?word="
      html <- RCurl::getURL(paste0(url2,ID))
      parsed_html <- XML::htmlParse(html, asText = TRUE)
      table_nodes <- XML::getNodeSet(parsed_html, "//table")  # 选择所有表格节点
      table <- XML::readHTMLTable(table_nodes[[2]])  # 提取第一个表格节点的表格数据
      tbl = data.frame(
        Lab.ID = Lab.ID,
        CAS.ID = CAS.ID,
        Compound_name = Compound_name,
        Formula = Formula,
        mw = mw,
        organism = organism,
        synonyms = synonyms,
        Judge_plant = case_when(
          str_detect(table[9,2],"Plantae") ~ TRUE,
          TRUE ~ FALSE
        ),
        InChIKey = table[5,2],
        SMILE = table[7,2],
        Species_all = table %>% pull(V3) %>% na.omit() %>% paste(.,collapse = "|")
      )
    },error = function(e) {
      message(msg_no(x," no match results\n"))
    })
  }
  #> multicore webspider
  multi_run_fun = function(name_list) {
    name <- name_list %>% pull(knapsack_id)
    p <- progressr::progressor(steps = length(name));
    b = furrr::future_map_dfr(.x = name,.f = function(.x) {
      Sys.sleep(0.1);
      p()
      a <- name2cid_fun(
        x = .x
      )
      return(a)
    });
    # b <- b %>% dplyr::filter(!is.na(Compound_name))
    return(b)
  }
  #> run single search
  if(type == 'single') {
    cid_tbl <- name2cid_fun(
      x = input_data
    )
    message(msg_yes("Done!"))
  }
  #> run multiple search
  if(type == 'multiple') {
    #> check format
    if ("knapsack_id" %in% colnames(input_data) ) {
      Name_clean = input_data %>% 
        select(knapsack_id) %>% 
        distinct() %>% 
        drop_na() %>% 
        mutate(order = seq(1:nrow(.)))
    } else {
      return();
      message(msg_no('Error in input data infomation, please set the colname of kanpsack id as "knapsack_id"'))
    } 
    #> run
    future::plan("multisession", workers = core_num)
    with_progress({
      kns_tbl = multi_run_fun(name_list = Name_clean)
    })
    message(msg_yes("Done!"))
    return(kns_tbl)
  }
}


