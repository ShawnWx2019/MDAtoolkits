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
#' @importFrom dplyr select mutate distinct pull case_when
#' @importFrom progressr progressor
#' @importFrom purrr map_df
#' @importFrom crayon green bold italic red yellow
#' @references See http://www.knapsackfamily.com/knapsack_core/top.php
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
      ## use different user_agents.
      user_agents = c(
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.3 Safari/605.1.15",
        'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/110.0',
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.5060.53 Safari/537.36 Edg/103.0.1264.37',
        'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.5060.53 Safari/537.36 Edg/103.0.1264.37',
        'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.5060.53 Safari/537.36',
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36',
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:101.0) Gecko/20100101 Firefox/101.0'
      )
      random_agent <- sample(user_agents,1)
      
      headers = c('User_Agent' = random_agent,'Accept' = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
      url <- "http://www.knapsackfamily.com/knapsack_core/result.php?sname=all&word="
      
      # set random parameters, in case the same requre for several times.
      set.seed(1234)
      random_param <- sample(1:100,1)
      
      # fetch the HTML content of the page
      html <- RCurl::getURL(url = paste0(url,ID,"?random=", random_param),header = headers)
      
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
      html <- RCurl::getURL(url = paste0(url2,ID,"?random=", random_param),header = headers)
      
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
  
  multi_run_fun = function(name_list) {
    name <- name_list %>% pull(knapsack_id)
    p <- progressr::progressor(steps = length(name));
    repeat {
      b <- tryCatch({
        b <- purrr::map_dfr(.x = name, .f = function(.x) {
          s.time = sample(runif(n = 50,1,3) %>% round(.,digits = 2),1)
          Sys.sleep(s.time);
          p()
          a <- name2cid_fun(x = .x)
          return(a)
        })
        return(b)
      }, error = function(e) {
        message("Error occurred: ", conditionMessage(e), "\n")
        NULL
      })
      if (!is.null(b)) {
        if (nrow(b) == length(name)) {
          break
        } else {
          missing_names <- setdiff(name, b$Lab.ID)
          message(paste0("Missing ", length(missing_names), " entries, retrying..."))
          name <- missing_names
        }
      } else {
        message("Retrying...")
      }
    }
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
    with_progress({
      kns_tbl = multi_run_fun(name_list = Name_clean)
    })
    message(msg_yes("Done!"))
    return(kns_tbl)
  }
}


