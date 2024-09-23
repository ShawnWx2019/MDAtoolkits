#####################################
#       Prj: MDAtoolkits
#       Assignment: archived scripts
#       Author：Shawn
#       Version：0.0.2
#####################################


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

mda_pubchem_crawler = function(cid_info,type = "multiple",core_num = 1) {
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



# Classification  -----------------------------------------------------------


#' @title mda_classfire.
#' @description Compound classification via classyfireR
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param query 'character' or 'data.frame'; For single InChiKey, query equals the InChiKey, For multiple InChiKeys, query is a data frame from mda_pubchem_crawler.
#' @param type "single','multiple';  For one compound or multiple compounds.
#' @return pubchem_item An data frame about stored the pubchem information of input cids.
#' @importFrom dplyr select mutate bind_rows as_tibble
#' @importFrom tidyr pivot_wider drop_na
#' @importFrom magrittr %>%
#' @importFrom purrr map map_df
#' @importFrom crayon green bold italic red yellow
#' @importFrom classyfireR get_classification classification
#' @references See classyfireR https://github.com/aberHRML/classyfireR
#' @export

mda_classfire = function(query,type = "multiple") {
  #> message setting
  msg_yes = green$bold$italic;
  msg_no = red$bold$italic;
  msg_warning = yellow$bold$italic;
  message(msg_yes("Start analysis....\n Get compound classification by InChIkey. "))
  ## rebuild function for purrr 
  shawn_get_classification = function(id){
    Sys.sleep(0.1)
    get_classification(inchi_key = id)
  }
  ## convert classyfireR result to tbl
  classfireR2tbl = function(x){
    # make a black df
    if (is.null(x)) {
      tmp_x = data.frame(
        InchIkeys = NA,
        Level = NA,
        Classification = NA
      ) %>% as_tibble()
    } else if(is.null(x@meta$inchikey)) {  # make na values as blank
      tmp_x = data.frame(
        InchIkeys = NA,
        Level = NA,
        Classification = NA)
    } else {
      tmp_x = classification(x) %>% 
        mutate(InchIkeys = gsub("InChIKey=","",meta(x)$inchikey)) %>% 
        dplyr::select(InchIkeys,Level,Classification)
    }
    return(tmp_x)
  }
  # run
  if(type == "single" & length(query) == 1) {
    InchIKeys = query
  } else {
    InchIKeys = query$InChIKey
  }
  # tryCatch(
  #   {
  #     cat("Start processsing...")
  #     classification_list = map(InchIKeys,shawn_get_classification)
  #     # convert as classyfireR2tbl
  #     result_tbl = purrr::map_df(classification_list,classfireR2tbl)
  #     # convert long table to wide table
  #     class_out = bind_rows(result_tbl) %>%
  #       drop_na() %>% unique() %>%
  #       pivot_wider(names_from = Level,values_from = Classification)
  #   },error = function(e) {
  #     cat("Please check internet connection!")
  #   }
  # )
  cat("Start processsing...")
  classification_list = map(InchIKeys,shawn_get_classification)
  # convert as classyfireR2tbl
  result_tbl = purrr::map_df(classification_list,classfireR2tbl)
  # convert long table to wide table
  class_out = bind_rows(result_tbl) %>%
    drop_na() %>% unique() %>%
    pivot_wider(names_from = Level,values_from = Classification)
  return(class_out)
}


#' @title classfireR2tbl
#' @description merge classyfireR result
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param x 'character' ; result of shawn_get_classification
#' @return pubchem_item An data frame about stored the pubchem information of input cids.
#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_wider drop_na
#' @importFrom magrittr %>%
#' @importFrom purrr map map_df
#' @references See classyfireR https://github.com/aberHRML/classyfireR
#' @export
classfireR2tbl = function(x){
  # make a black df
  if (is.null(x)) {
    tmp_x = data.frame(
      InchIkeys = NA,
      Level = NA,
      Classification = NA
    ) %>% as_tibble()
  } else if(is.null(x@meta$inchikey)) {  # make na values as blank
    tmp_x = data.frame(
      InchIkeys = NA,
      Level = NA,
      Classification = NA)
  } else {
    tmp_x = classification(x) %>% 
      mutate(InchIkeys = gsub("InChIKey=","",meta(x)$inchikey)) %>% 
      select(InchIkeys,Level,Classification)
  }
  return(tmp_x)
}


# Table merge  -----------------------------------------------------------


#' @title mda_merge_info
#' @description Merge all information.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email {shawnwang2016@@126.com}.
#' @param orz_data 'dataframe'; generated by function mda_data_org.
#' @param name2cid 'dataframe'; generated by function mda_get_cid.
#' @param pubchem_detail 'dataframe'; generated by function mda_pubchem_crawler.
#' @param pubchem_class 'dataframe'; generated by function mda_classfire.
#' @return pubchem_item An data frame about stored the pubchem information of input cids.
#' @importFrom dplyr mutate left_join right_join relocate
#' @importFrom tidyr pivot_wider drop_na
#' @importFrom magrittr %>%
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
#' }
#' 

mda_merge_info = function(orz_data,name2cid,pubchem_detail,pubchem_class,CTS_kegg){
  name2cid$cid = as.character(name2cid$cid)
  pubchem_detail$CID = as.character(pubchem_detail$CID)
  result_out = name2cid %>% 
    left_join(.,pubchem_detail,by = c("cid" = "CID") )%>% 
    left_join(.,pubchem_class,by = c("InChIKey"="InchIkeys")) %>% 
    right_join(.,orz_data,by = c("query" = "name") ) %>% 
    left_join(CTS_kegg,by = c("InChIKey" = "InChIkey")) %>% 
    mutate(
      MolecularFormula = gsub(" ","",MolecularFormula),
      MolecularWeight = as.numeric(MolecularWeight),
      mw = as.numeric(mw),
      Check_MF = case_when(
        mf == MolecularFormula ~ TRUE,
        TRUE ~ FALSE
      ),
      Check_MW = case_when(
        abs(mw - MolecularWeight) <= 5 ~  TRUE,
        TRUE ~ FALSE
      ),
      High_identical = case_when(
        Check_MF == TRUE & Check_MW == TRUE ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    distinct() %>% 
    relocate(compound_id,.before = query) %>% 
    as.data.frame()
  
  return(result_out)
}


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



# PCA   -----------------------------------------------------------


#' @title mda_PCA
#' @description Get kegg cid by CTS
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param mat 'Dataframe' or 'matrix'; with Compound_ID as 1st column,.
#' @param type "single','multiple';  For one compound or multiple compounds.
#' @return KEGGID InChikey 2 kegg cid.
#' @importFrom PCAtools pca biplot
#' @importFrom ggplotify as.ggplot
#' @export


mda_pca = function(mat,group,interactive=F,removeVar = 0.1,logTransfer = F,title = "test") {
  check_title = colnames(mat)[1]
  if(check_title != "Compound_ID") {
    message("Please set Compound_ID in 1st column, and set the 1st column name as 'Compoun_ID'. and try again")
    return()
  }
  mat %>% 
    column_to_rownames(Compound_ID) -> mat_x
  if(isTRUE(logTransfer)) {
    mat_x = log10(mat_x)
  } 
  
  pca_mat <- PCAtools::pca(mat = mat_x,metadata = group,scale = T,removeVar = removeVar)
  plt_bi = PCAtools::biplot(
    pcaobj = pca_mat,
    legendPosition = "top",
    colby = "group",
    title = "PCA biplot",
    subtitle = paste0("result of ", title),
    showLoadings = F,
    hline = 0,
    vline = 0
  )
  
  pca_out = list(
    pca_result = pca_mat,
    plt_bi = plt_bi
  )
  return(pca_out)
}


