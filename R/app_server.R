#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT dataTableOutput
#' @import stringr
#' @import tidyverse
#' @import tidyr
#' @import shinyjqui
#' @import bslib
#' @import shinymanager
#' @import purrr
#' @import progressr
#' @import furrr
#' @import progress
#' @import shinyWidgets
#' @importFrom classyfireR get_classification
#' @noRd


app_server <- function(input, output, session) {
  bs_themer()
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  ## demo data
  output$Sample_meta = DT::renderDataTable(
    mda_demo_data$sample_meta,
    extensions = c('Buttons', 'Scroller'), 
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scrollX = T,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))
  
  output$Compound_meta = DT::renderDataTable(
    mda_demo_data$compound_meta,
    extensions = c('Buttons', 'Scroller'), 
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scrollX = T,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  
  output$exp_meta = DT::renderDataTable(
    mda_demo_data$exp_meta,
    extensions = c('Buttons', 'Scroller'), 
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scrollX = T,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  
  ## step1 get cid ===============
  com_input <- reactive({
    file1 <- input$compound_input
    if(is.null(file1)){return()}
    read.delim(file = file1$datapath,
               sep="\t",
               header = T,quote = NULL,
               stringsAsFactors = F)
  })

  cid_var <- reactiveValues(data=NULL)

  observeEvent(
    input$get_cid,
    {
      cid_var$file = com_input()
      cid_var$mod = as.character(input$cid_mod)
      if(ncol(cid_var$file) != 5) {
        cat("Please Check the input file!")
        return()
      } else {
        cid_var$data_info = data.frame(
          Compound_ID = cid_var$file[,1],
          name = cid_var$file[,2],
          mf = cid_var$file[,3],
          mw = cid_var$file[,4],
          RT = cid_var$file[,5]
        )
      
      cid_var$name = unique(cid_var$data_info$name)
      cid_var$compound = cid_var$data_info$Compound_ID
      withProgress(message = 'Fetching PubChem CID',value = 0,
                   {
                     n <- length(cid_var$name)
                     print(n)
                     for (i in 1:n) {
                       if (i == 1) {
                           dat <- shawn_get_cid_sh(name = cid_var$name[1],mod = cid_var$mod)
                       } else {
                           dat <- rbind(dat,shawn_get_cid_sh(name = cid_var$name[i],mod = cid_var$mod))
                       }
                       incProgress(1/n,detail = paste0(cid_var$compound[i]," in processing (",i,"/",n,")"))
                       Sys.sleep(0.1)
                     }
                   })
      cid_var$cid_tbl_out <- dat
      
      output$cid_message <- renderPrint({
        str(list(
          `Compound number` = length(cid_var$compound),
          `Compound name number` = length(cid_var$name),
          `Successful` = nrow(dat %>% filter(!is.na(cid)) %>% select(query) %>% unique()),
          `failed` = nrow(dat %>% filter(is.na(cid)) %>% select(query) %>% unique())
        ))
      })
      }
      }
  )
  output$cid_tbl = DT::renderDataTable(
    cid_var$cid_tbl_out,
    extensions = c('Buttons', 'Scroller'), 
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scrollX = T,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  

# step2 get detial-------------------------------------------------------------------
  detail_file_info = reactive({
    file1 <- input$name2cid_file
    if(is.null(file1)){return()}
    read.delim(file = file1$datapath,
               sep="\t",
               header = T,quote = NULL,
               stringsAsFactors = F)
  })

  detail_var = reactiveValues(data =NULL)
  
  observeEvent(
    input$get_detail,
    {
      detail_var$detail_break = as.character(input$cid_break)
      if(detail_var$detail_break == "no"){
        detail_var$name2cid = cid_var$cid_tbl_out
      } else {
        detail_var$name2cid = detail_file_info()
      }
      
      detail_var$cid = unique(detail_var$name2cid %>% filter(!is.na(cid)) %>% select(cid))
      detail_var$cid = detail_var$cid$cid
      
      withProgress(message = 'Fetching PubChem annotation',value = 0,
                   {
                     n <- length(detail_var$cid)
                     print(n)
                     for (i in 1:n) {
                       if (i == 1) {
                         dat <- mda_pubchem_crawler_sh(cid = detail_var$cid[1])
                       } else {
                         dat <- rbind(dat,mda_pubchem_crawler_sh(cid = detail_var$cid[i]))
                       }
                       incProgress(1/n,detail = paste0(detail_var$cid[i]," in processing (",i,"/",n,")"))
                       Sys.sleep(0.1)
                     }
                   })
      detail_var$detail_tbl_out <- dat
      
      output$detail_message <- renderPrint({
        str(list(
          `Compound name number` = length(unique(detail_var$name2cid$query)),
          `CID number` = length(unique(detail_var$cid)),
          `Successful` = nrow(dat %>% filter(!is.na(MolecularFormula)) %>% select(CID) %>% unique()),
          `failed` = nrow(dat %>% filter(is.na(MolecularFormula)) %>% select(CID) %>% unique())
        ))
      })
    }
  )
  
  output$detail_tbl = DT::renderDataTable(
    detail_var$detail_tbl_out,
    extensions = c('Buttons', 'Scroller'), 
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scrollX = T,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))

  # Step 3-------------------------------------------------------------------
  class_file_info = reactive({
    file1 <- input$cid2detail2
    if(is.null(file1)){return()}
    read.delim(file = file1$datapath,
               sep="\t",
               header = T,quote = NULL,
               stringsAsFactors = F)
  })
  
  class_var = reactiveValues(data =NULL)
  
  observeEvent(
    input$get_classification,
    {
      class_var$class_break = as.character(input$class_break)
      if(class_var$class_break == "no"){
        class_var$name2cid = detail_var$detail_tbl_out
      } else {
        class_var$name2cid = class_file_info()
      }
      
      class_var$inchikey = unique(class_var$name2cid %>% filter(!is.na(InChIKey)) %>% select(InChIKey))
      class_var$inchikey = class_var$inchikey$InChIKey
      class_var$tmp_list = list()
      
      withProgress(message = 'Classification ...',value = 0,
                   {
                     n <- length(class_var$inchikey)
                     for (i in 1:n) {
                       tryCatch({
                         class_var$tmp_list[[i]] = classyfireR::get_classification(inchi_key = class_var$inchikey[i])
                       },error = function(e) {})
                       incProgress(1/n,detail = paste0("Classification in processing (",i,"/",n,")"))
                     }
                   })
      
      class_var$result_tbl = purrr::map_df(class_var$tmp_list,classfireR2tbl)
      
      dat = bind_rows(class_var$result_tbl) %>% 
        drop_na() %>% unique() %>% 
        pivot_wider(names_from = Level,values_from = Classification) 
      
      class_var$class_tbl_out = dat
      
      output$class_message <- renderPrint({
        str(list(
          `CID number` = length(unique(class_var$name2cid$cid)),
          `InChIKey number` = length(unique(class_var$inchikey)),
          `Successful` = nrow(dat %>% filter(!is.na(superclass)) %>% select(InchIkeys) %>% unique()),
          `failed` = nrow(dat %>% filter(is.na(superclass)) %>% select(InchIkeys) %>% unique())
        ))
      })
    }
  )
  
  output$class_tbl = DT::renderDataTable(
    class_var$class_tbl_out,
    extensions = c('Buttons', 'Scroller'), 
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scrollX = T,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  

# Step 4 kegg -------------------------------------------------------------------


  kegg_file_info = reactive({
    file1 <- input$detail2class
    if(is.null(file1)){return()}
    read.delim(file = file1$datapath,
               sep="\t",
               header = T,quote = NULL,
               stringsAsFactors = F)
  })

  kegg_var = reactiveValues(data =NULL)

  observeEvent(
    input$get_KEGG_cid,
    {
      kegg_var$kegg_break = as.character(input$kegg_break)
      if(kegg_var$kegg_break == "no"){
        kegg_var$in2class = class_var$class_tbl_out
      } else {
        kegg_var$in2class = kegg_file_info()
      }

      kegg_var$inchikey = unique(kegg_var$in2class %>% filter(!is.na(InchIkeys)) %>% select(InchIkeys))
      kegg_var$inchikey = kegg_var$inchikey$InchIkeys
      kegg_var$tmp_list = list()
      CTS_url = "https://cts.fiehnlab.ucdavis.edu/rest/convert/InChIKey/KEGG/"
      withProgress(message = 'Classification ...',value = 0,
                   {
                     n <- length(kegg_var$inchikey)
                     for (i in 1:n) {
                       tryCatch({
                         url = paste0(CTS_url,kegg_var$inchikey[i])
                         kegg_var$tmp_list[[i]] = tryCatch(InChikey2kegg(url = url))
                       },error = function(e) {})
                       incProgress(1/n,detail = paste0("KEGG annotation in processing (",i,"/",n,")"))
                       Sys.sleep(0.1)
                     }
                   })
      dat = bind_rows(kegg_var$tmp_list)
      kegg_var$kegg_tbl_out = dat
      output$kegg_message <- renderPrint({
        str(list(
          `InChIKey number` = length(unique(kegg_var$inchikey)),
          `KEGG CID number` = length(unique(dat$KEGG)),
          `Successful` = nrow(dat %>% filter(!is.na(KEGG)) %>% select(InChIkey) %>% unique()),
          `failed` = nrow(dat %>% filter(is.na(KEGG)) %>% select(InChIkey) %>% unique())
        ))
      })
    }
  )

  output$KEGG_tbl = DT::renderDataTable(
    kegg_var$kegg_tbl_out,
    extensions = c('Buttons', 'Scroller'),
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scrollX = T,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  
  # Step 5 Merge table -------------------------------------------------------------------
  
  
  merge_file_info = reactive({
    file1 <- input$detail2class
    if(is.null(file1)){return()}
    read.delim(file = file1$datapath,
               sep="\t",
               header = T,quote = NULL,
               stringsAsFactors = F)
  })
  
  merge_var = reactiveValues(data =NULL)
  
  observeEvent(
    input$merge_table,
    {
      merge_var$merge_break = as.character(input$merge_break)
      if(merge_var$merge_break == "no"){
        merge_var$step1 = cid_var$file ## raw data 'orz_data'
        merge_var$step2 = cid_var$cid_tbl_out ## name to cid  'name2cid'
        merge_var$step3 = detail_var$detail_tbl_out ## cid to inchikey 'pubchem_detail'
        merge_var$step4 = class_var$class_tbl_out  ## inchikey to classyfire 'pubchem_class'
        merge_var$step5 = kegg_var$kegg_tbl_out ## inchikey to kegg
      } else {
        merge_var$step1 = cid_var$file ## raw data
        merge_var$step2 = detail_file_info() ## name to cid 
        merge_var$step3 = class_file_info() ## cid to inchikey
        merge_var$step4 = kegg_file_info()  ## inchikey to classyfire
        merge_var$step5 = merge_file_info() ## inchikey to kegg
      }
      
      merge_var$result_out = mda_merge_info(
        orz_data = merge_var$step1,
        name2cid = merge_var$step2,
        pubchem_detail = merge_var$step3,
        pubchem_class = merge_var$step4,
        CTS_kegg = merge_var$step5
      )
      
      output$merge_message <- renderPrint({
        str(list(
          `Compound number` = length(unique(merge_var$result_out$compound_id)),
          `Match with PubChem` = nrow(merge_var$result_out %>% filter(!is.na(cid)) %>% select(compound_id) %>% unique()),
          `Can be classfied` = nrow(merge_var$result_out %>% filter(!is.na(superclass)) %>% select(compound_id) %>% unique()),
          `High confidence` = nrow(merge_var$result_out %>% filter(isTRUE(High_identical)) %>% select(compound_id) %>% unique())
        ))
      })
    }
  )
  
  output$final_tbl = DT::renderDataTable(
    merge_var$result_out,
    extensions = c('Buttons', 'Scroller'),
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scrollX = T,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  
  
  
}

