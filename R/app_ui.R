#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom DT dataTableOutput
#' @import stringr
#' @import tidyverse
#' @import shinyjqui
#' @import bslib
#' @import shinymanager
#' @import purrr
#' @import progressr
#' @import furrr
#' @import progress
#' @import dplyr
#' @import shinyWidgets
#' @noRd

theme <- bs_theme(
  bg = "#FCFCFC", fg = "black", primary = "#FCC780",
  #base_font = font_google("Quicksand"),
  base_font = font_google("Kalam"),
  code_font = font_google("Space Mono")
)

theme_code = bs_theme(
  bg = "white", fg = "black", primary = "#FCC780",
  #base_font = font_google("Quicksand"),
  base_font = font_google("Kalam"),
  code_font = font_google("Space Mono")
)

# enlarged auto fonts
if (is_installed("thematic")) {
  thematic::thematic_shiny(
    font = thematic::font_spec("auto", scale = 2, update = TRUE)
  )
}

# Source in ggplot2 examples

#theme <- bs_global_get()
if ("3" %in% theme_version(theme)) {
  warning("This example app requires Bootstrap 4 or higher", call. = FALSE)
}

rounded <- isTRUE(as.logical(bs_get_variables(theme %||% bslib::bs_theme(), "enable-rounded")))
pill <- function(...) {
  shiny::tabPanel(..., class = "p-3 border", class = if (rounded) "rounded")
}
tab <- function(...) {
  shiny::tabPanel(..., class = "p-3 border border-top-0", class = if (rounded) "rounded-bottom")
}
gradient <- function(theme_color = "primary") {
  bg_color <- paste0("bg-", theme_color)
  bgg_color <- paste0("bg-gradient-", theme_color)
  bg_div <- function(color_class, ...) {
    div(
      class = "p-3", class = color_class,
      paste0(".", color_class), ...
    )
  }
  fluidRow(
    column(6, bg_div(bg_color)),
    column(6, bg_div(bgg_color))
  )
}

## golem_add_external_resources()


# password ----------------------------------------------------------------


library(shinymanager)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials <- data.frame(
  user = paste0("user",c(1:100)),
  password = paste0("henu",c(85,66,95,63,51,98,51,67,61,95,39,88,47,51,48,63,62,79,57,78,42,53,54,99,45,100,79,53,52,91,29,20,42,55,27,39,59,23,81,65,74,46,59,28,48,19,63,96,49,76, 5,20,65,19,37,21,90,26,99, 5,13,27,73,6,56,69,32,41,74,96,77,59,48,38,40,23,16,41,29,71,43,29,42,22,84,54,41,59,97,66,35,35,94,40,58,53,16,26,73,32))
  
)


# ui ----------------------------------------------------------------------

app_ui <- secure_app(
  head_auth = tags$script(inactivity),theme = theme_code,
    navbarPage(
    theme = shiny::bootstrapLib(theme),
    title = div(
      tags$img(
        src = "https://shawnmagic-1257599720.cos.ap-chengdu.myqcloud.com/MDAtoolskits2.svg",
        height = 40,
        weight = 40,
        style = "margin:10px 10px"
      ),
      "Enjoy!"
    ),
    collapsible = T,
    id = "navbar",
    ## home page panel ====
    tabPanel(
      useShinyjs(),
      title = "Home page",
      icon = icon("home"),type = "pill",
      tabsetPanel(
        pill(
          title = "Introduction",
          icon = icon("microscope"),
          fluidRow(
            column(
              6, class = "p-3 border", class =  "rounded",
              HTML(
                "
              
              <div id = 'header' style='text-align:center;'>
              <h1>What can I do ?</h1>
              </div>
              
              <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
              
              <div id = 'content' style='float:left'>
              
              This app is developed for downstream data analysis and visualization of <font style='bold' color = 'green'>Untargeted metabolomics or widely targeted metabolomics</font> data based on LC-MS (GC-MS).
              
              <h3> Candidate compounds evaluation </h3>
              <ol>
              <li> Candidate compounds with high confidence will be screened out by comparison with the Pubchem database. Based on chemical name, formula and molecular weight.<br></li>
              <li> The classification of candidate compounds is done by the ClassFireR package.<br></li>
              <li> The KEGG pathway annotation of each compound was^ from KEGG database and metaboanalyst.<br>
              </ol>
              
              <h3> Accumulation profile </h3>
              <ol>
              <li> Overall accumulation profile will presented by heatmap.<br></li>
              <li> Correspounding analysis of compounds and samples.<br></li>
              <li> PCA.<br>
              <li> Different accumulated metabolite.<br></li>
              <ul>
                t-test p-value<br></li>
                q-value (BH method)<br></li>
                PLS-DA or OPLS-DA (VIP)<br></li>
                Fold change.<br></li>
              </ul>
              <li> Key compounds information.<br></li>
              <li> todo .... </li>
              </ol>
              </div>
              
              "
              )
            ),
            column(
              6, class = "p-3 border", class =  "rounded",
              HTML(
                "
              <div id = 'header' style='text-align:center;'>
              <h1>Brief structure </h1>
              </div>
              
              <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
              <br>
              <br>
              <br>
                "
              ),
              img(
                src = "https://shawnmagic-1257599720.cos.ap-chengdu.myqcloud.com/MDAtoolkits.svg",
                height = 300,
                weight = 800
              )
            )
          ),
          fluidRow(
            column(
              6, class = "p-3 border", class =  "rounded",
              HTML(
                "
              <div id = 'header' style='text-align:center;'>
              <h1>Matters need attention </h1>
              </div>
              
              <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
              <div id = 'content' style='float:left'>
              
              <h3> input file format </h3>
              <ol>
              <li> Only accept tab delimited files. <font color = 'red'> DO NOT use comma delimited file such as .csv file. </font> 
              <li> <font color = 'red'> DO NOT use pure numbers as customized compound ID. </font>
              <li> <font color = 'red'>DO NOT use 'space', '-' and other special symbols in all headers (column names), it’s better to use '_' instead. </font>
              <li> This app <font color = 'red'> DOSE NOT </font> provide MS1, MS2 database search service, the input file should be the result after the database searching by commercial software or other open source software (eg. MetID,MetDNA).
              <li> The software also <font color = 'red'> DOSE NOT </font> support peak area cleaning, imputation and normalization. The raw peaks data should be cleanning by commercial software or other open source software (eg.Metflow)
              </ol>
              
              <h3> other precaution </h3>
              <ol> 
              <li> Generally, the number of compounds upload each time <font color = 'red'> SHOULD NOT </font> be greater than 500.
              <li> If you wait for more than 40 minutes, there may be an error in the program, please refresh the page and try again.
              <li> Bug report: shawndrive2019@126.com
              </ol>
              </div>
              
              "
              )
            ),
            column(
              6, class = "p-3 border", class =  "rounded",
              HTML(
                "
              <div id = 'header' style='text-align:center;'>
              <h1> Acknowledgement </h1>
              </div>
              
              <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
              <br>
              <br>
              <br>
              
              
              "
              )
            )
          )
        ),
        tab(
          title = "Sample demo",
          icon = icon("leaf"),
          fluidPage(
            DT::dataTableOutput("Sample_meta")
          )
          
        ),
        tab(
          title = "Compound demo",
          icon = icon("atom"),
          fluidPage(
            DT::dataTableOutput("Compound_meta")
          )
          
        ),
        tab(
          title = "Normalized peak area demo",
          icon = icon("braille"),
          fluidPage(
            DT::dataTableOutput("exp_meta")
          )
          
        )
      )
    ),
    ### Identification ======
    tabPanel(
      useShinyjs(),
      title = "Identification",
      icon = icon("home"),type = "pill",
      tabsetPanel(
        pill(
          title = "Compound name to PubChem CID",
          icon = icon("microscope"),
          fluidRow(
            column( width = 4, class = "p-3 border", class =  "rounded",
                    HTML(
                      "
                      <div id = 'header' style='text-align:center;'>
                      <h2> options </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      "
                    ),
              fileInput(
                inputId = "compound_input",
                label = "Upload compound metadata",
                accept = c('.tsv','.xls','.txt')
              ),
              selectInput(
                inputId = "cid_mod",
                label = "Match method",
                choices = c("all","first"),
                selected = "all"
              ),
              br(),
              actionButton("get_cid",label = "Start Step1",icon = icon("play"))
            ),
            column(4,class = "p-3 border", class =  "rounded",
                   HTML(
                     "
                      <div id = 'header' style='text-align:center;'>
                      <h2> Summary </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      "
                   ),
                   verbatimTextOutput("cid_message")
                   ),
            column(4,class = "p-3 border", class =  "rounded",
                   HTML(
                     "
                      <div id = 'header' style='text-align:center;'>
                      <h2> Reference link </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      <div id = 'content' style='float:left'>
                      <h3> websites </h3>
                      <ol>
                      <li> <font color = grey> <b> right click and open in new tab</b></font> 
                      <li> Github repo for webchem package: <a href='https://github.com/ropensci/webchem'>Visit webchem</a>
                      <li> PubChem <a href='https://pubchem.ncbi.nlm.nih.gov'> Visit PubChem</a>
                      <li> PubChem API <a href='https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest'>How to use PUG REST</a>
                      </ol>
                      <h3> citation </h3>
                      <ol>
                      <li> Szöcs E, Stirling T, Scott ER, et al (2020) webchem: An R Package to Retrieve Chemical Information from the Web. J Stat Soft 93:.<a href='https://doi.org/10.18637/jss.v093.i13'> https://doi.org/10.18637/jss.v093.i13</a>
                      </div>
                      "
                   ),
            ),
            br(),
            column(
              width = 12,class = "p-3 border", class =  "rounded",
              HTML(
                "
              <div id = 'header' style='text-align:center;'>
              <h1> Compound name to PubChem CID </h1>
              </div>
              <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
              <br>

              "
              ),
              DT::dataTableOutput("cid_tbl")
            )
          )
          ),
        ## cid 2 detail 
        pill(
          title = "PubChem CID to details",
          icon = icon("microscope"),
          fluidRow(
            column( width = 4, class = "p-3 border", class =  "rounded",
                    HTML(
                      "
                      <div id = 'header' style='text-align:center;'>
                      <h2> options </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      "
                    ),
                    fileInput(
                      inputId = "name2cid_file",
                      label = "Upload compound name to cid file.(Only for discontinuous task)",
                      accept = c('.tsv','.xls','.txt')
                    ),
                    radioButtons(inputId = "cid_break",label = "task from reupload file?",choices = c("yes","no"),selected = "no"),
                    br(),
                    actionButton("get_detail",label = "Start Step2",icon = icon("play"))
            ),
            column(4,class = "p-3 border", class =  "rounded",
                   HTML(
                     "
                      <div id = 'header' style='text-align:center;'>
                      <h2> Summary </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      "
                   ),
                   verbatimTextOutput("detail_message")
            ),
            column(4,class = "p-3 border", class =  "rounded",
                   HTML(
                     "
                      <div id = 'header' style='text-align:center;'>
                      <h2> Reference link </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      <div id = 'content' style='float:left'>
                      <h3> websites </h3>
                      <ol>
                      <li> <font color = grey> <b> right click and open in new tab</b></font> 
                      <li> Github repo for webchem package: <a href='https://github.com/ropensci/webchem'>Visit webchem</a>
                      <li> PubChem <a href='https://pubchem.ncbi.nlm.nih.gov'> Visit PubChem</a>
                      <li> PubChem API <a href='https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest'>How to use PUG REST</a>
                      </ol>
                      <h3> citation </h3>
                      <ol>
                      <li> Szöcs E, Stirling T, Scott ER, et al (2020) webchem: An R Package to Retrieve Chemical Information from the Web. J Stat Soft 93:.<a href='https://doi.org/10.18637/jss.v093.i13'> https://doi.org/10.18637/jss.v093.i13</a>
                      </ol> 
                      </div>
                      "
                   ),
            ),
            br(),
            column(
              width = 12,class = "p-3 border", class =  "rounded",
              HTML(
                "
              <div id = 'header' style='text-align:center;'>
              <h1> PubChem CID to compound annotation </h1>
              </div>
              <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
              <br>

              "
              ),
              DT::dataTableOutput("detail_tbl")
            )
          )
        ),
        ## classyfire 
        pill(
          title = "Compound classify",
          icon = icon("microscope"),
          fluidRow(
            column( width = 4, class = "p-3 border", class =  "rounded",
                    HTML(
                      "
                      <div id = 'header' style='text-align:center;'>
                      <h2> options </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      "
                    ),
                    fileInput(
                      inputId = "cid2detail2",
                      label = "Upload compound cid to detail file. (Only for discontinuous task)",
                      accept = c('.tsv','.xls','.txt')
                    ),
                    radioButtons(inputId = "class_break",label = "task from reupload file?",choices = c("yes","no"),selected = "no"),
                    br(),
                    actionButton("get_classification",label = "Start Step3",icon = icon("play"))
            ),
            column(4,class = "p-3 border", class =  "rounded",
                   HTML(
                     "
                      <div id = 'header' style='text-align:center;'>
                      <h2> Summary </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      "
                   ),
                   verbatimTextOutput("class_message")
            ),
            column(4,class = "p-3 border", class =  "rounded",
                   HTML(
                     "
                      <div id = 'header' style='text-align:center;'>
                      <h2> Reference link </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      <div id = 'content' style='float:left'>
                      <h3> websites </h3>
                      <ol>
                      <li> <font color = grey> <b> right click and open in new tab</b></font> 
                      <li> Github repo for classyfireR package: <a 'https://github.com/aberHRML/classyfireR'>Visit classyfireR</a>
                      <li> ClassyFire API <a href='https://bitbucket.org/wishartlab/classyfire_api/src/master/'>How to use classyFire</a>
                      </ol>
                      <h3> citation </h3>
                      <ol>
                      <li> Djoumbou Feunang Y, Eisner R, Knox C, Chepelev L, Hastings J, Owen G, Fahy E, Steinbeck C, Subramanian S, Bolton E, Greiner R, and Wishart DS. ClassyFire: Automated Chemical Classification With A Comprehensive, Computable Taxonomy. Journal of Cheminformatics, 2016, 8:61.<a href='https://jcheminf.springeropen.com/articles/10.1186/s13321-016-0174-y'> DOI: 10.1186/s13321-016-0174-y
                      </a>
                      </ol> 
                      </div>
                      "
                   ),
            ),
            br(),
            column(
              width = 12,class = "p-3 border", class =  "rounded",
              HTML(
                "
              <div id = 'header' style='text-align:center;'>
              <h1> Compound classification via ClassyFire </h1>
              </div>
              <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
              <br>
              "
              ),
              DT::dataTableOutput("class_tbl")
            )
          )
        ),
        # kegg
        pill(
          title = "KEGG annotation",
          icon = icon("microscope"),
          fluidRow(
            column( width = 4, class = "p-3 border", class =  "rounded",
                    HTML(
                      "
                      <div id = 'header' style='text-align:center;'>
                      <h2> options </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      "
                    ),
                    fileInput(
                      inputId = "detail2class",
                      label = "Upload classyfication file. (Only for discontinuous task)",
                      accept = c('.tsv','.xls','.txt')
                    ),
                    radioButtons(inputId = "kegg_break",label = "task from reupload file?",choices = c("yes","no"),selected = "no"),
                    br(),
                    actionButton("get_KEGG_cid",label = "Start Step4",icon = icon("play"))
            ),
            column(4,class = "p-3 border", class =  "rounded",
                   HTML(
                     "
                      <div id = 'header' style='text-align:center;'>
                      <h2> Summary </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      "
                   ),
                   verbatimTextOutput("kegg_message")
            ),
            column(4,class = "p-3 border", class =  "rounded",
                   HTML(
                     "
                      <div id = 'header' style='text-align:center;'>
                      <h2> Reference link </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      <div id = 'content' style='float:left'>
                      <h3> websites </h3>
                      <ol>
                      <li> <font color = grey> <b> right click and open in new tab</b></font>
                      <li> CTS - The Chemical Translation Service (Batch convert): <a 'http://cts.fiehnlab.ucdavis.edu/batch'>Visit CTS-batch</a>
                      <li> CTS API <a href='http://cts.fiehnlab.ucdavis.edu/services'>CTS REST Services</a>
                      </ol>
                      <h3> citation </h3>
                      <ol>
                      <li> Gert Wohlgemuth, Pradeep Kumar Haldiya, Egon Willighagen, Tobias Kind, Oliver Fiehn, The Chemical Translation Service—a web-based tool to improve standardization of metabolomic reports, Bioinformatics, Volume 26, Issue 20, 15 October 2010, Pages 2647–2648<a href=', https://doi.org/10.1093/bioinformatics/btq476'>  https://doi.org/10.1093/bioinformatics/btq476
                      </a>
                      </ol>
                      </div>
                      "
                   ),
            ),
            br(),
            column(
              width = 12,class = "p-3 border", class =  "rounded",
              HTML(
                "
              <div id = 'header' style='text-align:center;'>
              <h1> Compound KEGG annotation </h1>
              </div>
              <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
              <br>
              "
              ),
              DT::dataTableOutput("KEGG_tbl")
            )
          )
        ),
        ## table merge
        pill(
          title = "Merge result",
          icon = icon("microscope"),
          fluidRow(
            column( width = 4, class = "p-3 border", class =  "rounded",
                    HTML(
                      "
                      <div id = 'header' style='text-align:center;'>
                      <h2> options </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      "
                    ),
                    fileInput(
                      inputId = "detail2class",
                      label = "Upload KEGG annotation file. (Only for discontinuous task)",
                      accept = c('.tsv','.xls','.txt')
                    ),
                    radioButtons(inputId = "merge_break",label = "task from reupload file?",choices = c("yes","no"),selected = "no"),
                    br(),
                    actionButton("merge_table",label = "Start Step5",icon = icon("play"))
            ),
            column(4,class = "p-3 border", class =  "rounded",
                   HTML(
                     "
                      <div id = 'header' style='text-align:center;'>
                      <h2> Summary </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      "
                   ),
                   verbatimTextOutput("merge_message")
            ),
            column(4,class = "p-3 border", class =  "rounded",
                   HTML(
                     "
                      <div id = 'header' style='text-align:center;'>
                      <h2> Notice </h2>
                      </div>
                      <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
                      <br>
                      <div id = 'content' style='float:left'>
                      <h3> For continuous tasks</h3>
                      <ol>
                      <li> Make sure that each of the first 4 steps has output results! 
                      <li> <font color = red> DO NOT </font> upload any file and keep [task from reupload file] button as [no]
                      <li> Click [Start step5]
                      </ol>
                      <h3> For uncontinuous tasks </h3>
                      <ol>
                      <li> Upload the result files in the corresponding tabs in the order of step1-5, do not click any start button.
                      <li> Make sure all result files were uploaded, select yes in [task from reupload file].
                      <li> The result file <font color = red> MUST BE downloaded from this shiny app </font> 
                      <li> It is recommended to download the .csv file, then delete the useless information and save it as a tab-delimited .txt file.
                      </ol>
                      </div>
                      "
                   ),
            ),
            br(),
            column(
              width = 12,class = "p-3 border", class =  "rounded",
              HTML(
                "
              <div id = 'header' style='text-align:center;'>
              <h1> Compound KEGG annotation </h1>
              </div>
              <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
              <br>
              "
              ),
              DT::dataTableOutput("final_tbl")
            )
          )
        )
      )
    )
  
))



