library(bslib)
library(shiny)
library(rlang)
library(curl)
library(shinyjqui)
library(shinyjs)
library(bs4Dash)
library(shinymanager)

theme <- bs_theme(
  bg = "#0b3d91", fg = "white", primary = "#FCC780",
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
  user = c("wangxiao", "zhaobin", "zhangyin", "other"),
  password = c("wx1990125", "henuzhanglab01", "henuzhanglab02", "henuzhanglab03"),
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)

# ui ----------------------------------------------------------------------



ui <- secure_app(
  head_auth = tags$script(inactivity),theme = theme_code,
  navbarPage(
    theme = theme,
    title = div(
      img(
        src = "MDAtoolskits2.svg",
        height = 40,
        weight = 40,
        style = "margin:10px 10px"
      ),
      "Enjoy!"
    ),
    collapsible = T,
    id = "navbar",
    ## home page panel
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
                src = "MDAtoolkits.svg",
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
          icon = icon("leaf")
          
        ),
        tab(
          title = "Compound demo",
          icon = icon("atom")
          
        ),
        tab(
          title = "Normalized peak area demo",
          icon = icon("braille")
          
        )
      )
    )
  )
)
  


# server ------------------------------------------------------------------


server <- function(input, output, session) {
  bs_themer()
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
 
}

shinyApp(ui,server)
