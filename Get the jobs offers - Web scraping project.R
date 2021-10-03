# Wczytanie bibliotek -----------------------------------------------------
library(RSelenium)
library(wdman)
library(rvest)

# User made functions -----------------------------------------------------
scroll <- function(x){
  ff$executeScript(
    sprintf(
      "var es = document.getElementsByClassName('css-ic7v2w');
      var e = es[0];
      e.scrollTo(0, %s);",
      x
    )
  )
}

offers <- function(){
  ff$getPageSource()[[1]] %>%
    read_html() %>%
    html_elements(xpath = "//div[@class = 'css-ic7v2w']/div/div[not(@class)]")
}

lst_id <- function(){
  offers()[offers() %>% length()] %>%
    html_elements(xpath = "@style") %>%
    html_text() %>%
    gsub(x = ., '.*top: |px; heigh.*' ,"") %>%
    as.numeric()
}

scr_max <- function(){
  # Find borders
  bd <- ff$getPageSource()[[1]] %>%
    read_html() %>%
    html_elements(xpath = "//div[@class = 'css-ic7v2w']/div/@style") %>%
    html_text() %>%
    gsub(x = ., '.*heigh.: |px; width.*' ,"") %>%
    as.numeric()

  # Scroll to bottom
  scroll(bd)

  # Get last offer position
  lst <- offers()[offers() %>% length()] %>%
    html_elements(xpath = "@style") %>%
    html_text() %>%
    gsub(x = ., '.*top: |px; heigh.*' ,"") %>%
    as.numeric()

  # Scroll to top
  scroll(0)

  lst <- lst_id()
}

get_text <- function(x, txt = offers()){
  txt %>%
    html_element(xpath = x) %>%
    html_text()
}

get_offers <- function(){

  lst_off <- scr_max()
  Sys.sleep(2)
  loc_max <- 0

  df <- data.frame(
    id = NULL,
    title = NULL,
    company = NULL,
    city = NULL,
    remote = NULL,
    salary_from = NULL,
    salary_to = NULL,
    tech1 = NULL,
    tech2 = NULL,
    tech3 = NULL,
    link = NULL
  )

  while(loc_max != lst_off){

    # Df
    h <- get_text('a/div/div[2]/div[1]/div[2]/*')
    h <- gsub(x = h, " ", "")
    f <- suppressWarnings(as.numeric(gsub(x = h, '-.*' ,"")))
    t <- suppressWarnings(as.numeric(gsub(x = h, '.*-|PLN.*' ,"")))

    tmp <- data.frame(
      id = offers() %>%
        html_elements(xpath = "@style") %>%
        html_text() %>%
        gsub(x = ., '.*top: |px; heigh.*' ,"") %>%
        as.numeric(),
      title = get_text("a/div/div[2]/div[1]/div[1]/div"),
      company = get_text("a/div/div[2]/div[2]/div[2]/div[1]/text()"),
      city = get_text("a/div/div[2]/div[2]/div[2]/div[2]/span[1]/text()"),
      remote = ifelse(
        is.na(get_text("a/div/div[2]/div[2]/div[2]/div[2]/span[3]/text()")),
        FALSE,
        TRUE
      ),
      salary_from = f,
      salary_to = t,
      tech1 = get_text("a/div/div[2]/div[2]/div[3]/span[1]/text()"),
      tech2 = get_text("a/div/div[2]/div[2]/div[3]/span[2]/text()"),
      tech3 = get_text("a/div/div[2]/div[2]/div[3]/span[3]/text()"),
      link = sprintf("https://justjoin.it%s", get_text("a/@href"))
    )


    df <- rbind(
      df, tmp
    )

    loc_max <- lst_id()
    int <- loc_max + (89 * 3)
    scroll(int)
    Sys.sleep(0.5)

  }

  df <- df %>%
    dplyr::filter(!duplicated(id))

}

# Wczytanie strony --------------------------------------------------------
rd <- rsDriver(
  browser = "firefox",
  check = FALSE,
  verbose = FALSE,
  port = 4567L,
  extraCapabilities = list(
    "moz:firefoxOptions" = list(
      args = list(
        "--disable-web-security",
        "--user-data-dir",
        "--allow-running-insecure-content"
      )
    )
  )
)

ff <- rd$client


# Przejdz na JJ
ff$maxWindowSize()
ff$navigate("https://justjoin.it/all/data")

# Sciagnij oferty
dt <- get_offers()






