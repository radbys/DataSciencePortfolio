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
  Sys.sleep(1)
  lst <- lst_id()

  # Scroll to top
  scroll(0)

  return(lst)
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

get_details <- function() {
  iters <- 1:nrow(dt)

  lapply(iters[1:30], function(i){

    print(sprintf("Scraping offer number: %s", i))

    ff$navigate(dt$link[i])
    Sys.sleep(abs(rnorm(1, 2, 1.5)))

    source <- ff$getPageSource()[[1]] %>%
      read_html()

    topbar <- source %>%
      html_elements(xpath = "//div[@class = 'css-1kgdb8a']/div/div/text()") %>%
      html_text()

    techs <- source %>%
      html_elements(xpath = "//div[@class = 'css-1ikoimk']/div/div/@title") %>%
      html_text()

    text <- source %>%
      html_elements(xpath = "//div[@class = 'css-alatv1']/span") %>%
      html_elements(xpath = ".//div/text() |
                           .//strong/text() |
                           .//li/text()") %>%
      html_text()



    data.frame(
      size = as.numeric(gsub("[+><, ]|.*-", "", topbar[2], perl = TRUE)),
      exp_lvl = topbar[4],
      exp_yrs = text[grepl("(year.*experience|experience*year|lat.*doś|doś.*lat|let.*doś|doś.*let)",
                           text,
                           ignore.case = TRUE,
                           perl = TRUE)] %>%
        paste(collapse = ". "),
      r_men = text[grepl("(?<![^.,/;: ])R$|(?<![^.,/;: ])R(?![^.,/;: ])", text, perl = TRUE)] %>%
        paste(collapse = " "),
      add_tech = paste(techs[-c(1:3)], collapse = ", ")
    )
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame()
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
det <- get_details()


offs <- cbind(
  dt,
  det
)






