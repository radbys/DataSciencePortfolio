################################################################################
# Title:  Get the job offers - Web scraping project
# Date:   2021-08-10
# Technologies:
#     * R
#     * HTML
#     * Java Script (couple of lines of code)
# Libraries:
#   * tidyverse
#   * RSelenium
#   * rvest
# Tasks:
#   * Data preparation
# Preface:
#   The incentive to write this code is the inability to filter job offers for
#   the R language on one of the most popular websites with job offers in Poland.
#
#   This might be transformed later into a package, of course, it would require
#   speeding up the code first, and some general upgrades but most importantly,
#   permission from the owners of the portal from which it extracts the data.
#
#   There are several places where I broke the `R Inferno` by growing a vector.
#   I did try another approach with including for loop and apply but it wasn't
#   just reliable enough.
################################################################################

# Libraries
library(RSelenium)
library(rvest)

# User made function

  # Java script element scroll
  scroll <- function(x) {
    RSelenium::ff$executeScript(
      sprintf(
        "var es = document.getElementsByClassName('css-ic7v2w');
        var e = es[0];
        e.scrollTo(0, %s);",
        x
      )
    )
  }

  # Html of visible offers
  offers <- function() {
    RSelenium::ff$getPageSource()[[1]] %>%
      rvest::read_html() %>%
      rvest::html_elements(xpath = "//div[@class = 'css-ic7v2w']/div/div[not(@class)]")
  }

  # ID of last visible offer
  lst_id <- function() {
    offers()[offers() %>% length()] %>%
      rvest::html_elements(xpath = "@style") %>%
      rvest::html_text() %>%
      gsub(x = ., ".*top: |px; heigh.*", "") %>%
      as.numeric()
  }

  # Last offer id
  scr_max <- function() {
    # Find borders
    bd <- RSelenium::ff$getPageSource()[[1]] %>%
      rvest::read_html() %>%
      rvest::html_elements(xpath = "//div[@class = 'css-ic7v2w']/div/@style") %>%
      rvest::html_text() %>%
      gsub(x = ., ".*heigh.: |px; width.*", "") %>%
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

  # Rvest functions wrapper for cleaner code
  get_text <- function(x, txt = offers()) {
    txt %>%
      rvest::html_element(xpath = x) %>%
      rvest::html_text()
  }

  # Crating df with offers
  get_offers <- function() {
    lst_off <- scr_max()
    Sys.sleep(2)
    loc_max <- 0

    # Starting df
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

    # Growing a df (yikes!)
    while (loc_max != lst_off) {

      # Salary
      h <- get_text("a/div/div[2]/div[1]/div[2]/*")
      h <- gsub(x = h, " ", "")
      f <- suppressWarnings(as.numeric(gsub(x = h, "-.*", "")))
      t <- suppressWarnings(as.numeric(gsub(x = h, ".*-|PLN.*", "")))

      # Creating df
      tmp <- data.frame(
        id = offers() %>%
          rvest::html_elements(xpath = "@style") %>%
          rvest::html_text() %>%
          gsub(x = ., ".*top: |px; heigh.*", "") %>%
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

      # Binding with current df
      df <- rbind(
        df, tmp
      )

      # Figuring out should the next iterations start
      loc_max <- lst_id()
      int <- loc_max + (89 * 3)
      scroll(int)
      Sys.sleep(0.5)
    }

    # Excluding possible duplicates
    df <- df %>%
      dplyr::filter(!duplicated(id))
  }

  # Crating df with details from inside the offers
  get_details <- function() {
    iters <- 1:nrow(dt)

    lapply(iters, function(i) {
      print(sprintf("Scraping offer number: %s", i))

      RSelenium::ff$navigate(dt$link[i])
      Sys.sleep(abs(rnorm(1, 2, 1.5)))

      source <- RSelenium::ff$getPageSource()[[1]] %>%
        rvest::read_html()

      topbar <- source %>%
        rvest::html_elements(xpath = "//div[@class = 'css-1kgdb8a']/div/div/text()") %>%
        rvest::html_text()

      techs <- source %>%
        rvest::html_elements(xpath = "//div[@class = 'css-1ikoimk']/div/div/@title") %>%
        rvest::html_text()

      text <- source %>%
        rvest::html_elements(xpath = "//div[@class = 'css-alatv1']/span") %>%
        rvest::html_elements(xpath = ".//div/text() |
                             .//strong/text() |
                             .//li/text()") %>%
        rvest::html_text()

      data.frame(
        size = as.numeric(gsub("[+><, ]|.*-", "", topbar[2], perl = TRUE)),
        exp_lvl = topbar[4],
        exp_yrs = text[grepl("(year.*experience|experience*year|lat.*doś|doś.*lat|let.*doś|doś.*let)",
          text,
          ignore.case = TRUE,
          perl = TRUE
        )] %>%
          paste(collapse = ". "),
        r_men = text[grepl("(?<![^.,/;: ])R$|(?<![^.,/;: ])R(?![^.,/;: ])", text, perl = TRUE)] %>%
          paste(collapse = " "),
        add_tech = paste(techs[-c(1:3)], collapse = ", ")
      )
    }) %>%
      do.call(rbind, .) %>%
      as.data.frame()
  }

# Opening up the webDriver
rd <- RSelenium::rsDriver(
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
ff <- RSelenium::rd$client


# Navigating to job offers website
RSelenium::ff$maxWindowSize()
RSelenium::ff$navigate("https://justjoin.it/all/data")

# Scraping
dt <- get_offers()
det <- get_details()

# Joining data
offs <- cbind(
  dt,
  det
)

# Filtering what's interesting
offs %>%
  dplyr::filter(
    (tech1 == "R" |
    tech2  == "R" |
    tech3  == "R" |
    r_men  != "" )&
    (
    city %in% c("Gdynia", "Sopot", "Gdańsk") |
    remote == TRUE
    )
  )
