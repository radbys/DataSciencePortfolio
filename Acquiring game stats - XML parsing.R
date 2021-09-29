################################################################################
# Title:  Acquiring game stats - XML parsing
# Date:   2021-30-09
# Technologies:
#     * R
#     * XML
# Libraries:
#   * tidyverse
#   * xml2
# Tasks:
#   * Data preparation
################################################################################

# Libraries
library(tidyverse)
library(xml2)

# User made function
get_data <- function(x, exclude_duplicates = TRUE){

  rtn <- lapply(x, function(x){
    code <- xml2::xml_find_all(x, "code/text()") %>%
      xml2::xml_text()

    start <-xml2:: xml_find_all(x, "start/text()") %>%
      xml2::xml_text() %>%
      as.integer()

    end <- xml2::xml_find_all(x, "end/text()") %>%
      xml2::xml_text() %>%
      as.integer()

    labels <- xml2::xml_find_all(x, "label/text/text()") %>%
      xml2::xml_text() %>%
      unique() %>%
      paste(collapse = ", ")

    if(nchar(labels) == 0) labels <- NA

    data.frame(code, start, end, labels)

  }) %>%
    do.call(rbind, .) %>%
    as.data.frame()

  if(exclude_duplicates){
    rtn <- rtn %>%
      dplyr::group_by(start) %>%
      dplyr::filter(end == max(end)) %>%
      dplyr::ungroup()
  }

  rtn
}


# Import xml file
x <- xml2::read_xml("C:/Users/SirAnalyst/Desktop/game.xml")


# Create players table
  # Purpose: This table will be used to hide personal data of the players
  # behind id created  by joining their shirt number with their initials
codes <- xml2::xml_find_all(x, "//code") %>%
  xml2::xml_text() %>%
  unique()

players <- as.data.frame(codes) %>%
  dplyr::filter(grepl("\\d", codes)) %>%
  dplyr::mutate(
    number = as.numeric(gsub("\\-.*", "", codes)),
    name = gsub(".*\\-", "", codes)
  ) %>%
  dplyr::mutate(
    id = sprintf(
      "%s%s",
      number,
      gsub("(\\b[A-Z])[^A-Z]+", "\\1", name)
    )
  ) %>%
  dplyr::arrange(number)


# Scrums table
  # Purpose: Make it easier for the scrum coach to find the scrum events while
  # watching the video and provide basic background information
xpath <- sprintf("//instance[code = '%s']", codes[grep("Scrum", codes)]) %>%
  paste(collapse = " ") %>%
  gsub(x = ., "] /", "] | /")

iters <- xml2::xml_find_all(x, xpath)

scrums <- get_data(iters)


# Plays table
  # Purpose: Create a list of events assigned to the players for easier analysis
iters <- xml2::xml_find_all(x, "ALL_INSTANCES/instance[label]")

plays <- dplyr::inner_join(
  get_data(iters),
  players,
  by = c("code" = "codes")) %>%
  select(id, start, end, labels)


# Players stats table
  # Purpose: Provide formatted statistics ready for further comparative analysis
  # by coaching stuff and players
stats <- plays %>%
  dplyr::select(-c(start, end)) %>%
  dplyr::mutate(labels = strsplit(as.character(labels), ",")) %>%
  tidyr::unnest(labels) %>%
  group_by(id) %>%
  summarise(
    Carries           = sum(labels == "Carries"),
    Scrum_Pickup      = sum(labels == "Scrum Pickup"),
    Try_Scored        = sum(labels == "Try Scored"),
    Contact_Area      = sum(labels == "Contact Area"),
    Tackle_Made       = sum(labels == "Tackle Made"),
    Tackle_Missed     = sum(labels == "Tackle Missed"),
    High_Ball_Catches = sum(labels == "High Ball Catches"),
    In_Play_Kicks     = sum(labels == "In Play Kicks"),
    Kick_at_Posts     = sum(labels == "Kick at Posts"),
    Kick_at_Touch     = sum(labels == "Kick at Touch"),
    Lineout_Steal     = sum(labels == "Lineout Steal"),
    Lineout_Throw     = sum(labels == "Lineout Throw"),
    Penalty_Conceded  = sum(labels == "Penalty Conceded"),
    Turnover_Conceded = sum(labels == "Turnover Conceded"),
    Turnover_Won      = sum(labels == "Turnover Won")
  )

stats <- stats[match(players$id, stats$id), ]


