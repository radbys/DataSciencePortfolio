################################################################################
# Title:  Activities in Rugby - Exploratory Data Analysis
# Date:   2021-09-16
# Technologies:
#     * R
#     * Html (Kaggle only)
#     * CSS  (Kaggle only)
# Libraries:
#   * tidyverse
#   * stringi
#   * lubridate
#   * ggthemes
#   * gridExtra
#   * gtable
# Tasks:
#   * Data wrangle
#   * Feature engineering
#   * EDA
# Exteral link:
#   * https://www.kaggle.com/radbys/activities-in-rugby-exploratory-data-analysis
################################################################################

# Environment settings
  # Loading libraries
  library(tidyverse)
  library(stringi)
  library(lubridate)
  library(ggthemes)
  library(gridExtra)
  library(grid)
  library(gtable)

  # Plot settings
  ggplot2::theme_set(ggthemes::theme_fivethirtyeight())

  ggplot2::theme_update(
    text = ggplot2::element_text(size = 20),
    plot.title = ggplot2::element_text(size = 26, face = "bold"),
    strip.text = ggplot2::element_text(size = 20, face = "bold", colour = "black"),
    strip.background = ggplot2::element_rect(fill = "darkgrey"),
    legend.key.size = ggplot2::unit(0.5, "cm"),
    legend.key.width = ggplot2::unit(0.5, "cm")
  )

  colors <- c("#0a1128", "#001f54", "#aa8f66", "#ffff82", "grey")

  # Default ggplot2 colors
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }


  # Data wranlge
  # Data Import
  zdf <- read.csv("../input/garmin/Zeke.csv")
  sdf <- read.csv("../input/garmin/Stachu.csv")
  wdf <- read.csv("../input/garmin/Wiaan.csv")

  # Data homogeneity check
  dim(zdf)
  dim(sdf)
  dim(wdf)

  cbind(
    head(names(zdf)),
    head(names(sdf)),
    head(names(wdf))
  )

  # Data unification
  sdf <- sdf %>% dplyr::select(1:21, 27:37)
  zdf <- zdf %>% dplyr::select(1:21, 24:30, 38:41)
  names(sdf) <- names(wdf)[-10]
  names(zdf) <- names(wdf)[-10]

  df <- rbind(
    wdf %>% dplyr::select(-10) %>% dplyr::mutate(Person = "WG"),
    sdf %>% dplyr::mutate(Person = "SPN"),
    zdf %>% dplyr::mutate(Person = "PZ")
  )

  dplyr::glimpse(df)

  # Simple conversions
  df$Date <- as.POSIXct(df$Date)
  df$Favorite <- as.logical(df$Favorite)
  df$Distance <- as.numeric(ifelse(df$Distance > 200,
    as.numeric(df$Distance) / 1000,
    gsub(",", ".", df$Distance)
  ))
  df$Calories <- as.numeric(gsub(",", "", df$Calories))
  df$Avg.HR <- as.numeric(df$Avg.HR)
  df$Max.HR <- as.numeric(df$Max.HR)
  df$Avg.Run.Cadence <- as.numeric(df$Avg.Run.Cadence)
  df$Max.Run.Cadence <- as.numeric(df$Max.Run.Cadence)
  df$Total.Ascent <- as.numeric(df$Total.Ascent)
  df$Total.Descent <- as.numeric(df$Total.Descent)
  df$Number.of.Laps <- as.numeric(df$Number.of.Laps)
  df$Min.Elevation <- as.numeric(df$Min.Elevation)
  df$Max.Elevation <- as.numeric(df$Max.Elevation)

  # Conversions into minutes
  df$Time <- lubridate::period_to_seconds(lubridate::hms(df$Time)) / 60
  df$Best.Lap.Time <- lubridate::period_to_seconds(lubridate::hms(df$Best.Lap.Time)) / 60
  df$Moving.Time <- lubridate::period_to_seconds(lubridate::hms(df$Moving.Time)) / 60
  df$Elapsed.Time <- lubridate::period_to_seconds(lubridate::hms(df$Elapsed.Time)) / 60
  df$Surface.Interval <- lubridate::period_to_seconds(lubridate::hms(df$Surface.Interval)) / 60

  # Speed-tempo conversions
  tempo_to_speed <- function(x) {
    rtn <- as.numeric(
      ifelse(grepl("\\:", x),
        round(60 / (lubridate::period_to_seconds(lubridate::ms(x)) / 60), 1),
        ifelse(grepl("\\.", x),
          x,
          NA
        )
      )
    )
  }

  speed_to_tempo <- function(x) {
    rtn <- as.numeric(ifelse(grepl("\\.", x),
      round(60 / as.numeric(x), 1),
      ifelse(grepl(":", x),
        round(lubridate::period_to_seconds(lubridate::ms(x)) / 60, 1),
        NA
      )
    ))
  }

  df$Avg.Tempo <- speed_to_tempo(df$Avg.Speed)
  df$Min.Tempo <- speed_to_tempo(df$Max.Speed)

  df$Avg.Speed <- tempo_to_speed(df$Avg.Speed)
  df$Max.Speed <- tempo_to_speed(df$Max.Speed)

  # String manipulation
  df <- df %>%
    dplyr::mutate(Activity.Type = stringi::stri_replace_all_fixed(
      Activity.Type,
      c(
        "Kardio", "Kolarstwo", "Bieganie", "Chodzenie", "PĹ‚ywanie w basenie", "WioĹ›larstwo", "Bieg na bieĹĽni", "Bieg przeĹ‚ajowy",
        "Jazda na rowerze treningowym", "SiĹ‚ownia i sprzÄ™t fitness", "Trening siĹ‚owy", "Oddech", "Inne", "Wykryto zdarzenie"
      ),
      c(
        "Cardio", "Cycling", "Running", "Walking", "Swimming", "Rowing", "Treadmill Running", "Trail Running",
        "Indoor Cycling", "Strength training", "Strength training", "Breathing", "Other", "Other"
      ),
      vectorize_all = FALSE
    ))

  df <- df %>%
    dplyr::mutate(Title = stringi::stri_replace_all_regex(
      Title,
      c(
        ".*Bieg.*", ".*Run.*", "Kardio", ".*Cardio.*", ".*SiĹ.*", ".*Strength.*", ".*Spacerowanie.*", ".*Walking.*", ".*Kolarstwo.*",
        ".*Cycl.*", ".*Rugby.*", ".*Hik.*", ".*Golf.*", ".*x.*", "Relaks.*", "PĹ‚ywanie.*", ".*Wykryto zdarzenie"
      ),
      c(
        "Running", "Running", "Cardio", "Cardio", "Strength training", "Strength training", "Walking", "Walking", "Cycling", "Cycling",
        "Rugby", "Hiking", "Golf", "Intervals", "Relax", "Swimming", "Event found"
      ),
      vectorize_all = FALSE,
      case_insensitive = TRUE
    ))

  df <- df %>%
    dplyr::mutate(Decompression = stringi::stri_replace_all_fixed(Decompression, "Nie", "No"))

  # Excluding close to zero variance and Favorite column
  ival <- apply(df, 2, function(x) {
    length(unique(x))
  })

  favourite_column <- df$Favorite
  idx <- which(ival == 1)
  df <- df[, -idx]


# Feature engineering
  # Date vars
  df <- df %>%
    dplyr::mutate(
      Month = lubridate::month(Date),
      Week  = lubridate::week(Date),
      Day   = lubridate::wday(Date, week_start = 1),
      Hour  = lubridate::hour(Date)
    )

  # Team practice
  df <- df %>%
    dplyr::mutate(Team_practice = ifelse(Day %in% c(1:2, 4:5) & Hour %in% c(18, 19), 1, 0)) %>%
    dplyr::mutate(Team_practice = dplyr::case_when(
      Person == "SPN" & Team_practice == 1 & Day < 4 ~ 0,
      Person == "WG" & Date < "2021-03-01" ~ 0,
      T ~ Team_practice
    ))

  # Grouping types and titles
  df <- df %>%
    dplyr::mutate(
      Activity.Type = stringi::stri_replace_all_regex(
        Activity.Type,
        c(".*running.*", ".*cycling*", "Breathing|Swimming|Golf|Joga|Pilates|Rowing|Stoper"),
        c("Running", "Cycling", "Other"),
        vectorize_all = FALSE,
        case_insensitive = TRUE
      )
    )

  out <- df$Title %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    as.data.frame() %>%
    dplyr::filter(Freq < 10) %>%
    dplyr::pull(1)

  df <- df %>%
    dplyr::mutate(Title = ifelse(Title %in% out, "Other", Title))


# Exploratory Data Analysis
  # Personal preferences
    # Favorite workouts
    df %>%
      dplyr::select(Person, Title) %>%
      dplyr::group_by(Person, Title) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(position = rank(-n)) %>%
      ggplot2::ggplot(aes(Person, n, fill = Title, group = position)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge", col = "black") +
      ggplot2::ggtitle("Number of activities performed by type") +
      ggplot2::scale_fill_manual(values = gg_color_hue(8))

    # Activities by months
    my_colors <- colors[c(2, 5, 3, 1)]
    names(my_colors) <- c("PZ", "Season", "SPN", "WG")

    df %>%
      dplyr::select(Person, Month) %>%
      ggplot2::ggplot(aes(Month)) +
      ggplot2::geom_rect(aes(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04) +
      ggplot2::geom_rect(aes(xmin = 8, xmax = 11, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04) +
      ggplot2::geom_area(aes(fill = Person), stat = "count", alpha = 1, col = "black") +
      ggplot2::facet_grid(Person ~ .) +
      ggplot2::ggtitle("Number of performed activities by month") +
      ggplot2::scale_x_continuous(breaks = 1:12) +
      ggplot2::scale_fill_manual(
        "",
        values = my_colors,
        breaks = c("Season")
      ) +
      ggplot2::theme(legend.text = ggplot2::element_text(size = 20))

    # Activities by hours
    df %>%
      dplyr::select(Person, Hour) %>%
      dplyr::mutate(Hour = substring(Hour, 1, 2)) %>%
      ggplot2::ggplot(aes(factor(Hour, levels = c(0:23)), group = Person, fill = Person)) +
      ggplot2::geom_histogram(stat = "count", position = "dodge") +
      ggplot2::facet_grid(Person ~ .) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_fill_manual(values = colors[c(2, 3, 1)]) +
      ggplot2::ggtitle("Number of performed activities by hour of the day")

    # Activities by days
    df %>%
      dplyr::select(Person, Day) %>%
      ggplot2::ggplot(aes(Day, group = Person, fill = Person)) +
      ggplot2::geom_density(kernel = "gaussian") +
      ggplot2::facet_grid(Person ~ .) +
      ggplot2::scale_x_continuous(
        breaks = 1:7,
        labels = c(
          "Monday", "Tuesday", "Wednesday", "Thursday",
          "Friday", "Saturday", "Sunday"
        )
      ) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_fill_manual(values = colors[c(2, 3, 1)]) +
      ggplot2::ggtitle("Performed activities distribution across the week")

    # Training frequency per day of the week
    df %>%
      dplyr::select(Person, Week, Team_practice) %>%
      dplyr::group_by(Person, Week, Team_practice) %>%
      dplyr::summarise(n = n()) %>%
      ggplot2::ggplot(aes(Person, n, fill = Person)) +
      ggplot2::geom_boxplot(col = "grey", size = 1.08, alpha = 0.9) +
      ggplot2::scale_fill_manual(values = colors[c(2, 3, 1)]) +
      ggplot2::scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 2)) +
      ggplot2::facet_grid(. ~ Team_practice, labeller = ggplot2::as_labeller(c("0" = "Individual", "1" = "Team"))) +
      ggplot2::ggtitle("Box plot of activities durning the week") +
      ggplot2::theme(legend.position = "none")

  # Season vs off season
    # Activity type by months
    ord <- dimnames(table(df$Title))[[1]]

    df %>%
      dplyr::select(Month, Title) %>%
      ggplot2::ggplot(aes(Month, fill = Title)) +
      ggplot2::geom_rect(aes(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04) +
      ggplot2::geom_rect(aes(xmin = 8, xmax = 11, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04) +
      ggplot2::geom_bar(position = "fill", col = "black", alpha = 0.95) +
      ggplot2::scale_x_continuous(breaks = 1:12) +
      ggplot2::scale_y_continuous(limits = c(-0.1, 1.1)) +
      ggplot2::scale_fill_manual("",
        values = c(gg_color_hue(8), "grey"),
        breaks = c(ord, "Season")
      ) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
      ggplot2::ggtitle("Performed activities by months")

    # Duration of activity type by months
    col <- c(gg_color_hue(12), "grey")
    names(col) <- c(1:12, "Season")

    plot <- df %>%
      dplyr::select(Month, Time) %>%
      ggplot2::ggplot(aes(Month, Time, group = Month, fill = factor(Month))) +
      ggplot2::geom_rect(aes(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04) +
      ggplot2::geom_rect(aes(xmin = 8, xmax = 11, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_x_continuous(breaks = 1:12) +
      ggplot2::scale_y_continuous(limits = c(0, 150)) +
      ggplot2::scale_fill_manual("",
        breaks = "Season",
        values = col
      ) +
      ggplot2::ggtitle("Duration of activities by months")


    table <- df %>%
      dplyr::select(Month, Time) %>%
      dplyr::group_by(Month) %>%
      dplyr::summarise(
        Q1 = quantile(Time, 0.25),
        Q2 = quantile(Time, 0.5),
        Q3 = quantile(Time, 0.75)
      ) %>%
      dplyr::ungroup() %>%
      round(., 3)


    table <- table %>%
      t() %>%
      .[-1, ] %>%
      as.data.frame()

    names(table) <- 1:12


    theme <- gridExtra::ttheme_default(
      core = list(
        padding = ggplot2::unit(c(17, 8), "mm"),
        fg_params = list(fontsize = 15)
      ),
      colhead = list(
        padding = ggplot2::unit(c(10, 8), "mm"),
        fg_params = list(fontsize = 20)
      ),
      rowhead = list(
        padding = ggplot2::unit(c(10, 8), "mm"),
        fg_params = list(fontsize = 20)
      )
    )

    tbl <- gridExtra::tableGrob(table, theme = theme)

    gridExtra::grid.arrange(plot, tbl,
      nrow = 2, heights = c(5, 2)
    )

    # Average training difficulty
    col <- colors[c(2, 3, 1, 5)]
    names(col) <- c("Avg.HR", "Calories", "Max.HR", "Season")

    df %>%
      dplyr::select(Month, Avg.HR, Max.HR, Calories) %>%
      dplyr::group_by(Month) %>%
      dplyr::summarise(
        Avg.HR = mean(Avg.HR, na.rm = TRUE),
        Max.HR = mean(Max.HR, na.rm = TRUE),
        Calories = mean(Calories, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      tidyr::gather(
        key = "Var",
        value = "Value",
        -Month
      ) %>%
      ggplot2::ggplot(aes(Month, Value)) +
      ggplot2::geom_rect(aes(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04) +
      ggplot2::geom_rect(aes(xmin = 8, xmax = 11, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04) +
      ggplot2::geom_smooth(aes(col = Var, fill = Var)) +
      ggplot2::facet_wrap(~Var, scales = "free_y") +
      ggplot2::scale_fill_manual(values = col) +
      ggplot2::scale_colour_manual("", values = col, breaks = "Season") +
      ggplot2::guides(fill = "none") +
      ggplot2::ggtitle("Average training difficulty")

    # Activities performed by month and type
    col <- colors[c(2, 3, 5)]
    names(col) <- c(0, 1, "Season")

    df %>%
      dplyr::select(Month, Team_practice) %>%
      count(Team_practice, Month) %>%
      ggplot2::ggplot(aes(Month, n, fill = factor(Team_practice))) +
      ggplot2::geom_rect(aes(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04) +
      ggplot2::geom_rect(aes(xmin = 8, xmax = 11, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04) +
      ggplot2::geom_col(col = "black") +
      ggplot2::scale_x_continuous(breaks = 1:12) +
      ggplot2::scale_fill_manual("",
        values = col,
        breaks = "Season"
      ) +
      ggplot2::facet_wrap(Team_practice ~ .,
        labeller = ggplot2::as_labeller(c("0" = "Individual", "1" = "Team")),
        scales = "free_y"
      ) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Number of activities performed by month and type")

  # Practice characteristics
  data <- df %>%
    dplyr::filter(Title != "Other") %>%
    dplyr::select(
      "Title", "Distance", "Calories", "Time", "Avg.HR", "Max.HR",
      "Avg.Speed", "Max.Speed"
    ) %>%
    dplyr::filter(Max.Speed < 90 | is.na(Max.Speed)) %>%
    dplyr::group_by(Title) %>%
    dplyr::summarise_all(mean, na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(vars(-Title), round, 2) %>%
    dplyr::arrange(desc(Title)) %>%
    dplyr::select(
      Title, Avg.HR, Avg.Speed, Calories,
      Distance, Max.HR, Max.Speed, Time
    ) %>%
    dplyr::mutate(Title = ifelse(Title == "Strength training", "Strength", Title))

  p <- data %>%
    tidyr::gather("var", "value", -Title) %>%
    dplyr::arrange(var, desc(value)) %>%
    ggplot2::ggplot(aes(Title, value, fill = var)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(. ~ var,
      ncol = 3,
      scales = "free_x"
    ) +
    ggplot2::scale_fill_manual(values = colors[rep(c(2, 3, 1), 3)]) +
    ggplot2::coord_flip() +
    ggplot2::theme(legend.position = "none")

  gp <- ggplot2::ggplotGrob(p)

  gp0 <- gtable::gtable_add_grob(
    x = gp,
    grobs = gridExtra::tableGrob(
      data,
      theme = gridExtra::ttheme_default(
        base_size = 20,
        core = list(padding = ggplot2::unit(c(5, 12), "mm"))
      ),
      rows = NULL
    ),
    t = 16,
    l = 10,
    b = 21,
    r = 12,
    name = ""
  )
  grid::grid.draw(gp0)
