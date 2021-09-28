################################################################################
# Title:  NBA - data preparation
# Date:   2020-04-04
# Technologies:
#     * R
#     * Html (Kaggle only)
#     * CSS  (Kaggle only)
# Libraries:
#   * tidyverse
#   * ggthemes
#   * ggridges
#   * ggforce
#   * knitr
#   * kableExtra
#   * outliers
#   * lubridate
#   * cowplot
#   * readxl
# Tasks:
#   * Data Overview
#   * Data Cleansing
#   * Outliers Study
#   * Feature Engineering
#   * Clustering Analysis
# Exteral link:
#   * https://www.kaggle.com/radbys/nba-data-preparation
################################################################################

# Environment settings
  # Loading libraries
  library(tidyverse)
  library(ggthemes)
  library(ggridges)
  library(ggforce)
  library(knitr)
  library(kableExtra)
  library(outliers)
  library(lubridate)
  library(cowplot)
  library(readxl)

  # Plot settings
  theme_set(theme_fivethirtyeight())

  theme_update(
    text = element_text(size = 20),
    plot.title = element_text(size = 26, face = "bold"),
    strip.text = element_text(size = 20, face = "bold", colour = "black"),
    strip.background = element_rect(fill = "darkgrey"),
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  )


  colors <- c("#0a1128", "#001f54", "#aa8f66", "#ffff82")

  # User made functions
  movavg <- function(x, y = 1) {
    if (class(x)[1] == "integer" | class(x)[1] == "numeric") {
      x
    } else if (dim(x)[2] > 1) {
      warning(`Choose a column`)
      break
    } else {
      x <- x %>% pull()
    }

    v <- rep(0, length(x))

    for (i in 1:y) {
      v <- v + lag(x, i)
    }

    v <- round(v / y, 2)
    return(v)
  }

# Original data Overview
  # Games
  g <- read_excel("../input/projectnba/2007-2017 Mecze.xlsx")
  names(g) <- c("Game_ID", "Date", "x1", "x2")

  kable(head(g)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

  kable(summary(g)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

  glimpse(g)

  # Teams
  t <- read_excel("../input/projectnba/2007-2017 Druzyny.xlsx")
  names(t) <- c("Team_ID", "Team_Name", "Team_Short")

  kable(head(t)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    scroll_box(width = "100%")

  kable(summary(t)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

  glimpse(t)

  # Odds
  o <- read_excel("../input/projectnba/2007-2017 Kursy.xlsx")
  names(o) <- c("Game_ID", "x1_Od", "x2_Od")

  kable(head(o)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    scroll_box(width = "100%")

  kable(summary(o)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

  glimpse(o)

  # Lines
  l <- read_excel("../input/projectnba/2007-2017 Linie.xlsx")
  names(l) <- c("Game_ID", "Line")

  kable(head(l)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    scroll_box(width = "100%")

  kable(summary(l)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

  glimpse(l)

  # Points
  p <- read_excel("../input/projectnba/2007-2017 Zdobyte.xlsx")
  names(p) <- c("Game_ID", "x1_P", "x2_P")

  kable(head(p)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    scroll_box(width = "100%")

  kable(summary(p)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

  glimpse(p)


  # Budgets
  b <- read_excel("../input/projectnba/2007-2017 Budzety.xlsx")
  names(b) <- c("Game_ID", "x1_B", "x2_B")

  kable(head(b)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    scroll_box(width = "100%")

  kable(summary(b)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

  glimpse(b)

  # Stats
  s <- read_excel("../input/projectnba/2007-2017 Druzynowe.xlsx")
  names(s) <- c("Game_ID", names(s)[-1])

  kable(head(s)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    scroll_box(width = "100%")

  kable(summary(s)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    scroll_box(width = "100%")

  glimpse(s)

# Data wrangle
  # Creating main df
  main <- left_join(g, t, by = c("x1" = "Team_ID")) %>%
    # Games and Teams
    select(Game_ID, Date, x1, x2, x1_N = Team_Short) %>%
    left_join(., t, by = c("x2" = "Team_ID")) %>%
    select(Game_ID, Date, x1, x2, x1_N, x2_N = Team_Short) %>%
    left_join(o, by = "Game_ID") %>%
    # Odds
    left_join(l, by = "Game_ID") %>%
    # Lines
    left_join(p, by = "Game_ID") %>%
    # Points
    left_join(b, by = "Game_ID") %>%
    # Budgets
    left_join(s, by = "Game_ID")

  kable(head(main)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    scroll_box(width = "100%")


# Data cleansing
  # NA`s
    # check for NA`s
    colSums(is.na(main))

    # NA`s column name
    for (i in 1:ncol(main)) {
      if (sum(is.na(main[, i])) > 0) {
        print(paste("Outlier in:", names(main[, i])))
      }
    }

    # NA row
    paste("Game_ID: ", which(is.na(main$x1_N)))
    paste("Game_ID: ", which(is.na(main$x1_B)))

    # glimpse at na rows
    kable(main[12438, 1:13]) %>%
      kable_styling()

    # replace NAs
    main[12438, 3] <- 20
    main[12438, 5] <- "NYK"
    main[12438, 10] <- 104
    main[12438, 12] <- 103062240

    # verryfing NA reduction
    kable(main[12438, 1:13]) %>%
      kable_styling()


  # Outliers
    # Determining outlier

    # percetntage of outliers depending on threshold
    ot <- main[, -1:-9]
    thres1 <- c(1:5)
    thres2 <- c(1:5)

    for (j in 2:5) {
      for (i in 10:51) {
        ot[, (i - 9)] <- ifelse(round(abs(scores(main[, i])), 2) > j, T, F)
      }
      thres1[j - 1] <- ot %>%
        mutate(sumVar = ifelse(rowSums(.[1:41]) > 0, T, F)) %>%
        select(sumVar) %>%
        summarize(round(sum(sumVar) / nrow(ot), 2)) %>%
        pull()
      thres2[j - 1] <- ot %>%
        colSums() %>%
        sum() / (nrow(ot) * ncol(ot))
    }

    kable(data.frame(
      "Threshold_value" = c(1:5),
      "Games_Perc" = thres1,
      "Games_num" = round(thres1 * nrow(main), 0),
      "Obser_Perc" = round(thres2, 2),
      "Obser_%num" = round(thres2 * (nrow(ot) * ncol(ot)))
    )) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

    tres <- 3

    # Outlier indicating
    ot <- main[, -1:-9]
    sc <- main[, -1:-9]

    for (i in 10:51) {
      ot[, (i - 9)] <- ifelse(round(abs(scores(main[, i])), 2) > tres, T, F)
    }

    ot %>%
      summarise_each(funs(sum)) %>%
      gather(var, outliers) %>%
      filter(outliers > 0) %>%
      arrange(desc(outliers)) %>%
      kable() %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
      scroll_box(height = "200px")

    # Joining data
    ot <- data.frame(main[1:2], ot)

    ob <- data.frame(
      gather(ot, var, outlier, -Game_ID, -Date),
      gather(main[, c(1:2, 10:51)], var, value, -Game_ID, -Date)[4]
    )

    ob %>%
      filter(var != "x1_B" & var != "x2_B") %>%
      ggplot(aes(var, value)) +
      geom_boxplot(aes(fill = outlier)) +
      facet_grid(~outlier,
        scales = "free",
        labeller = labeller(outlier = setNames(c("'Clean' data", "Outliers"), c(F, T)))
      ) +
      coord_flip() +
      labs(
        title = "Outliers",
        subtitle = "Quick preview",
        x = "",
        y = ""
      ) +
      theme(legend.position = "none") +
      scale_fill_manual(values = c(colors[2], colors[3]))

  # Outliers study
    #MINs
    mins <- ob %>%
      filter(var == "x1_MIN" | var == "x2_MIN")

    ggplot() +
      geom_rect(aes(xmin = 0, xmax = 12897, ymin = 240, ymax = 340), fill = "darkgrey", alpha = 0.4) +
      geom_point(data = mins, aes(Game_ID, value, col = outlier), size = 2) +
      geom_curve(aes(x = 10438, xend = 12400, y = 210, yend = 201),
        curvature = -0.5,
        arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
        size = 1, colour = "darkgrey"
      ) +
      annotate("text", label = "outside value", x = 11350, y = 230, colour = "darkgrey", fontface = "bold", size = 4) +
      labs(
        title = "Scatter plot of MINs",
        subtitle = "with possible values area tinted",
        x = "",
        y = ""
      ) +
      scale_color_manual(values = c(colors[2], colors[3])) +
      guides(colour = guide_legend(override.aes = list(size = 10)))


    # Row to change
    row <- which(main$Game_ID == mins %>%
      filter(value < 220) %>%
      select(Game_ID) %>%
      pull())

    # making a change
    main[row, "x1_MIN"] <- 240

    # TOs & STLs
    ob %>%
      filter(var == "x1_STL" | var == "x2_STL" | var == "x1_TOV" | var == "x2_TOV") %>%
      ggplot(aes(Game_ID, value, col = outlier)) +
      geom_point(size = 2) +
      facet_grid(var ~ outlier) +
      labs(
        title = "Scatter plot of TOs and STLs",
        subtitle = "grouped by veriables",
        x = "",
        y = ""
      ) +
      scale_color_manual(values = c(colors[2], colors[3])) +
      guides(colour = guide_legend(override.aes = list(size = 10))) +
      theme(
        strip.background.x = element_blank(),
        strip.text.x = element_blank()
      )

    #PF & FTA
    a <- ob %>%
      filter((var == "x1_PF" | var == "x2_FTA") & outlier == T) %>%
      select(Game_ID, var, value)
    b <- ob %>%
      filter((var == "x2_PF" | var == "x1_FTA") & outlier == T) %>%
      select(Game_ID, var, value)

    aa <- ggplot(a, aes(Game_ID, value, col = var)) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", size = 2) +
      scale_y_continuous(limits = c(30, 54)) +
      theme(legend.position = "none") +
      scale_color_manual(values = c(colors[2], colors[3])) +
      labs(
        title = "Line plots of FTAs and PF",
        subtitle = "grouped by teams",
        x = "",
        y = ""
      )

    bb <- ggplot(b, aes(Game_ID, value, col = var)) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", size = 2) +
      scale_y_continuous(limits = c(30, 54)) +
      theme(legend.position = "none") +
      scale_color_manual(values = c(colors[3], colors[2]))


    ggdraw() +
      draw_plot(aa, x = 0, y = 0.45, width = 1, height = 0.55) +
      draw_plot(bb, x = 0, y = 0, width = 1, height = 0.46)

    # PTS
    row <- ob %>%
      filter((var == "x1_P" | var == "x2_P" | var == "x1_FGM" | var == "x2_FGM") & outlier == T) %>%
      select(Game_ID) %>%
      pull()

    main[row, c("Game_ID", "x1_P", "x2_P", "x1_FGM", "x2_FGM")] %>%
      gather(var, value, -Game_ID) %>%
      ggplot(aes(x = Game_ID)) +
      geom_line(aes(y = value), size = 1, col = colors[2]) +
      facet_grid(var ~ ., scales = "free_y") +
      labs(
        title = "Time series comparision",
        subtitle = "Points scored by field goals made",
        x = "",
        y = ""
      ) +
      geom_line(
        data = . %>% filter(var %in% c("x1_P", "x2_P") & Game_ID < 250),
        aes(y = value),
        col = colors[3],
        size = 2
      )

    # Outlier rows
    main %>%
      select(Game_ID, x1_P, x2_P, x1_FGM, x2_FGM) %>%
      filter(x1_P < 50 | x2_P < 50) %>%
      kable() %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

    # OKC(SEA) vs GSW:
    main[248, 10:11] <- c(96, 109)
    # LAL vs ORL:
    main[249, 10:11] <- c(97, 104)

    # FGs
    fg <- main[
      (ob %>%
        filter((var == "x1_FGM" | var == "x1_FGA" | var == "x1_FG." |
          var == "x2_FGM" | var == "x2_FGA" | var == "x2_FG.") & outlier == T) %>%
        select(Game_ID) %>%
        arrange(Game_ID) %>%
        pull()),
      c("Game_ID", "x1_FGM", "x1_FGA", "x1_FG%", "x2_FGM", "x2_FGA", "x2_FG%")
    ]


    fg %>%
      mutate(
        x1_calc = round(x1_FGM / x1_FGA, 2) * 100,
        x2_calc = round(x2_FGM / x2_FGA, 2) * 100
      ) %>%
      ggplot() +
      geom_point(aes(`x1_FG%`, x1_calc + 5), col = colors[3], size = 2) +
      geom_point(aes(`x2_FG%`, x2_calc - 5), col = colors[2], size = 2) +
      annotate("text", x = 57.5, y = 32.5, label = "x1_FGM/x1_FGA to x1_FG%", size = 6, col = colors[2]) +
      annotate("text", x = 32.5, y = 57.5, label = "x1_FGM/x1_FGA to x1_FG%", size = 6, col = colors[3]) +
      labs(
        title = "Scatter plot of FG%",
        subtitle = "Comparison of calculated percentage vs actual one",
        x = "",
        y = ""
      )


    # Budgets
    bo <- bind_rows(
      main %>% select(Date, t = x1_N, b = x1_B) %>% mutate(y = "x1", sc = scores(b)),
      main %>% select(Date, t = x2_N, b = x2_B) %>% mutate(y = "x2", sc = scores(b))
    ) %>%
      mutate(Date = year(Date))

    bc <- bo %>%
      group_by(Date) %>%
      mutate(max = max(b), min = min(b)) %>%
      filter(b == max | b == min) %>%
      select(Date, t, b) %>%
      distinct(t, b) %>%
      ungroup()

    ggplot(bo, aes(y = b)) +
      geom_boxplot() +
      facet_grid(~Date) +
      labs(
        x = "",
        y = ""
      ) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank()
      ) +
      geom_text(
        data = bc,
        aes(x = 1, y = b, label = t),
        col = colors[2],
        fontface = "bold",
        hjust = 1,
        size = 4
      ) +
      labs(
        title = "Budget boxplots",
        subtitle = "With max and min values\n",
        x = "",
        y = ""
      )

    # FTs
    fts <- ob %>%
      filter(var == "x1_FT." | var == "x1_FTM" | var == "x2_FT." | var == "x2_FTM")

    elip <- data.frame(
      var = c("x1_FT.", "x2_FT.", "x2_FTM"),
      x0 = c(10700, 5450, 8600),
      y0 = c(22, 14, 52),
      a = c(2700, 1100, 1100),
      b = c(10, 5, 5),
      angle = c(0, 0, 0)
    )

    ggplot() +
      geom_point(data = fts, aes(Game_ID, value, col = outlier), size = 2) +
      geom_ellipse(data = elip, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle), col = NA, fill = "darkgrey", alpha = 0.4) +
      facet_grid(~var) +
      scale_color_manual(values = c(colors[2], colors[3])) +
      guides(colour = guide_legend(override.aes = list(size = 10))) +
      labs(
        title = "FT% and FTM scatter plot",
        subtitle = "with shaded outliers",
        x = "",
        y = ""
      )

    # 3Ps
    tp <- ob %>%
      filter((var %in% c("x1_3PA", "x2_3PA")))

    ggplot() +
      geom_ellipse(aes(x0 = 1150, y0 = 45, a = 400, b = 3, angle = 0),
        col = NA, fill = "darkgrey", alpha = 0.4
      ) +
      geom_ellipse(aes(x0 = 3000, y0 = 47, a = 400, b = 3, angle = 0),
        col = NA, fill = "darkgrey", alpha = 0.4
      ) +
      geom_point(data = tp, aes(Game_ID, value, col = outlier), size = 2) +
      scale_color_manual(values = c(colors[2], colors[3])) +
      guides(colour = guide_legend(override.aes = list(size = 10))) +
      labs(
        title = "Scatter plot of 3Ps",
        subtitle = "with shaded outliers",
        x = "",
        y = ""
      )

    main[which((main$x1_3PA > 42 | main$x2_3PA > 42) & main$Game_ID < 5000), 1:6]

    # REBs
    ob %>%
      filter(var %in% c("x1_REB", "x2_REB", "x1_OREB", "x2_OREB", "x1_DREB", "x2_DREB") &
        outlier == T) %>%
      ggplot(aes(Game_ID, value, col = var)) +
      geom_point(size = 2) +
      facet_wrap(var ~ ., ncol = 3) +
      labs(
        title = "Scatter plot of REB",
        subtitle = "Comparison of calculated percentage vs actual one",
        x = "",
        y = ""
      ) +
      scale_color_manual(values = c(colors[3], colors[2], "darkgrey", colors[3], colors[2], "darkgrey")) +
      theme(legend.position = "none")


# Feature engineering
  # Basic stats
    # Points sum
    main <- main %>%
      mutate(Sum_P = x1_P + x2_P)

    cbp <- as.data.frame(table(cut(main$Sum_P, 13))) %>%
      rename(ans = 1, var = 2) %>%
      mutate(val = "Sum_P")


    # O/U
    main <- main %>%
      mutate(O_U = ifelse(Sum_P > Line, 1, 0))

    cbp <- bind_rows(
      cbp,
      as.data.frame(table(main$O_U)) %>%
        rename(ans = 1, var = 2) %>%
        mutate(val = "O/U")
    )

    # OT
    main <- main %>%
      mutate(OT = case_when(
        x1_MIN + x2_MIN > 628 ~ 3,
        x1_MIN + x2_MIN > 576 ~ 2,
        x1_MIN + x2_MIN > 526 ~ 1,
        T ~ 0
      ))

    cbp <- bind_rows(
      cbp,
      as.data.frame(table(main$O_U)) %>%
        rename(ans = 1, var = 2) %>%
        mutate(val = "OT")
    )

    # Conf
    west <- c(7, 8, 10, 11, 13, 14, 15, 18, 19, 21, 24, 25, 26, 27, 29)
    east <- c(1, 2, 3, 4, 5, 6, 9, 12, 16, 17, 20, 22, 23, 28, 30)

    main <- main %>%
      mutate(conf = ifelse(x1 %in% west & x2 %in% west, "w",
        ifelse(x1 %in% east & x2 %in% east, "e", "i")
      ))

    cbp <- bind_rows(
      cbp,
      as.data.frame(table(main$conf)) %>%
        rename(ans = 1, var = 2) %>%
        mutate(val = "Conf")
    )

    # CO
    main <- main %>%
      mutate(
        x1_CO = ifelse(x1 %in% west, "w", "e"),
        x2_CO = ifelse(x2 %in% west, "w", "e")
      )

    cbp <- bind_rows(
      cbp,
      as.data.frame(table(c(main$x1_CO, main$x2_CO))) %>%
        rename(ans = 1, var = 2) %>%
        mutate(val = "CO")
    )

    # Winner
    main <- main %>%
      mutate(
        x1_W = ifelse(x1_P > x2_P, 1, 0),
        x2_W = ifelse(x1_P < x2_P, 1, 0)
      )

    cbp <- bind_rows(
      cbp,
      as.data.frame(table(c(main$x1_W, main$x2_W))) %>%
        rename(ans = 1, var = 2) %>%
        mutate(val = "Win")
    )

    # Form
    main <- inner_join(main,
      bind_rows(
        main %>% select(Game_ID, x = x1, s = x1_W) %>% mutate(t = "x1_F"),
        main %>% select(Game_ID, x = x2, s = x2_W) %>% mutate(t = "x2_F")
      ) %>%
        arrange(Game_ID, t) %>%
        group_by(x) %>%
        mutate(f = movavg(s, 10)) %>%
        ungroup() %>%
        select(-s) %>%
        nest(x, f, .key = `value_col`) %>%
        spread(key = t, value = value_col) %>%
        unnest(x1_F, x2_F, .sep = "_") %>%
        select(Game_ID,
          x1 = x1_F_x,
          x2 = x2_F_x,
          x1_F = x1_F_f,
          x2_F = x2_F_f
        ),
      by = c("Game_ID", "x1", "x2")
    )

    cbp <- bind_rows(
      cbp,
      as.data.frame(table(c(main$x1_F, main$x2_F))) %>%
        rename(ans = 1, var = 2) %>%
        mutate(val = "Form")
    )

    # season
    main <- main %>%
      mutate(ses = case_when(
        Date < ymd("2008-07-01") ~ "07/08",
        Date < ymd("2009-07-01") ~ "08/09",
        Date < ymd("2010-07-01") ~ "09/00",
        Date < ymd("2011-07-01") ~ "10/11",
        Date < ymd("2012-07-01") ~ "11/12",
        Date < ymd("2013-07-01") ~ "12/13",
        Date < ymd("2014-07-01") ~ "13/14",
        Date < ymd("2015-07-01") ~ "14/15",
        Date < ymd("2016-07-01") ~ "15/16",
        T ~ "16/17"
      ))

    cbp <- bind_rows(
      cbp,
      as.data.frame(table(main$ses)) %>%
        rename(ans = 1, var = 2) %>%
        mutate(val = "ses")
    )

    # PO
    main <- main %>%
      mutate(PO = case_when(
        ses == "16/17" & Date >= ymd("2017-04-15") ~ 1,
        ses == "15/16" & Date >= ymd("2016-04-16") ~ 1,
        ses == "14/15" & Date >= ymd("2015-04-18") ~ 1,
        ses == "15/14" & Date >= ymd("2014-04-19") ~ 1,
        ses == "12/13" & Date >= ymd("2013-04-20") ~ 1,
        ses == "11/12" & Date >= ymd("2012-04-28") ~ 1,
        ses == "10/11" & Date >= ymd("2011-04-16") ~ 1,
        ses == "09/10" & Date >= ymd("2010-04-17") ~ 1,
        ses == "08/09" & Date >= ymd("2009-04-18") ~ 1,
        ses == "07/08" & Date >= ymd("2008-04-19") ~ 1,
        T ~ 0
      ))

    cbp <- bind_rows(
      cbp,
      as.data.frame(table(main$PO)) %>%
        rename(ans = 1, var = 2) %>%
        mutate(val = "PO")
    )

    # Last season %
    ls <- bind_rows(
      main %>% select(Game_ID, ses, x = x1, s = x1_W) %>% mutate(t = "x1_F"),
      main %>% select(Game_ID, ses, x = x2, s = x2_W) %>% mutate(t = "x2_F")
    ) %>%
      arrange(Game_ID, t) %>%
      group_by(ses, x) %>%
      summarize(pr = round(sum(s) / n(), 2))


    ls <- bind_rows(
      data.frame(
        ses = rep("06/07", 30),
        x = c(1:30),
        pr = c(
          0.36, 0.29, 0.5, 0.40, 0.59, 0.61, 0.81, 0.54, 0.64, 0.51,
          0.63, 0.42, 0.48, 0.51, 0.26, 0.53, 0.34, 0.39, 0.47, 0.40,
          0.37, 0.48, 0.47, 0.74, 0.39, 0.40, 0.70, 0.57, 0.62, 0.5
        )
      ),
      ls
    )

    ls <- data.frame(
      ses = ls$ses[31:330],
      x = ls$x[1:300],
      pr = ls$pr[1:300]
    )

    main <- left_join(main, ls, by = c("ses", "x1" = "x")) %>% rename(x1_LS = pr)
    main <- left_join(main, ls, by = c("ses", "x2" = "x")) %>% rename(x2_LS = pr)

    cbp <- bind_rows(
      cbp,
      as.data.frame(table(cut(c(main$x1_LS, main$x2_LS), 4))) %>%
        rename(ans = 1, var = 2) %>%
        mutate(val = "LS")
    )

    # Season %
    main <- inner_join(main,
      bind_rows(
        main %>% select(Game_ID, ses, x = x1, s = x1_W) %>% mutate(t = "x1_S%"),
        main %>% select(Game_ID, ses, x = x2, s = x2_W) %>% mutate(t = "x2_S%")
      ) %>%
        arrange(Game_ID, t) %>%
        group_by(ses, x) %>%
        mutate(pr = round(lag(cummean(s)), 2)) %>%
        ungroup() %>%
        select(-ses, -s) %>%
        nest(x, pr, .key = `value_col`) %>%
        spread(key = t, value = value_col) %>%
        unnest(`x1_S%`, `x2_S%`, .sep = "_") %>%
        select(Game_ID,
          x1 = `x1_S%_x`,
          x2 = `x2_S%_x`,
          `x1_S%` = `x1_S%_pr`,
          `x2_S%` = `x2_S%_pr`
        ),
      by = c("Game_ID", "x1", "x2")
    )

    cbp <- bind_rows(
      cbp,
      as.data.frame(table(cut(c(main$`x1_S%`, main$`x2_S%`), 4))) %>%
        rename(ans = 1, var = 2) %>%
        mutate(val = "S")
    )

    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 4
    to_add <- data.frame(matrix(NA, empty_bar * nlevels(factor(cbp$val)), ncol(cbp)))
    colnames(to_add) <- colnames(cbp)

    to_add$val <- rep(levels(factor(cbp$val)), each = empty_bar)
    data <- rbind(cbp, to_add)
    data <- data %>% arrange(val)
    data$id <- seq(1, nrow(data))
    data <- data %>%
      group_by(val) %>%
      mutate(var = var / max(var, na.rm = TRUE))


    # normalized =

    # Get the name and the y position of each label
    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)


    # prepare a data frame for base lines
    base_data <- data %>%
      group_by(val) %>%
      summarize(start = min(id), end = max(id) - empty_bar) %>%
      rowwise() %>%
      mutate(title = mean(c(start, end)))

    # prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1, ]


    # Make the plot
    ggplot(data, aes(x = as.factor(id), y = var, fill = val)) + # Note that id is a factor. If x is numeric, there is some space between the first bar
      geom_bar(aes(x = as.factor(id), y = var, fill = val), stat = "identity", alpha = 0.5) +
      geom_bar(aes(x = as.factor(id), y = var, fill = val), stat = "identity", alpha = 0.5) +
      ylim(-1.5, 1.1) +
      # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
      geom_segment(
        data = grid_data, aes(x = end, y = 0.80, xend = start, yend = 0.80),
        colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE
      ) +
      geom_segment(
        data = grid_data, aes(x = end, y = 0.60, xend = start, yend = 0.60),
        colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE
      ) +
      geom_segment(
        data = grid_data, aes(x = end, y = 0.40, xend = start, yend = 0.40),
        colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE
      ) +
      geom_segment(
        data = grid_data, aes(x = end, y = 0.20, xend = start, yend = 0.20),
        colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE
      ) +
      coord_polar() +
      geom_text(
        data = label_data, aes(x = id, y = var + 0.1, label = ans, hjust = hjust),
        color = "black", fontface = "bold", alpha = 0.6, size = 8, angle = label_data$angle, inherit.aes = FALSE
      ) +
      geom_segment(
        data = base_data, aes(x = start, y = -0.1, xend = end, yend = -0.1),
        colour = "black", alpha = 1, size = 1, inherit.aes = FALSE
      ) +
      geom_text(
        data = base_data, aes(x = title, y = -0.4, label = val), colour = "black",
        alpha = 0.8, size = 8, fontface = "bold", inherit.aes = FALSE
      ) +
      labs(
        title = "Circle bar plot of basic statistics ",
        subtitle = "grouped by veriable",
        x = "",
        y = ""
      ) +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1, 4), "cm")
      )

  # Advanced stats
    # POSS
    main <- main %>%
      mutate(
        x1_POSS = (0.5 * ((x1_FGA + 0.4 * x1_FTA - 1.07 * (x1_OREB / (x1_OREB + x2_DREB)) * (x1_FGA - x1_FGM)
          + x1_TOV) + (x2_FGA + 0.4 * x2_FTA - 1.07 * (x2_OREB / (x2_OREB + x1_DREB))
          * (x2_FGA - x2_FGM) + x2_TOV))),
        x2_POSS = (0.5 * ((x2_FGA + 0.4 * x2_FTA - 1.07 * (x2_OREB / (x2_OREB + x1_DREB)) * (x2_FGA - x2_FGM)
          + x2_TOV) + (x1_FGA + 0.4 * x1_FTA - 1.07 * (x1_OREB / (x1_OREB + x2_DREB))
          * (x1_FGA - x1_FGM) + x1_TOV)))
      )
    # OFFRTG
    main <- main %>%
      mutate(
        x1_OffRtg = 100 * (x1_P / x1_POSS),
        x2_OffRtg = 100 * (x2_P / x2_POSS)
      )

    # DEFRTG
    main <- main %>%
      mutate(
        x1_DefRtg = x2_OffRtg,
        x2_DefRtg = x1_OffRtg
      )

    # ASF%
    main <- main %>%
      mutate(
        `x1_Ast%` = x1_AST / x1_FGM,
        `x2_Ast%` = x2_AST / x2_FGM
      )

    # AST/TO
    main <- main %>%
      mutate(
        `x1_Ast/To` = x1_AST / (x1_TOV),
        `x2_Ast/To` = x2_AST / (x2_TOV)
      )

    # OREB%
    main <- main %>%
      mutate(
        `x1_OReb%` = (x1_OREB) / (x1_OREB + x2_DREB),
        `x2_OReb%` = (x2_OREB) / (x2_OREB + x1_DREB)
      )

    # DREB%
    main <- main %>%
      mutate(
        `x1_DReb%` = (x2_OREB) / (x2_OREB + x1_DREB),
        `x2_DReb%` = (x1_OREB) / (x1_OREB + x2_DREB)
      )

    # EFG
    main <- main %>%
      mutate(
        x1_Efg = ((x1_FGM + (0.5 * x1_3PM)) / x1_FGA),
        x2_Efg = ((x2_FGM + (0.5 * x2_3PM)) / x2_FGA)
      )

    # TS
    main <- main %>%
      mutate(
        x1_TS = x1_P / (2 * x1_FGA + 0.44 * x1_FTA),
        x2_TS = x2_P / (2 * x2_FGA + 0.44 * x2_FTA)
      )

    # PACE
    main <- main %>%
      mutate(
        x1_Pace = (240 / (x1_MIN)) * ((0.5 * ((x1_FGA + 0.4 * x1_FTA - 1.07 * (x1_OREB / (x1_OREB + x2_DREB)) *
          (x1_FGA - x1_FGM) + x1_TOV) + (x2_FGA + 0.4 * x2_FTA - 1.07 * (x2_OREB / (x2_OREB + x1_DREB)) *
          (x2_FGA - x2_FGM) + x2_TOV))) + (0.5 * ((x2_FGA + 0.4 * x2_FTA - 1.07 *
          (x2_OREB / (x2_OREB + x1_DREB)) * (x2_FGA - x2_FGM) + x2_TOV) +
          (x1_FGA + 0.4 * x1_FTA - 1.07 * (x1_OREB / (x1_OREB + x2_DREB)) *
            (x1_FGA - x1_FGM) + x1_TOV)))) / 2,
        x2_Pace = (240 / (x2_MIN)) * ((0.5 * ((x2_FGA + 0.4 * x2_FTA - 1.07 * (x2_OREB / (x2_OREB + x1_DREB)) *
          (x2_FGA - x2_FGM) + x2_TOV) + (x1_FGA + 0.4 * x1_FTA - 1.07 * (x1_OREB / (x1_OREB + x2_DREB)) *
          (x1_FGA - x1_FGM) + x1_TOV))) + (0.5 * ((x1_FGA + 0.4 * x1_FTA - 1.07 *
          (x1_OREB / (x1_OREB + x2_DREB)) * (x1_FGA - x1_FGM) + x1_TOV) +
          (x2_FGA + 0.4 * x2_FTA - 1.07 * (x2_OREB / (x2_OREB + x1_DREB)) *
            (x2_FGA - x2_FGM) + x2_TOV)))) / 2
      )

    # Plots
    ridges <- main %>%
      select(68:87) %>%
      gather(key, value) %>%
      mutate(key = substr(key, 4, 10))

    aa <- ridges %>%
      filter(key %in% c("POSS", "Pace", "OffRtg", "DefRtg")) %>%
      ggplot(aes(value, key)) +
      geom_density_ridges(aes(fill = key), alpha = 0.8, quantile_lines = TRUE, quantiles = 2) +
      theme(axis.text.x = element_blank()) +
      scale_fill_cyclical(values = c(colors[3], colors[2])) +
      labs(
        title = "Density ridges for advanced statistics",
        subtitle = "with median marked",
        x = "",
        y = ""
      )

    bb <- ridges %>%
      filter(!key %in% c("POSS", "Pace", "OffRtg", "DefRtg", "Ast/To")) %>%
      ggplot(aes(value, key)) +
      geom_density_ridges(aes(fill = key), alpha = 0.8, quantile_lines = TRUE, quantiles = 2) +
      theme(axis.text.x = element_blank()) +
      scale_fill_cyclical(values = c(colors[3], colors[2])) +
      scale_x_continuous(limits = c(0, 1))

    ggdraw() +
      draw_plot(aa, x = 0, y = 0.45, width = 1, height = 0.55) +
      draw_plot(bb, x = 0, y = 0, width = 1, height = 0.46)


  # Variables arrangement and predictors calculating
  main <- main %>%
    select(c(1:6, 62, 12:13, 63:67, 56:57, 55, 54, 58:61, 7:9, 10:11, 52, 53, 14:51, 68:87))

  a <- main %>%
    select(1:3, 5, 7:8, 10:11, 13, 15, 17:19, 21, 23, 25:26, 28:30, seq(32, 86, 2))

  b <- main %>%
    select(1:2, 4, 6:7, 9, 10, 12, 14, 16:18, 20, 22, 24, 25, 27:29, seq(31, 87, 2))

  names(a) <- gsub("x1_", "", names(a))
  names(a) <- gsub("x1", "x", names(a))
  names(b) <- names(a)

  pred <- bind_rows(a, b) %>%
    arrange(Game_ID)

  x1 <- bind_cols(
    pred %>% select(1:2),
    pred %>% select(4:16),
    pred %>%
      select(3, 17:48) %>%
      group_by(x) %>%
      mutate_each(~ movavg(., 10)) %>%
      ungroup()
  ) %>%
    select(1:2, 16, everything())

  x2 <- x1

  pred <- left_join(
    main[, 1:4],
    x1,
    by = c("Game_ID", "Date", "x1" = "x")
  )

  pred <- left_join(
    pred,
    x2,
    by = c("Game_ID", "Date", "x2" = "x"),
    suffix = c("_x1", "_x2")
  )

  vec <- c()

  for (i in 5:49) {
    vec[(length(vec) + 1)] <- i
    vec[(length(vec) + 1)] <- (i + 45)
  }

  pred <- pred %>%
    select(1:4, vec)

  kable(head(na.omit(pred))) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    scroll_box(width = "100%")


# Clustering
  # Data prep
  clust1 <- main %>%
    select(-x2) %>%
    rename(x = x1)

  clust2 <- main %>%
    select(-x1) %>%
    rename(x = x2)

  names(clust1) <- gsub("x1_", "", names(clust1))
  names(clust1) <- gsub("x2_", "OPP_", names(clust1))

  names(clust2) <- gsub("x2_", "", names(clust2))
  names(clust2) <- gsub("x1_", "OPP_", names(clust2))

  clust <- bind_rows(
    clust1,
    clust2
  ) %>%
    arrange(Date) %>%
    select(3, 6, 7, 8, 10, 11, 18:86) %>%
    group_by(x, ses) %>%
    summarise_each(funs(mean(., na.rm = TRUE)))

  scal <- bind_cols(
    clust %>% select(1, 2),
    as.data.frame(scale(clust[, 3:74]))
  )

  kable(head(scal)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    scroll_box(width = "100%")

  # Selecting number of clusters and kmeans implementation
  dis <- 0
  for (i in 1:30) {
    km <- kmeans(scal[, -2], centers = i, nstart = 20)
    dis[i] <- km$tot.withinss
  }

  data.frame(
    clusters = c(1:30),
    dis = dis,
    pos = as.factor(c(rep(1, 2), rep(0, 6), rep(1, 22)))
  ) %>%
    ggplot(aes(clusters, dis)) +
    geom_line(size = 1.5, col = "darkgrey") +
    geom_point(aes(col = pos), size = 3) +
    geom_curve(
      x = 9.5,
      xend = 4.3,
      y = 22000,
      yend = 22900,
      arrow = arrow(length = unit(0.2, "cm")), size = 1.5, colour = colors[2]
    ) +
    annotate("text", x = 6.8, y = 28200, label = "selected value\n k = 4", size = 8) +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = seq(0, 30, 5)) +
    scale_color_manual(values = c(colors[3], colors[2])) +
    labs(
      title = "Elbow plot of total within-cluster sum of squares",
      subtitle = "With highlited possible number of clusters",
      x = "",
      y = ""
    )

  # Modeling
  kmeans_model <- kmeans(scal[, -2], centers = 4, nstart = 20)


# Clusters characteristics
cluster_char <- bind_cols(
  cluster = kmeans_model$cluster,
  scal[, -1:-2]
) %>%
  group_by(cluster) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  round(., 3) %>%
  gather(key, value, -cluster)


  # General
  var <- c("B", "LS", "W", "F", "OPP_F", "Sum_P", "Od", "OPP_Od", "Line", "O_U")

  heatmap <- function(x) {
    cluster_char %>%
      filter(key %in% x) %>%
      ggplot(aes(key, cluster)) +
      geom_tile(aes(fill = value), colour = "black") +
      coord_flip() +
      scale_fill_gradient2(
        midpoint = 0,
        low = colors[2],
        mid = "#f0f0f0",
        high = colors[3],
        space = "Lab"
      ) +
      theme(legend.position = "none") +
      labs(
        title = "General variables heat map",
        subtitle = "Variables ~ Clusters",
        x = "Cluster",
        y = "Variable"
      )
  }

  heatmap(var)

  # Own stats
    # Points
    var <- c("P")
    heatmap(var)

    # FGs
    var <- c("TS", "Efg", "FGM", "FGA", "FG%")
    heatmap(var)

    # 3Ps
    var <- c("3PM", "3PA", "3P%")
    heatmap(var)

    # FTs
    var <- c("FTM", "FTA", "FT%")
    heatmap(var)

    # REBs
    var <- c("OREB", "DREB", "OReb%", "DReb%")
    heatmap(var)

    # ASTs
    var <- c("AST", "Ast%", "x1_Ast/To", "AstRat")
    heatmap(var)

    # STLs BLKs TOVs PFs
    var <- c("STL", "BLK", "TOV", "PF")
    heatmap(var)

    # RTGs
    var <- c("+/-", "OffRtg", "DefRtg")
    heatmap(var)

    # Rest
    var <- c("MIN", "POSS", "Pace")
    heatmap(var)

  # Opponents stats
    # Points
    var <- c("OPP_P")
    heatmap(var)

    #FGs
    var <- c("OPP_TS", "OPP_Efg", "OPP_FGM", "OPP_FGA", "OPP_FG%")
    heatmap(var)

    #3Ps
    var <- c("OPP_3PM", "OPP_3PA", "OPP_3P%")
    heatmap(var)

    #FTs
    var <- c("OPP_FTM", "OPP_FTA", "OPP_FT%")
    heatmap(var)

    #REBs
    var <- c("OPP_OREB", "OPP_DREB", "OPP_OReb%", "OPP_DReb%")
    heatmap(var)

    #ASTs
    var <- c("OPP_AST", "OPP_Ast%", "x2_Ast/To", "OPP_AstRat")
    heatmap(var)

    #STLs BLKs TOVs PFs
    var <- c("OPP_STL", "OPP_BLK", "OPP_TOV", "OPP_PF")
    heatmap(var)

    #RTGs
    var <- c("OPP_+/-", "OPP_OffRtg", "OPP_DefRtg")
    heatmap(var)

    #Rest
    var <- c("OPP_MIN", "OPP_POSS", "OPP_Pace")
    heatmap(var)

# Cluster membership
  # data transformation
  team_clust <- left_join(
    bind_rows(
      main %>%
        select(x = x1, ses, PO, w = x1_W),
      main %>%
        select(x = x2, ses, PO, w = x2_W)
    ) %>%
      group_by(x, ses) %>%
      summarise(
        PO = max(PO),
        W = sum(w)
      ),
    t,
    by = c("x" = "Team_ID")
  ) %>%
    ungroup() %>%
    mutate(x2 = paste(Team_Short, ses)) %>%
    mutate(cluster = kmeans_model$cluster) %>%
    arrange(cluster, x2) %>%
    select(7, 1, 2, 8, 5, 6, 3, 4)

  # add images
  team_clust <- left_join(
    team_clust,
    main %>%
      select(x1_N) %>%
      distinct() %>%
      mutate(
        image = c(
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/25/small_logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/16/small_logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/18/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/5/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/9/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/14/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/2/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/7/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/23/small_logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/24/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/26/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/13/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/30/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/28/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/12/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/11/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/1/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/27/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/6/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/19/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/17/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/4/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/15/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/10/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/22/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/21/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/3/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/8/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/20/logo.png",
          "https://d1si3tbndbzwz9.cloudfront.net/basketball/team/29/small_logo.png"
        )
      ),
    by = c("Team_Short" = "x1_N")
  )

  # plotting
  team_clust %>%
    select(cluster, PO, W) %>%
    group_by(cluster) %>%
    summarise(
      PO = sum(PO),
      W = sum(W)
    ) %>%
    ungroup() %>%
    gather(key, value, -cluster) %>%
    ggplot() +
    geom_line(aes(cluster, value, group = key, col = key), size = 2) +
    facet_grid(key ~ ., scales = "free_y") +
    scale_color_manual(values = c(colors[2], colors[3])) +
    theme(legend.position = "none") +
    labs(
      title = "Line plot of clusters sucess",
      subtitle = "Grouped by playoffs apperances and sum of wins",
      x = "",
      y = ""
    )

  # y scale interval
  sesc <- 1 / 5

  team_clust <- team_clust %>%
    mutate(cluster2 = case_when(
      cluster == 1 ~ 0.5 + sesc,
      cluster == 2 ~ 0.5 + (sesc * 2),
      cluster == 3 ~ 0.5 + (sesc * 3),
      cluster == 4 ~ 0.5 + (sesc * 4)
    ))

  # scale for gganimate
  team_clust <- team_clust %>%
    arrange(x2) %>%
    group_by(Team_Name) %>%
    mutate(n = row_number()) %>%
    ungroup()

  # animated plot
  anim <- function(x) {
    tc1 <- team_clust %>%
      filter(Team_Short %in% ts)

    anim <- ggplot(tc1) +
      geom_tile(data = tc1, aes(n, PO, group = seq_along(n)), alpha = 0.4, fill = "darkgrey") +
      geom_line(aes(n, cluster2, group = Team_Short)) +
      geom_image(aes(n, cluster2, image = image, group = seq_along(n)), size = .1) +
      facet_wrap(Team_Short ~ ., ncol = 1, strip.position = "right") +
      scale_y_continuous(
        limits = c(0.5, 1.5),
        breaks = c(0.7, 0.9, 1.1, 1.3),
        labels = c("1", "2", "3", "4"),
        sec.axis = sec_axis(~ . * 1, breaks = 0)
      ) +
      scale_x_continuous(
        breaks = team_clust$n,
        labels = team_clust$ses
      ) +
      labs(
        title = "Line plot of teams clusters",
        subtitle = "with shadowed plyoffs apparances",
        x = "",
        y = ""
      ) +
      transition_reveal(n)

    animate(anim, width = 850, height = 500)
    anim_save("anim.gif")
  }

    # Atlantic
    ts <- c("TOR", "BOS", "PHI", "BKN", "NYK")
    anim(ts)

    # Central
    ts <- c("TOR", "IND", "CHI", "DET", "CLE")
    anim(ts)

    # Southeast
    ts <- c("MIA", "ORL", "WAS", "CHA", "ATL")
    anim(ts)

    # Western
    ts <- c("DEN", "UTA", "OKC", "POR", "MIN")
    anim(ts)

    # Pacific
    ts <- c("LAL", "LAC", "SAC", "PHX", "GSW")
    anim(ts)

    # Southwest
    ts <- c("HOU", "DAL", "MEM", "NOP", "SAs")
    anim(ts)
