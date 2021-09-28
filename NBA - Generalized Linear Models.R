################################################################################
# Title:  NBA - generalized linear models
# Date:   2020-22-04
# Technologies:
#     * R
#     * Html (Kaggle only)
#     * CSS  (Kaggle only)
# Libraries:
#   * caret
#   * tidyverse
#   * zoo
#   * tictoc
#   * ggthemes
#   * knitr
#   * kableExtra
#   * pracma
# Tasks:
#   * Data preparation
#   * Preprocessing
#   * PCA
#   * Modeling
# Exteral link:
#   * https://www.kaggle.com/radbys/nba-generalized-linear-models
################################################################################

# Environment setup
  # Data import
  main <- read.csv("../input/mianmian//main.csv")
  main <- main[, -1]
  clust <- read.csv("../input//mianmian//clusters.csv")

  # Libraries
  library(caret)
  library(tidyverse)
  library(zoo)
  library(tictoc)
  library(ggthemes)
  library(knitr)
  library(kableExtra)
  library(pracma)

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

  # User-made functions
  movavg <- function(x, l = "no", n = 2, method = "s") {

    # Pull vector
    if (class(x)[1] == "integer" | class(x)[1] == "numeric") {
      x
    } else if (dim(x)[2] > 1) {
      warning("Choose a column")
      stop()
    } else {
      x <- x %>% pull()
    }

    # If lagged
    if (l == "no" | l == 0) {
      x
    } else {
      x <- lag(x, l)
    }

    # n
    if (n == 0) {
      warning(`Choose n > 0`)
      stop()
    } else if (n == 1) {
      x
    } else {
      if (method == "s") {
        x <- pracma::movavg(x = x, n = n, type = method)
      } else if (method == "e") {
        y <- pracma::movavg(x = na.omit(x), n = n, type = method)
        x <- c(rep(NA, sum(is.na(x))), round(y, 2))
      } else {
        warning("Choose a proper method")
        stop()
      }
    }

    return(x)
  }


# Data preparation
  # Adding clusters
  main <- left_join(
    main,
    clust %>%
      select(3:5),
    by = c("ses", "x1" = "x")
  )

  main <- left_join(
    main,
    clust %>%
      select(3:5),
    by = c("ses", "x2" = "x"),
    suffix = c("_x1", "_x2")
  )

  main <- main %>%
    select(
      1:6,
      "x1_clust" = 88,
      "x2_clust" = 89,
      everything()
    )

  # Predictors counting
    # Current
    a <- main %>%
      dplyr::select(1:3, 5, 7, 25, 27:28, 30:31, 21, 9:10, 12:13, 15, 17, 19:20, 23)

    b <- main %>%
      dplyr::select(1:2, 4, 6, 8, 26:27, 29:31, 22, 9, 11:12, 14, 16, 18:20, 24)

    names(a) <- gsub("x1_", "", names(a))
    names(a) <- gsub("x1", "x", names(a))
    names(b) <- names(a)

    pred1 <- bind_rows(a, b) %>%
      arrange(Game_ID)

    kable(head(na.omit(pred1))) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


    # Not lagged

    a <- main %>%
      dplyr::select(1:3, 25, 27)

    b <- main %>%
      dplyr::select(1:2, 4, 26, 27)

    names(a) <- gsub("x1_", "", names(a))
    names(a) <- gsub("x1", "x", names(a))
    names(b) <- names(a)

    pred2 <- bind_rows(a, b) %>%
      arrange(Game_ID)

    pred2 <- bind_cols(
      pred2[, 1:2],
      pred2 %>%
        dplyr::select(3:5) %>%
        group_by(x) %>%
        mutate_each(~ movavg(x = ., l = "no", n = 10, method = "e")) %>%
        ungroup()
    )

    names(pred2)[4:5] <- paste(names(pred2)[4:5], "_L10", sep = "")


    kable(head(na.omit(pred2))) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


    # Lagged
    a <- main %>%
      dplyr::select(1:3, 28, 30:31, seq(32, 88, 2))

    b <- main %>%
      dplyr::select(1:2, 4, 29:31, seq(33, 89, 2))

    names(a)[25] <- "x1_Plus_Min"
    names(b)[25] <- "x2_Plus_Min"

    names(a) <- gsub("x1_", "", names(a))
    names(a) <- gsub("x1", "x", names(a))
    names(b) <- names(a)

    pred3 <- bind_rows(a, b) %>%
      arrange(Game_ID)

    pred3 <- bind_cols(
      pred3[, 1:2],
      pred3 %>%
        dplyr::select(3:35) %>%
        group_by(x) %>%
        mutate_each(~ movavg(x = ., l = 1, n = 10, method = "e")) %>%
        ungroup()
    )

    names(pred3)[4:35] <- paste(names(pred3)[4:35], "_L10", sep = "")

    kable(head(na.omit(pred3))) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
      scroll_box(width = "100%")


# Predictors extracting
  # Sep predictors
  sep <- inner_join(
    pred1,
    pred2,
    by = c("Game_ID", "Date", "x")
  )

  sep <- inner_join(
    sep,
    pred3,
    by = c("Game_ID", "Date", "x")
  )

  # Tog predictors
  x1 <- sep
  x2 <- sep

  tog <- left_join(
    main[, 1:4],
    x1,
    by = c("Game_ID", "Date", "x1" = "x")
  )

  tog <- left_join(
    tog,
    x2,
    by = c("Game_ID", "Date", "x2" = "x"),
    suffix = c("_x1", "_x2")
  )

  # Ordering tog veriables
  vec <- c()

  for (i in 5:55) {
    vec[(length(vec) + 1)] <- i
    vec[(length(vec) + 1)] <- (i + 51)
  }


  tog <- tog %>%
    dplyr::select(1:4, vec) %>%
    rename(
      Line = Line_x1,
      Sum_P = Sum_P_x1,
      O_U = O_U_x1,
      W = W_x1,
      ses = ses_x1,
      PO = PO_x1,
      conf = conf_x1,
      OT = OT_x1
    ) %>%
    dplyr::select(
      -Line_x2,
      -Sum_P_x2,
      -O_U_x2,
      -W_x2,
      -ses_x2,
      -PO_x2,
      -conf_x2,
      -OT_x2
    )

    # Sep predictors
    pred_sep <- sep[, c(3, 6:7, 13:54)] %>% na.omit()
    kable(na.omit(head(pred_sep))) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
      scroll_box(width = "100%")

    # Tog predictors
    pred_tog <- tog[, c(9:11, 18:98)] %>% na.omit()
    kable(na.omit(head(pred_tog))) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
      scroll_box(width = "100%")


# Preprocess
  # One hot encoding
  dummy <- pred_tog %>%
    transmute(
      n = row_number(),
      conf = conf,
      CO_x1 = CO_x1,
      CO_x2 = CO_x2
    )

  dummies <- dummyVars(n ~ ., data = dummy)

  dummies <- predict(dummies, newdata = dummy) %>%
    as.data.frame()

  pred_tog <- bind_cols(
    pred_tog[, 1:10],
    dummies,
    pred_tog[, 14:84]
  )

  # Collinearity
  findLinearCombos(pred_tog)
  pred_tog <- pred_tog[, -findLinearCombos(pred_tog)$remove]

  # Data spliting
  Index <- createDataPartition(
    pred_tog$Od_x1,
    p = .8,
    list = FALSE,
    times = 1
  )

  togTrain <- bind_cols(
    tog %>%
      na.omit() %>%
      select(Sum_P, Line, O_U, W),
    pred_tog
  )[Index, ]

  togTest <- bind_cols(
    tog %>%
      na.omit() %>%
      select(Sum_P, Line, O_U, W),
    pred_tog
  )[-Index, ]

  # PCA
  preProcValues <- preProcess(togTrain[, -1:-4], method = c("nzv", "scale", "pca"))
  togTrainPre <- predict(preProcValues, togTrain)
  togTestPre <- predict(preProcValues, togTest)


# Modeling
  # Predicting O/U with regression
    # How good are the bookies
      main %>%
        select(Sum_P, Line) %>%
        transmute(n = sqrt((Sum_P - Line)^2)) %>%
        summarise(RMSE = mean(n))

    # Does PCA help?
      # Controls
      controls <- trainControl(
        index = createFolds(togTrain$Sum_P, k = 10)
      )

      # full data
      tic()
      tog_sum_lm <- train(
        Sum_P ~ .,
        togTrain[, -2:-4],
        method = "lm",
        trControl = controls,
        preProcess = "scale"
      )
      cat("Model without PCA: ")
      toc()
      cat("\n")

      # preprocessed data
      tic()
      tog_sum_lm_pca <- train(
        Sum_P ~ .,
        togTrainPre[, -2:-4],
        method = "lm",
        trControl = controls
      )
      cat("Model with PCA: ")
      toc()
      cat("\n")

      summary(resamples(list(
        Full = tog_sum_lm,
        PCA = tog_sum_lm_pca
      )))

  # Building a model
    # stepwise
    tic()
    tog_sum_lm_step_pca <- train(
      Sum_P ~ .,
      togTrainPre[-2:-4],
      method = "lmStepAIC",
      trControl = controls,
      trace = FALSE
    )
    toc()

    summary(resamples(list(
      "Full model with PCA" = tog_sum_lm_pca,
      "Stepwise model with PCA" = tog_sum_lm_step_pca
    )))

  # Implementing the threshold
  thres <- 3

  togTrainPre %>%
    select(Sum_P, Line, O_U) %>%
    mutate(pred = round(predict.train(tog_sum_lm_step_pca), 1)) %>%
    mutate(bet = ifelse(pred > Line, 1, 0)) %>%
    mutate(res = ifelse(bet == O_U, 1, 0)) %>%
    mutate(resT = ifelse(abs(pred - Line) > thres, res, NA)) %>%
    summarise(
      res_eff = mean(res),
      res_bts = sum(res >= 0),
      resT_eff = mean(resT, na.rm = T),
      resT_bts = sum(resT >= 0, na.rm = T)
    ) %>%
    round(., 2)

  # Threshold vs Effectiveness
    # creating df
    chart_lm <- as.data.frame(matrix(ncol = 5, nrow = 0))
    names(chart_lm) <- c("eff", "bts", "prc", "thres", "set")

    for (i in 0:30) {
      chart_lm <- bind_rows(
        chart_lm,
        togTrainPre %>%
          select(Sum_P, Line, O_U) %>%
          mutate(pred = round(predict.train(tog_sum_lm_step_pca), 1)) %>%
          mutate(bet = ifelse(pred > Line, 1, 0)) %>%
          mutate(res = ifelse(bet == O_U, 1, 0)) %>%
          mutate(resT = ifelse(abs(pred - Line) >= i, res, NA)) %>%
          summarise(
            eff = mean(resT, na.rm = T),
            bts = sum(resT >= 0, na.rm = T)
          ) %>%
          round(., 2) %>%
          mutate(
            prc = round(bts / nrow(togTrainPre), 2),
            thres = i,
            set = "train"
          ),
        togTestPre %>%
          select(Sum_P, Line, O_U) %>%
          mutate(pred = round(predict(tog_sum_lm_step_pca, newdata = togTestPre), 1)) %>%
          mutate(bet = ifelse(pred > Line, 1, 0)) %>%
          mutate(res = ifelse(bet == O_U, 1, 0)) %>%
          mutate(resT = ifelse(abs(pred - Line) >= i, res, NA)) %>%
          summarise(
            eff = mean(resT, na.rm = T),
            bts = sum(resT >= 0, na.rm = T)
          ) %>%
          round(., 2) %>%
          mutate(
            prc = round(bts / nrow(togTestPre), 2),
            thres = i,
            set = "test"
          )
      )
    }

    # ploting effectiveness and naumber of bets vs treshold value
    chart_lm %>%
      select(eff, bts, thres, set) %>%
      gather(value, key, -thres, -set) %>%
      mutate(set = factor(set, levels = c("train", "test"))) %>%
      ggplot(aes(thres, key)) +
      geom_smooth(se = F, span = 0.2, col = colors[2]) +
      facet_wrap(value ~ set,
        scales = "free",
        labeller = "label_both"
      ) +
      labs(
        title = "Effectiveness and naumber of bets",
        subtitle = "according to treshold value",
        x = "",
        y = ""
      )


  # Profitability vs treshold
  odds <- 1.9

  chart_lm %>%
    mutate(
      set = factor(set, levels = c("train", "test")),
      profit = round((bts * eff) * 0.9 - (1 - eff) * bts, 2)
    ) %>%
    group_by(set) %>%
    mutate(max = max(profit)) %>%
    ggplot(aes(thres, profit)) +
    facet_wrap(~set, scales = "free") +
    geom_text(aes(label = ifelse(profit == max, thres, " ")), nudge_x = 2, size = 8, col = colors[2]) +
    geom_smooth(se = F, span = 0.1, col = colors[2]) +
    facet_wrap(~set, scales = "free") +
    labs(
      title = "Profitability vs treshold value",
      subtitle = "grouped by set",
      x = "",
      y = ""
    )

  # Bankroll analysis
    # assumptions
    budget <- 1000
    stake <- 1

    bkr_lm <- left_join(
      togTestPre %>%
        select(Line, O_U) %>%
        mutate(
          pred = round(predict(tog_sum_lm_step_pca, newdata = togTestPre), 1),
          mrg = floor(abs(pred - Line))
        ),
      chart_lm %>%
        filter(set == "test") %>%
        select(thres, eff),
      by = c("mrg" = "thres")
    )

    bkr_lm <- bkr_lm %>%
      mutate(eff = ifelse(is.na(eff), 1, eff)) %>%
      mutate(
        adv = eff - 0.5,
        scr = ifelse(
          (pred > Line & O_U == 1) | (pred < Line & O_U == 0),
          1,
          0
        )
      )

    # Implementing threshold value
    bkr_lm <- bkr_lm %>%
      filter(mrg >= 3)


    # creating df
    pay_lm <- data.frame(
      n = 0,
      flat = budget,
      kelly = budget,
      flat_stake = NA,
      kelly_stake = NA
    )

    for (i in 1:nrow(bkr_lm)) {
      # flat
      flat_stake <- stake

      if (bkr_lm[i, 7] == 1) {
        flat <- round(pay_lm[i, 2] + (odds - 1) * flat_stake, 2)
      } else {
        flat <- round(pay_lm[i, 2] - flat_stake, 2)
      }


      # kelly
      kelly_stake <- round((pay_lm[i, 3] * bkr_lm[i, 6]) / 200, 2)
      kelly_stake <- if (kelly_stake < 1) {
        1
      } else {
        kelly_stake
      }

      if (bkr_lm[i, 7] == 1) {
        kelly <- round(pay_lm[i, 3] + ((odds - 1) * kelly_stake), 2)
      } else {
        kelly <- round(pay_lm[i, 3] - kelly_stake, 2)
      }

      # binding
      pay_lm <- bind_rows(
        pay_lm,
        data.frame(
          n = i,
          flat,
          kelly,
          flat_stake,
          kelly_stake
        )
      )
    }

    ggplot(pay_lm, aes(n)) +
      geom_line(aes(y = flat), col = colors[2]) +
      geom_line(aes(y = kelly), col = colors[3]) +
      annotate("text",
        x = 1300, y = 1100,
        label = paste("Flat yield: ", round((pay_lm[nrow(pay_lm), 2] - pay_lm[1, 2]) / sum(pay_lm[, 4], na.rm = T), 2), sep = "\n"),
        col = colors[2],
        size = 8
      ) +
      annotate("text",
        x = 1100, y = 1400,
        label = paste("Kelly yield: ", round((pay_lm[nrow(pay_lm), 3] - pay_lm[1, 3]) / sum(pay_lm[, 5], na.rm = T), 2), sep = "\n"),
        col = colors[3],
        size = 8
      ) +
      labs(
        title = "Bankroll time series ",
        subtitle = "grouped by bez sizing strategy",
        x = "",
        y = ""
      )

# Predicting O/U with classification
  # Building a model
    # full data
    tic()
    tog_ou_glm_pca <- train(
      factor(O_U) ~ .,
      togTrainPre[, c(-1:-2, -4)],
      method = "glmStepAIC",
      family = "binomial",
      trControl = controls,
      trace = FALSE
    )
    toc()
    summary(tog_ou_glm_pca)

  # Threshold manipulation
  thres <- 0.05

  log_Train <- bind_cols(
    togTrainPre %>%
      select(Sum_P, Line, O_U),
    round(predict.train(tog_ou_glm_pca, type = "prob"), 2) %>%
      select(Under = 1, Over = 2)
  )

  log_Train %>%
    mutate(
      bet = ifelse(Under > Over, 0, 1),
      pred = ifelse(Under > Over, Under, Over),
      res = ifelse(bet == O_U, 1, 0)
    ) %>%
    mutate(resT = ifelse((pred - 0.5) > thres, res, NA)) %>%
    select(-4:-5) %>%
    summarise(
      res_eff = mean(res),
      res_bts = sum(res >= 0),
      resT_eff = mean(resT, na.rm = T),
      resT_bts = sum(resT >= 0, na.rm = T)
    ) %>%
    round(., 2)

  # Threshold vs Effectivenss
    # creating df
    chart_log <- as.data.frame(matrix(ncol = 5, nrow = 0))
    names(chart_log) <- c("eff", "bts", "prc", "thres", "set")


    log_Test <- bind_cols(
      togTestPre %>%
        select(Sum_P, Line, O_U),
      round(predict(tog_ou_glm_pca, newdata = togTestPre, type = "prob"), 2) %>%
        select(Under = 1, Over = 2)
    )


    for (i in seq(0, 0.45, by = 0.01)) {
      chart_log <- bind_rows(
        chart_log,
        log_Train %>%
          mutate(
            bet = ifelse(Under > Over, 0, 1),
            pred = ifelse(Under > Over, Under, Over),
            res = ifelse(bet == O_U, 1, 0)
          ) %>%
          mutate(resT = ifelse((pred - 0.5) >= i, res, NA)) %>%
          select(-4:-5) %>%
          summarise(
            eff = mean(resT, na.rm = T),
            bts = sum(resT >= 0, na.rm = T)
          ) %>%
          round(., 2) %>%
          mutate(
            prc = round(bts / nrow(togTrainPre), 2),
            thres = i,
            set = "train"
          ),
        log_Test %>%
          mutate(
            bet = ifelse(Under > Over, 0, 1),
            pred = ifelse(Under > Over, Under, Over),
            res = ifelse(bet == O_U, 1, 0)
          ) %>%
          mutate(resT = ifelse((pred - 0.5) >= i, res, NA)) %>%
          select(-4:-5) %>%
          summarise(
            eff = mean(resT, na.rm = T),
            bts = sum(resT >= 0, na.rm = T)
          ) %>%
          round(., 2) %>%
          mutate(
            prc = round(bts / nrow(togTrainPre), 2),
            thres = i,
            set = "test"
          )
      )
    }

    # ploting effectiveness and naumber of bets vs treshold value
    chart_log %>%
      select(eff, bts, thres, set) %>%
      gather(value, key, -thres, -set) %>%
      mutate(set = factor(set, levels = c("train", "test"))) %>%
      ggplot(aes(thres, key)) +
      geom_smooth(se = F, span = 0.2, col = colors[2]) +
      facet_wrap(value ~ set,
        scales = "free",
        labeller = "label_both"
      ) +
      labs(
        title = "Effectiveness and naumber of bets",
        subtitle = "according to treshold value",
        x = "",
        y = ""
      )


  # Profitability vs treshold
  odds <- 1.9

  chart_log %>%
    mutate(
      set = factor(set, levels = c("train", "test")),
      profit = round((bts * eff) * 0.9 - (1 - eff) * bts, 2)
    ) %>%
    group_by(set) %>%
    mutate(max = max(profit)) %>%
    ggplot(aes(thres, profit)) +
    facet_wrap(~set, scales = "free") +
    geom_text(aes(label = ifelse(profit == max, thres, " ")), nudge_x = 0.01, size = 8) +
    geom_smooth(se = F, span = 0.2, col = colors[2]) +
    facet_wrap(~set, scales = "free") +
    labs(
      title = "Profitability vs treshold value",
      subtitle = "grouped by set",
      x = "",
      y = ""
    )

  # Bankroll analysis
  bkr_log <- left_join(
    log_Test %>%
      mutate(
        bet = ifelse(Under > Over, 0, 1),
        pred = ifelse(Under > Over, Under, Over),
        res = ifelse(bet == O_U, 1, 0)
      ) %>%
      select(Line, O_U, pred, scr = res) %>%
      mutate(mrg = round(pred - 0.5, 2)),
    chart_log %>%
      filter(set == "test") %>%
      select(thres, eff),
    by = c("mrg" = "thres")
  )


  bkr_log <- bkr_log %>%
    mutate(eff = ifelse(is.na(eff), 1, eff)) %>%
    mutate(adv = eff - 0.5)

  # Implementing threshold
  bkr_log <- bkr_log %>%
    filter(mrg >= 0.02)

  # creating df
  pay_log <- data.frame(
    n = 0,
    flat = budget,
    kelly = budget,
    flat_stake = NA,
    kelly_stake = NA
  )

  for (i in 1:nrow(bkr_log)) {
    # flat
    flat_stake <- stake

    if (bkr_log[i, 4] == 1) {
      flat <- round(pay_log[i, 2] + (odds - 1) * flat_stake, 2)
    } else {
      flat <- round(pay_log[i, 2] - flat_stake, 2)
    }


    # kelly
    kelly_stake <- round((pay_log[i, 3] * bkr_log[i, 7]) / 200, 2)
    kelly_stake <- if (kelly_stake < 1) {
      1
    } else {
      kelly_stake
    }

    if (bkr_log[i, 4] == 1) {
      kelly <- round(pay_log[i, 3] + ((odds - 1) * kelly_stake), 2)
    } else {
      kelly <- round(pay_log[i, 3] - kelly_stake, 2)
    }

    # binding
    pay_log <- bind_rows(
      pay_log,
      data.frame(
        n = i,
        flat,
        kelly,
        flat_stake,
        kelly_stake
      )
    )
  }

  ggplot(pay_log, aes(n)) +
    geom_line(aes(y = flat), col = colors[2]) +
    geom_line(aes(y = kelly), col = colors[3]) +
    annotate("text",
      x = 2000, y = 1010,
      label = paste("Flat yield: ", round((pay_log[nrow(pay_log), 2] - pay_log[1, 2]) / sum(pay_log[, 4], na.rm = T), 2), sep = "\n"),
      col = colors[2],
      size = 8
    ) +
    annotate("text",
      x = 1800, y = 1200,
      label = paste("Kelly yield: ", round((pay_log[nrow(pay_log), 3] - pay_log[1, 3]) / sum(pay_log[, 5], na.rm = T), 2), sep = "\n"),
      col = colors[3],
      size = 8
    ) +
    labs(
      title = "Bankroll time series ",
      subtitle = "grouped by bez sizing strategy",
      x = "",
      y = ""
    )

# Comparison and Sum up
comp <- inner_join(
  pay_lm,
  pay_log,
  by = "n",
  suffix = c("_lm ", "_log")
)

comp %>%
  select(1, 2:3, 6:7) %>%
  gather(key, value, -n) %>%
  mutate(type = substr(key, nchar(key) - 2, nchar(key))) %>%
  ggplot(aes(n, value, group = key, col = key)) +
  geom_line() +
  facet_grid(~type) +
  scale_color_manual(values = c(colors[2], colors[2], colors[3], colors[3])) +
  labs(
    title = "Models profits comparision",
    subtitle = "grouped by medelingmodel type",
    x = "",
    y = ""
  )
