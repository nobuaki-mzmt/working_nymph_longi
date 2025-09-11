
{
  library(data.table)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(dplyr)
  
  library(ggplot2)
  library(patchwork)
  library(RColorBrewer)
  library(ggridges)
  
  library(lme4)
  library(car)
}

# data organization
{
  df_wn <- fread("data_raw/working_nymph_data.csv")
  df_wn <- df_wn %>%
    mutate(
      time = (start_seq + end_seq)/2/30/60
    )
  
  df_flow <- fread("data_raw/flow.csv")
  df_flow$video_index <- match(df_flow$video, unique(df_flow$video))
  df_flow$time_stamp <- df_flow$video_index * 600
  
  df_flow <- df_flow %>%
    mutate(
      caste = case_when(
        str_detect(traffic, "w-") ~ "worker",
        str_detect(traffic, "min-s") ~ "minor_soldier",
        str_detect(traffic, "maj-s") ~ "major_soldier",
        str_detect(traffic, "nymph") ~ "nymph",
      ),
      direction = case_when(
        str_detect(traffic, "inbound") ~ "inbound",
        str_detect(traffic, "outbound") ~ "outbound",
        str_detect(traffic, "onbound") ~ "outbound",
      )
    )
  
  df_flow$item[df_flow$item == ""] <- "None"
  
  df_summary_item <- df_flow %>%
    group_by(caste, direction, item, time_stamp) %>%
    summarise(count = n(), .groups = "drop") %>%
    complete(caste, direction, item, time_stamp,
             fill = list(count = 0))
  
  df_summary_caste <- df_flow %>%
    group_by(caste, direction, time_stamp) %>%
    summarise(count = n(), .groups="drop") %>%
    complete(caste, direction, time_stamp,
             fill = list(count = 0))
}

# Traffic overview
{
  p1 <- ggplot(subset(df_summary_item, caste =="worker" & (item == "None" | direction == "outbound")), 
         aes(x = time_stamp/60, y = count, color = item, lty = direction, shape = direction)) +
    geom_line(alpha = .8) +
    geom_point(alpha = .8, fill = "white", size = .8) +
    scale_linetype_manual(values = c("inbound" = "dashed", "outbound" = "solid")) +
    scale_color_viridis_d(option = "C", end = .7) +
    scale_shape_manual(values = c("inbound" = 21, "outbound" = 19)) +
    scale_x_continuous(breaks = seq(0, 480, 120), limits = c(0, 480)) +
    scale_y_continuous(breaks = seq(0, 800, 400), limits = c(0, 800)) +
    labs(x = "Time (min)", y = "Traffic (/min)", fill = "State") +
    theme_classic( ) +
    theme(legend.position = "none",
          aspect.ratio = 3/4)+
    ggtitle("Worker")
  
  p2 <- ggplot(subset(df_summary_caste, (caste =="minor_soldier" | caste =="major_soldier") & 
                  direction == "outbound"), 
         aes(x = time_stamp/60, y = count, color = caste, lty = direction)) +
    geom_line(alpha = .8) +
    geom_point(alpha = .8, size = .8) +
    scale_color_viridis_d(option = "D", end = .8, direction = -1) +
    scale_x_continuous(breaks = seq(0, 480, 120), limits = c(0, 480)) +
    scale_y_continuous(breaks = seq(0, 400, 200), limits = c(0, 400)) +
    labs(x = "Time (min)", y = "Traffic (/min)", fill = "State") +
    theme_classic( ) +
    theme(legend.position = "none",
          aspect.ratio = 3/4)+
    ggtitle("Soldier")
  
  p3 <- ggplot(subset(df_summary_caste, (caste =="nymph") & 
                  direction == "outbound"), 
         aes(x = time_stamp/60, y = count)) +
    geom_line(alpha = .8, col = "#880808") +
    geom_point(alpha = .8, col = "#880808", size = .8) +
    geom_segment(data = df_wn, aes(x = time, y = 50, xend = time, yend = 40),
                 arrow = arrow(angle = 15, length = unit(0.1, "inches")), size = 0.2  ,
                 alpha = 0.75,
                 ) + 
    scale_x_continuous(breaks = seq(0, 480, 120), limits = c(0, 480)) +
    scale_y_continuous(breaks = seq(0, 80, 40), limits = c(0, 80)) +
    labs(x = "Time (min)", y = "Traffic (/min)", fill = "State") +
    theme_classic( ) +
    theme(legend.position = "none",
          aspect.ratio = 3/4) +
    ggtitle("Nymph")
  
  p_dynamics <- p1+p2+p3
  ggsave(p_dynamics, file = "output/dynamics.pdf", width = 8, height = 3.5,
         device = cairo_pdf, family = "Arial" )
}

# Traffic correlation
{
  df_flow2 <- df_flow[df_flow$direction == "outbound"]
  df_flow2$item[df_flow2$caste == "nymph"] <- "None"
  
  df_summary <- df_flow2 %>%
    group_by(caste, item, time_stamp) %>%
    summarise(count = n(), .groups = "drop")
  
  df_wide <- df_summary %>%
    unite("group", caste, item, sep="_") %>%
    tidyr::pivot_wider(
      names_from = group,
      values_from = count,
      values_fill = list(count = 0)
    )
  
  cor_mat <- cor(
    df_wide %>% select(-time_stamp),
    use = "pairwise.complete.obs",
    method = "spearman"
  )
  
  nice_names <- c("Major soldiers", "Minor soldiers", "Workers", 
                  "Workers (food)", "Workers (nestmate)", "Nymphs")
  
  name_map <- c(
    "major_soldier_None" = "Major soldiers",
    "minor_soldier_None" = "Minor soldiers",
    "worker_None"        = "Workers",
    "worker_with food"   = "Workers (food)",
    "worker_with brood"  = "Workers (nestmate)",
    "nymph_None"         = "Nymphs"
  )
  
  cor_df <- as.data.frame(cor_mat) %>%
    rownames_to_column("Var1") %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "correlation") %>%
    mutate(
      Var1 = factor(name_map[Var1], levels = rev(nice_names)),
      Var2 = factor(name_map[Var2], levels = nice_names)
    )
  
  cor.test(df_wide$nymph_None, df_wide$`worker_with brood`, method = "spearman")
  cor.test(df_wide$nymph_None, df_wide$`worker_with food`, method = "spearman")
  cor.test(df_wide$nymph_None, df_wide$`worker_None`, method = "spearman")
  cor.test(df_wide$nymph_None, df_wide$`major_soldier_None`, method = "spearman")
  
  ggplot(cor_df, aes(x = Var1, y = Var2, fill = correlation)) +
    geom_tile(color = "white") +
    scale_fill_distiller(palette = "RdBu", limit = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank(),
          aspect.ratio = 1)
  
  ggsave(file = "output/flow_correlation.pdf", width = 4, height = 4,
         device = cairo_pdf, family = "Arial" )
}

# proportion
{
  df_direction <- df_flow %>%
    group_by(direction) %>%
    summarise(count = n(), .groups="drop") %>%
    mutate(prop = count / sum(count))
  
  
  df_total <- df_flow %>%
    group_by(caste, direction, item) %>%
    summarise(count = n(), .groups="drop") %>%
    mutate(prop = count / sum(count))
  
  # estimation of traffic of each caste/items
  print(df_total * 10)
  
  
  df_caste_out <- subset(df_flow, direction == "outbound") %>%
    group_by(caste, item) %>%
    summarise(count = n(), .groups="drop") %>%
    mutate(prop = count / sum(count)) %>%
    complete(caste, item,
             fill = list(count = 0))
  
  df_caste_in <- subset(df_flow, direction == "inbound") %>%
    group_by(caste, item) %>%
    summarise(count = n(), .groups="drop") %>% 
    mutate(prop = count / sum(count)) %>%
    complete(caste, item,
             fill = list(count = 0))
  
  df_caste_total <- df_caste_out
  df_caste_total$count <- df_caste_out$count - df_caste_in$count
  
  df_caste_total <- df_caste_total %>%
    filter(count != 0) %>%
    mutate(prop = count / sum(count))
  
  # plot proportions
  df_caste_total$caste <- factor(df_caste_total$caste, 
                               levels = c("worker", "minor_soldier", "major_soldier", "nymph"))
  df_caste_total$fill_group <- interaction(df_caste_total$caste, df_caste_total$item, sep = "_")
  
  caste_colors <- viridis(4, option = "D")  # 4 distinct colors
  fill_colors <- c(
    "major_soldier_None"      = caste_colors[3],
    "minor_soldier_None"      = caste_colors[2],
    "nymph_None"              = caste_colors[4],
    "nymph_with brood"        = lighten(caste_colors[4], 0.4),
    "worker_None"             = caste_colors[1],
    "worker_with brood"       = lighten(caste_colors[1], 0.4),
    "worker_with food"        = darken(caste_colors[1], 0.9)
  )
  
  df_caste_total$fill_group <- factor(df_caste_total$fill_group,
                                    levels = c("worker_with food", "worker_with brood", "worker_None",
                                               "nymph_with brood", "nymph_None",
                                               "minor_soldier_None" ,"major_soldier_None"))
  
  ggplot(df_caste_total, aes(caste, y = prop, fill = fill_group)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = fill_colors) +
    labs(x = "", y = "Proportion of total traffic") +
    theme_classic() +
    scale_y_continuous(limits = c(0,1), breaks = c(0,.5,1), labels = c(0,0.5,1))+
    theme(legend.position = "none")
  ggsave("output/proportion.pdf", width = 4, height = 2.8,
         device = cairo_pdf, family = "Arial")
}


# nymph speed
{
  dataset <- ("data_fmt/df_nymph_speed.feather")
  df <- arrow::read_feather(dataset)
  df$unique_ind <- paste(df$video, df$ind, sep = "_")
  body_length <- tapply(df$body_length, df$unique_ind, mean, na.rm=T)
  
  df <- df[df$index %% 5 == 0,]
  
  df$step <- c(NA, sqrt(diff(df$x)^2 + diff(df$y)^2)) * 5
  df$step[df$index == 0] <- NA
  df$carrying <- df$ind == 0
  
  
  df <- df %>%
    left_join(
      tibble(unique_ind = names(body_length),
             body_length_ref = as.numeric(body_length)),
      by = "unique_ind"
    ) %>%
    mutate(step_scaled = step / body_length_ref)
  
  
  df_summary <- df %>%
    group_by(video, ind) %>%
    summarise(mean_step = mean(step_scaled, na.rm = TRUE), .groups = "drop")
  
  df_summary$carrying <- ifelse(df_summary$ind == 0, "Brood", "None")
  
  ggplot(df_summary, aes(x = carrying, y = mean_step)) +
    geom_boxplot(width = 0.5) +
    geom_point() + 
    theme_classic() +
    scale_y_continuous(limits = c(0, 5),
                       breaks = c(0, 5)) +
    labs(x = "", y = "Speed (Body length / sec)")
  ggsave("output/speed.pdf", width = 4, height = 3,
         device = cairo_pdf, family = "Arial" )
  
  
  r <- lmer(mean_step ~ carrying + (1|video), data=df_summary)
  Anova(r)
  t.test(mean_step ~ carrying, data=df_summary)
  
  r <- lmer(step ~ carrying + (1|video/unique_ind), data=df)
  Anova(r)
  
  
  ## path straghtness
  df_summary$unique_ind <- paste(df_summary$video, df_summary$ind, sep = "_")
  df_summary$straightness <- NA
  for(i_ind in unique(df$unique_ind)){
    df_temp <- subset(df, unique_ind == i_ind)
    df_temp <- na.omit(df_temp)
    straight_line <- as.numeric(sqrt((df_temp[dim(df_temp)[1], "x"] - df_temp[1, "x"])^2 + (df_temp[dim(df_temp)[1], "y"] - df_temp[1, "y"])^2))
    total_path <- sum(df_temp$step)
    df_summary[df_summary$unique_ind == i_ind,]$straightness <- straight_line/total_path
  }
  ggplot(df_summary, aes(x = carrying, y = straightness)) +
    geom_point()
  r <- lmer(straightness ~ carrying + (1|video), data=df_summary)
  Anova(r)
  
  ## angle
  df$head <- atan2(df$vec_y, df$vec_x)
  df$turn <- c(NA, diff(df$head))
  df$turn[df$index == 0] <- NA
  df$turn <- pmin(abs(df$turn), abs(2*pi - abs(df$turn)))
  
  df_summary <- df %>%
    group_by(video, ind) %>%
    summarise(mean_turn = mean(turn, na.rm = TRUE), .groups = "drop")
  df_summary$carrying <- ifelse(df_summary$ind == 0, "Brood", "None")
  
  
  r <- lmer(mean_turn ~ carrying + (1|video), data=df_summary)
  Anova(r)
  t.test(mean_turn ~ carrying, data=df_summary)
  
  r <- lmer(turn ~ carrying + (1|video/unique_ind), data=df)
  Anova(r)
  
  ggplot(df_summary, aes(x = carrying, y = mean_turn)) +
    geom_boxplot(width = 0.5) +
    geom_point() + 
    theme_classic() +
    scale_y_continuous(limits = c(0, .25),
                       breaks = c(0, .25)) +
    labs(x = "", y = "Turning angle (rad / sec)")
  ggsave("output/turn.pdf", width = 4, height = 3,
         device = cairo_pdf, family = "Arial" )
  
  
}

  