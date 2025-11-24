#packages needed
install.packages(c("tidyverse","readxl","worldfootballR","janitor","missMDa","ggplot2","ggrepl","gt","webshot2"))
library(worldfootballR)
library(readxl)
library(tidyverse)
library(janitor)
library(missMDA)
library(ggplot2)
library(ggrepel)
library(gt)
library(webshot2)

#data collection
season_end_year <- 2025

stat_types <- c("standard", "shooting", "passing", "passing_types", "passing_outcomes",
                "gca", "possession", "misc", "defense") 

list_stats <- map(stat_types, ~{
  tryCatch({
    df <- load_fb_big5_advanced_season_stats(season_end_year = season_end_year,
                                             stat_type = .x,
                                             team_or_player = "player")
    df$stat_type_source <- .x
    df
  }, error = function(e) {
    message("stat_type ", .x, " non dispo : ", e$message)
    NULL
  })
})


df_sample1 <- load_fb_big5_advanced_season_stats(season_end_year = season_end_year,
                                                 stat_type = "possession",
                                                 team_or_player = "player")

df_sample2 <- load_fb_big5_advanced_season_stats(season_end_year = season_end_year,
                                                 stat_type = "standard",
                                                 team_or_player = "player")
df_sample3 <- load_fb_big5_advanced_season_stats(season_end_year = season_end_year,
                                                 stat_type = "shooting",
                                                 team_or_player = "player")
df_sample4 <- load_fb_big5_advanced_season_stats(season_end_year = season_end_year,
                                                 stat_type = "passing",
                                                 team_or_player = "player")
df_sample5 <- load_fb_big5_advanced_season_stats(season_end_year = season_end_year,
                                                 stat_type = "passing_types",
                                                 team_or_player = "player")
df_sample6 <- load_fb_big5_advanced_season_stats(season_end_year = season_end_year,
                                                 stat_type = "misc",
                                                 team_or_player = "player")
df_sample7 <- load_fb_big5_advanced_season_stats(season_end_year = season_end_year,
                                                 stat_type = "defense",
                                                 team_or_player = "player")
df_sample8 <- read_excel("~/M2/Recherche Stage M2/Scouting dashboard/bdd_scaclean2.xlsx")

df_sample9 <- read_excel("~/M2/Recherche Stage M2/Scouting dashboard/indices finaux/CPA.xlsx")

df_merged <- Reduce(function(x, y) merge(x, y, by = "Player", all = TRUE),
                    list(df_sample1, df_sample2, df_sample3, df_sample4,
                         df_sample5, df_sample6, df_sample7, df_sample8, df_sample9))

#data cleaning
df_merged <- df_merged[, !duplicated(names(df_merged))]
key <- paste(df_merged$Squad.x, df_merged$Player, sep = "|")
df_merged <- df_merged[!duplicated(key), ]

colnames(df_merged)

#variables creation
df_merged$prcarries90 <- df_merged$PrgC_Progression / (df_merged$Min_Playing / 90)
df_merged$prpasses90 <- df_merged$PrgP_Progression / (df_merged$Min_Playing / 90) 
df_merged$prrecep90 <- df_merged$PrgR_Progression / (df_merged$Min_Playing / 90)
df_merged$kp90 <- df_merged$KP / (df_merged$Min_Playing / 90)
df_merged$Passes_ratees <- df_merged$Att_Total - df_merged$Cmp_Total
df_merged$passesratees90 <- df_merged$Passes_ratees / (df_merged$Min_Playing / 90)
df_merged$carriesratees <- df_merged$Dis_Carries + df_merged$Mis_Carries          
df_merged$carriesratees90 <- df_merged$carriesratees / (df_merged$Min_Playing / 90) 
df_merged$Ballons_perdus <- df_merged$Dis_Carries + df_merged$Mis_Carries + df_merged$Passes_ratees
df_merged$pertes90 <- df_merged$Ballons_perdus / (df_merged$Min_Playing / 90)
df_merged$passesfinalthird90 <- df_merged$Final_Third / (df_merged$Min_Playing / 90)
df_merged$crspa90 <- df_merged$CrsPA / (df_merged$Min_Playing / 90) 
df_merged$tbpass90 <- df_merged$TB_Pass / (df_merged$Min_Playing / 90) 
df_merged$cpa90 <- df_merged$cpa / (df_merged$Min_Playing / 90) 
df_merged$xa90 <- df_merged$xA_Expected / (df_merged$Min_Playing / 90) 
df_merged$scato90 <- df_merged$scato / (df_merged$Min_Playing / 90)
df_merged$db90 <- df_merged$Succ / (df_merged$Min_Playing / 90)
df_merged$prgdist90 <- df_merged$PrgDist_Total / (df_merged$Min_Playing / 90)

df1 <- df_merged[, c("Player","Squad.x","Comp.x","Nation.x","Pos.x","Born.x","Age.x","MP_Playing","Min_Playing","prpasses90","prcarries90","prrecep90","cpa90","passesfinalthird90","crspa90","tbpass90","pertes90","kp90","xa90","passesratees90","carriesratees90","sca","sca90","scato","scato90","db90")]

df3 <- df1 %>%  
  mutate(Min_Playing = as.numeric(gsub(",", ".", as.character(Min_Playing)))) %>%
  filter(!is.na(Min_Playing) & Min_Playing >= 1000)

na_count <- sum(is.na(df3$pertes90))
print(na_count)#74 only variable with missing datas, 5% of the df (and not best players) -> can be removed
unique(as.character(df3[is.na(df3$pertes90), 1]))
df4 <- df3 %>% 
  filter(!is.na(pertes90))

#graph
top_pass  <- df4 %>% filter(!is.na(prpasses90))  %>% arrange(desc(prpasses90))  %>% slice_head(n = 30) %>% pull(Player)
top_carry <- df4 %>% filter(!is.na(prcarries90)) %>% arrange(desc(prcarries90)) %>% slice_head(n = 30) %>% pull(Player)

plot_df <- df4 %>%
  filter(Player %in% union(top_pass, top_carry)) %>%
  mutate(group = case_when(
    Player %in% top_pass & Player %in% top_carry ~ "both",
    Player %in% top_pass ~ "top_pass",
    TRUE ~ "top_carry"
  ))

ggplot(plot_df, aes(x = prcarries90, y = prpasses90, color = group)) +
  geom_point(size = 2, alpha = 0.9) +
  geom_text_repel(aes(label = Player),
                  size = 3,             
                  max.overlaps = 200,
                  box.padding = 0.25,
                  point.padding = 0.3,
                  segment.size = 0.3) +
  labs(x = "Progressive carries /90 (prcarries90)",
       y = "Progressive passes /90 (prpasses90)",
       title = "Top30 prpasses90 & Top30 prcarries90") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

#checking for correlation
mat <- df4 %>% select(all_of(vars_use)) %>% mutate(across(everything(), as.numeric))
cor_mat <- cor(mat, use = "pairwise.complete.obs")
print(round(cor_mat, 2))

high_cor <- which(abs(cor_mat) > 0.85 & abs(cor_mat) < 1, arr.ind = TRUE)
if (nrow(high_cor) > 0) {
  cat("Paires très corrélées (>0.85) :\n")
  apply(high_cor, 1, function(i) {
    cat(rownames(cor_mat)[i[1]], " <-> ", colnames(cor_mat)[i[2]], 
        " = ", round(cor_mat[i[1], i[2]], 2), "\n")
  })
} else cat("Aucune paire > 0.85 détectée.\n")

#pass index
df5 <- df4
pass_vars <- c("prpasses90","passesfinalthird90","crspa90","tbpass90")
xa_var    <- "xa90"
miss_var  <- "passesratees90"

mod_df <- df5 %>%
  select(all_of(c(xa_var, pass_vars))) %>%
  na.omit()

X <- scale(mod_df[, pass_vars])
y <- mod_df[[xa_var]]

mod <- lm(y ~ X)

coefs <- coef(mod)[-1]
names(coefs) <- pass_vars

if (all(is.na(coefs))) {
  weights_quantity <- rep(1 / length(pass_vars), length(pass_vars))
  names(weights_quantity) <- pass_vars
} else {
  coefs_abs <- abs(coefs)
  if (sum(coefs_abs, na.rm = TRUE) == 0) {
    weights_quantity <- rep(1 / length(pass_vars), length(pass_vars))
    names(weights_quantity) <- pass_vars
  } else {
    weights_quantity <- coefs_abs / sum(coefs_abs, na.rm = TRUE)
  }
}

barplot(
  heights <- weights_quantity,
  names.arg = names(weights_quantity),
  main = "Weights for PassQuantity (learned from xa90)",
  ylab = "Weight",
  las = 2
)

df5 <- df5 %>%
  mutate(
    PassQuantity_raw = as.numeric(as.matrix(select(., all_of(pass_vars))) %*% weights_quantity)
  )

df5 <- df5 %>%
  mutate(Pos = sapply(strsplit(as.character(Pos.x), "[, /-]"), `[`, 1)) %>%
  group_by(Pos) %>%
  mutate(
    PassQuantity_pct = percent_rank(PassQuantity_raw) * 100,
    PassQuality_pct  = percent_rank(.data[[xa_var]]) * 100,
    PassAccuracy_pct = (1 - percent_rank(.data[[miss_var]])) * 100
  ) %>%
  ungroup()
xa_var   <- "xa90"
miss_var <- "passesratees90"

mod_q_diag <- lm(as.formula(paste(xa_var, "~ PassQuantity_raw")), data = df5)
df5$xa_resid_diag <- resid(mod_q_diag)

df5_diag <- df5 %>%
  group_by(Pos) %>%
  mutate(
    PassQuantity_pct_diag      = percent_rank(PassQuantity_raw) * 100,
    PassQuality_resid_pct_diag = percent_rank(xa_resid_diag) * 100,
    PassAccuracy_pct_diag      = (1 - percent_rank(.data[[miss_var]])) * 100
  ) %>%
  ungroup()

mod_w_diag <- df5_diag %>%
  select(all_of(c(xa_var,
                  "PassQuantity_pct_diag",
                  "PassQuality_resid_pct_diag",
                  "PassAccuracy_pct_diag"))) %>%
  na.omit()

Z_diag <- scale(mod_w_diag[, c("PassQuantity_pct_diag",
                               "PassQuality_resid_pct_diag",
                               "PassAccuracy_pct_diag")])
y_diag <- mod_w_diag[[xa_var]]

mod_index_diag <- lm(y_diag ~ Z_diag)

beta_diag <- coef(mod_index_diag)[-1]
names(beta_diag) <- c("quantity_resid", "quality_resid", "accuracy_resid")

beta_diag_abs <- abs(beta_diag)
weights_index_diag_resid <- beta_diag_abs / sum(beta_diag_abs)

print(beta_diag)
print(weights_index_diag_resid)

weights_index_final <- c(quantity = 0.4, quality = 0.5, accuracy = 0.1)

df5 <- df5 %>%
  group_by(Pos) %>%
  mutate(
    PassIndex_weighted_raw =
      weights_index_final["quantity"] * PassQuantity_pct +
      weights_index_final["quality"]  * PassQuality_pct +
      weights_index_final["accuracy"] * PassAccuracy_pct,
    PassIndex_weighted_pct = round(percent_rank(PassIndex_weighted_raw) * 100, 2)
  ) %>%
  ungroup()

export <- df5 %>%
  select(Player, Squad.x, Pos.x, Pos, Comp.x, Min_Playing, PassQuantity_raw, xa90, xa_resid_diag,
         PassQuantity_pct, PassQuality_pct, PassAccuracy_pct, PassIndex_weighted_raw,
         PassIndex_weighted_pct) %>%
  arrange(desc(PassIndex_weighted_pct))

write.csv2(export, "PassIndex.csv",
           row.names = FALSE, fileEncoding = "UTF-8")

top5_passindex <- df5 %>%
  mutate(Pos = factor(Pos, levels = c("GK", "DF", "MF", "FW"))) %>%  
  group_by(Pos) %>%
  slice_max(PassIndex_weighted_pct, n = 5, with_ties = FALSE) %>%
  arrange(Pos, desc(PassIndex_weighted_pct)) %>%
  ungroup() %>%
  select(
    Pos,
    Player,
    Squad = Squad.x,
    PassIndex = PassIndex_weighted_pct
  )
top5_passindex %>%
  gt() %>%
  tab_header(
    title = "Top 5 players by PassIndex for each position group"
  )
top5_passindex %>%
  gt() %>%
  tab_header(
    title = "Top 5 players by PassIndex for each position group"
  )

gt_tbl <- top5_passindex %>%
  gt() %>%
  tab_header(
    title = "Top 5 players by PassIndex for each position group"
  )
dir.create("C:/Users/liloy/Documents/M2/Recherche Stage M2/Scouting dashboard", showWarnings = FALSE)
gtsave(gt_tbl, filename = "C:/Users/liloy/Documents/M2/Recherche Stage M2/Scouting dashboard/top5_passindex.png")
