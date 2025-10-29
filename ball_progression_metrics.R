install.packages(c("tidyverse","worldfootballR","janitor","missMDa","ggplot2","ggrepl"))
library(worldfootballR)
library(tidyverse)
library(janitor)
library(missMDA)
library(ggplot2)
library(ggrepel)
# exemple : récupérer pour une saison & une ligue (ici Big-5 via helper) -> sélectionner des joueurs que je vois jouer régulièrement pour voir si les profils que mes indices font ressortir sont cohérents
season_end_year <- 2025

# stat_types utiles (exemples disponibles sur FBref)
stat_types <- c("standard", "shooting", "passing", "passing_types", "passing_outcomes",
                "gca", "possession", "misc", "defense") 

# fonction simple : récupérer player-season stats et lister les tables
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


# Récupère un sample (Big5) et inspecte
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
df_sample8 <- read_excel("sca90.xlsx", col_types = c("text", "numeric", "numeric"))

df_merged <- Reduce(function(x, y) merge(x, y, by = "Player", all = TRUE),
                    list(df_sample1, df_sample2, df_sample3, df_sample4,
                         df_sample5, df_sample6, df_sample7, df_sample8))

# 1. Supprimer les colonnes dupliquées
df_merged <- df_merged[, !duplicated(names(df_merged))]

# 2. Supprimer les lignes dupliquées (mêmes noms de joueurs)
df_merged <- df_merged %>%
  distinct(Player, .keep_all = TRUE)

# 3. (optionnel) Nettoyer les espaces en trop dans les noms de joueurs
df_merged$Player <- trimws(df_merged$Player)

colnames(df_merged)

df_merged$prcarries90 <- df_merged$PrgC_Progression / (df_merged$Min_Playing / 90)
df_merged$prpasses90 <- df_merged$PrgP_Progression / (df_merged$Min_Playing / 90) #Règle simple souvent citée : une passe qui déplace le ballon d’au moins 10 yards (≈9,15 m) plus près du but adverse qu’auparavant, ou toute passe complétée dans la surface adverse
df_merged$prrecep90 <- df_merged$PrgR_Progression / (df_merged$Min_Playing / 90)
df_merged$kp90 <- df_merged$KP / (df_merged$Min_Playing / 90)
df_merged$Passes_ratees <- df_merged$Att_Total - df_merged$Cmp_Total
df_merged$passesratees90 <- df_merged$Passes_ratees / (df_merged$Min_Playing / 90)
df_merged$carriesratees <- df_merged$Dis_Carries + df_merged$Mis_Carries          #Dis_Carries = nombre de fois où le joueur a été dispossessed (dépossédé) pendant un carry (c.-à-d. il s’est fait arracher le ballon alors qu’il portait la balle).
df_merged$carriesratees90 <- df_merged$carriesratees / (df_merged$Min_Playing / 90) #Mis_Carries = nombre de miscontrols (contrôles ratés) sur des carries (c.-à-d. fois où le joueur a mal contrôlé / perdu le contrôle du ballon pendant une conduite).
df_merged$Ballons_perdus <- df_merged$Dis_Carries + df_merged$Mis_Carries + df_merged$Passes_ratees
df_merged$pertes90 <- df_merged$Ballons_perdus / (df_merged$Min_Playing / 90)
df_merged$passesfinalthird90 <- df_merged$Final_Third / (df_merged$Min_Playing / 90)
df_merged$crspa90 <- df_merged$CrsPA / (df_merged$Min_Playing / 90) #crosses into the penalty area
df_merged$tbpass90 <- df_merged$TB_Pass / (df_merged$Min_Playing / 90) #through passes passes en profondeur qui percent la defense
df_merged$cpa90 <- df_merged$CPA_Carries / (df_merged$Min_Playing / 90) # carries into the penalty area nombre de courses avec le ballon qui aboutissent dans la surface de réparation adverse.
df_merged$xa90 <- df_merged$xA_Expected / (df_merged$Min_Playing / 90) #xA par 90min

df1 <- df_merged[, c("Player","Squad.x","Comp.x","Nation.x","Pos.x","Born.x","Age.x","MP_Playing","Min_Playing","prpasses90","prcarries90","prrecep90","cpa90","passesfinalthird90","crspa90","tbpass90","pertes90","kp90","xa90","passesratees90","carriesratees90","sca","sca90")]

df3 <- df1 %>%  #bdd pour indice progression (que les variables utilisées conservées)
  mutate(Min_Playing = as.numeric(gsub(",", ".", as.character(Min_Playing)))) %>%
  filter(!is.na(Min_Playing) & Min_Playing >= 1000)

na_count <- sum(is.na(df3$pertes90))
print(na_count)#74 only variable with missing datas, 5% of the df (and not best players) -> can be removed
unique(as.character(df6[is.na(df3$pertes90), 1]))
df3_clean <- df3 %>% 
  filter(!is.na(pertes90))

#faire les mêmes indices par poste pour le scouting biensur, mais sur fbref pas distinction dc/lateral......
#indice création / progression du jeu
# variables utilisées
pass_vars <- c("prpasses90","passesfinalthird90","crspa90","tbpass90","kp90","xa90","passesratees90")
vars_core <- c("prpasses90","prcarries90","prrecep90","kp90","xa90","cpa90",
               "passesfinalthird90","crspa90","tbpass90","pertes90","passesratees90","carriesratees90","sca","sca90")

# vérifier présence colonnes
present <- vars_core %in% names(df3_clean)
if (any(!present)) {
  cat("Colonnes manquantes :", paste(vars_core[!present], collapse = ", "), "\n")
  # on continue quand même avec ce qui existe
}
vars_use <- vars_core[present]

# Correlation matrix (NA gardés pour corr)
mat <- df3_clean %>% select(all_of(vars_use)) %>% mutate(across(everything(), as.numeric))
cor_mat <- cor(mat, use = "pairwise.complete.obs")
print(round(cor_mat, 2))

# lister paires > 0.75
high_cor <- which(abs(cor_mat) > 0.75 & abs(cor_mat) < 1, arr.ind = TRUE)
if (nrow(high_cor) > 0) {
  cat("Paires très corrélées (>0.75) :\n")
  apply(high_cor, 1, function(i) {
    cat(rownames(cor_mat)[i[1]], " <-> ", colnames(cor_mat)[i[2]], 
        " = ", round(cor_mat[i[1], i[2]], 2), "\n")
  })
} else cat("Aucune paire > 0.75 détectée.\n")

df3_clean <- df3_clean %>% select(-kp90)
#I removed kp bcp highly correlated with other type of passes (0,9 with xa90) and you can esaily accumulate them if your mates shoot a lot while this is not necessarely a pass which creates a net occasion, #remove key passes bcs if mates have high volume but low qualities you accumulate kp easily, doesn't necessary create opportunities of scoring, other stats are more relevant
#passesfinalthird90 and prpasses90 highly correlated (0,84) maybe because a progressive passes is often also a pass toward the final third of the field
#only other variables highly correlated are prcarries90 & prrecep90  

#méthode percentiles + moyenne -> rewards polyvalence, players witch make the game to progress by many ways, not really pure volume of progressive actions
#Très robuste aux outliers.
#Interprétation simple : « joueur X est au 92ᵉ percentile ».
#Facile à expliquer et défendre en entretien/README.
#Comparable entre métriques hétérogènes sans transformations compliquées.
#Inconvénients
#Perte d’information d’échelle (la différence absolue est aplatie).
#Si deux métriques très corrélées existent, elles pèsent deux fois (double comptage).
#Sensitive au choix des métriques incluses et au seuil minimal de minutes/joueurs.
length(unique(df3_clean$Player))
length(unique(df4$Player))

df4 <- df3_clean
setdiff(df4$Player, df3_clean$Player)
setdiff(df3_clean$Player, df4$Player)

vars_keep <- c("prpasses90","passesfinalthird90","crspa90","tbpass90","xa90",
               "prcarries90","prrecep90","cpa90","pertes90") 
reverse_vars <- c("pertes90")    # variables où moins = mieux
min_non_na_frac <- 0.6           # exiger au moins 60% des metrics pour calculer l'indice
weights <- NULL                  # named numeric vector (optional), ex: c(prpasses90=1.5, prcarries90=1)



# vérifier présence des colonnes demandées
vars_use <- intersect(vars_keep, names(df4))
missing <- setdiff(vars_keep, vars_use)
if (length(missing) > 0) {
  warning("Ces colonnes de vars_keep sont absentes et seront ignorées : ", paste(missing, collapse = ", "))
}
# avertissement si reverse_vars contient des noms non inclus
rev_missing <- setdiff(reverse_vars, names(df4))
if (length(rev_missing) > 0) {
  # si reverse_vars non présents dans df4 on avertit ; s'ils ne sont pas dans vars_keep ils seront ignorés plus bas
  message("Attention : ces reverse_vars ne sont pas des colonnes existantes (ou non dans vars_keep) : ", paste(rev_missing, collapse = ", "))
}

# conversion sécurisée en numérique (virer les virgules)
for (v in vars_use) {
  df4[[v]] <- as.numeric(gsub(",", ".", as.character(df4[[v]])))
}

# calculer percentiles 0..100 ; percent_rank renvoie 0..1
pct_cols <- character(0)
for (v in vars_use) {
  # percent_rank sur le vecteur brut (NA -> NA)
  vec <- df4[[v]]
  # if all NA -> will produce NA
  if (all(is.na(vec))) {
    pct <- rep(NA_real_, length(vec))
  } else {
    pct <- dplyr::percent_rank(vec) * 100
    pct[is.na(vec)] <- NA_real_   # garder NA à l'endroit d'origine
  }
  # inversion si variable "moins mieux"
  if (v %in% reverse_vars) pct <- ifelse(is.na(pct), NA, 100 - pct)
  pct_name <- paste0(v, "_pct")
  df4[[pct_name]] <- pct
  pct_cols <- c(pct_cols, pct_name)
}

# préparation poids
vars_present <- vars_use
if (is.null(weights)) {
  wvec <- rep(1, length(vars_present))
  names(wvec) <- vars_present
} else {
  wvec <- weights[vars_present]
  wvec[is.na(wvec)] <- 1
  # normaliser n'est pas strictement nécessaire pour la moyenne pondérée mais fait sens
  wvec <- wvec / sum(wvec)
}

# fonction moyenne pondérée ligne en ignorant NA
row_weighted_mean <- function(vals, w) {
  non_na <- !is.na(vals)
  if (sum(non_na) == 0) return(NA_real_)
  wsub <- w[non_na]
  vals_sub <- vals[non_na]
  return(sum(vals_sub * wsub) / sum(wsub))
}

# calculer Index_pct_raw (moyenne des percentiles) en exigeant min_non_na_frac
min_required <- ceiling(length(pct_cols) * min_non_na_frac)
index_raw <- rep(NA_real_, nrow(df4))
for (i in seq_len(nrow(df4))) {
  row_vals <- as.numeric(df4[i, pct_cols])
  n_non_na <- sum(!is.na(row_vals))
  if (n_non_na < min_required) {
    index_raw[i] <- NA_real_
  } else {
    # correspondance poids : wvec nommé -> on fournit dans le même ordre que pct_cols
    # build weight vector aligned with pct_cols
    weight_aligned <- sapply(vars_present, function(nm) wvec[nm])
    # but we need weights for the percent columns in same order:
    weight_for_row <- weight_aligned
    index_raw[i] <- row_weighted_mean(row_vals, weight_for_row)
  }
}
df4$Index_pct_raw <- index_raw

df4 <- df4 %>%
  mutate(
    Index_pct_percentile_global = ifelse(is.na(Index_pct_raw), NA_real_, dplyr::percent_rank(Index_pct_raw) * 100)
  )

export_df <- df4 %>%
  select(Player, Squad.x, Pos.x, Index_pct_percentile_global) %>%
  arrange(desc(Index_pct_percentile_global))
write.csv2(export_df, "df4_index_global2.csv", row.names = FALSE, fileEncoding = "UTF-8") #exporte le fichier

##When I so the results, I was first surprised to find Lee Kang-In top 2 and Osame Sahraoui top 3, I thought that top players with creative skills would be better ranked (in fact Dembele and Yamal for ex are just a few point of percentiles behind Lee and Sahraoui)
#I wondered if it was because there were the only kind of players to be able to both make the game progress with passes and carries so I plot this graph with top40 progressive passers and top 40 progressive carriers to check whether some players were amazing at doing both

#nuage de points
top_pass  <- df4 %>% filter(!is.na(prpasses90))  %>% arrange(desc(prpasses90))  %>% slice_head(n = 40) %>% pull(Player)
top_carry <- df4 %>% filter(!is.na(prcarries90)) %>% arrange(desc(prcarries90)) %>% slice_head(n = 40) %>% pull(Player)

# données à tracer (union) + groupe
plot_df <- df4 %>%
  filter(Player %in% union(top_pass, top_carry)) %>%
  mutate(group = case_when(
    Player %in% top_pass & Player %in% top_carry ~ "both",
    Player %in% top_pass ~ "top_pass",
    TRUE ~ "top_carry"
  ))

# graphique
ggplot(plot_df, aes(x = prcarries90, y = prpasses90, color = group)) +
  geom_point(size = 2, alpha = 0.9) +
  geom_text_repel(aes(label = Player),
                  size = 3,                 # un peu plus gros
                  max.overlaps = 200,
                  box.padding = 0.25,
                  point.padding = 0.3,
                  segment.size = 0.3) +
  labs(x = "Progressive carries /90 (prcarries90)",
       y = "Progressive passes /90 (prpasses90)",
       title = "Top40 prpasses90 & Top40 prcarries90") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

##There are no apparent top progressive passers AND carriers looking at these stats, maybe a separate index for passing & carring would be more relevant ?
#separation for "passers profile" and "carriers profile" -> "midfielders" vs "wingers" profile

#Indice de contribution au jeu par la passe (combine danger XA et volume/équilibre de passes progressives)
#pour filtrer profil qui créé de la valeur par la passe ? Shortlist de recrues offensives/milieux créatifs : filtrer les profils qui créent de la valeur par la passe
df5 <- df4

pass_vars <- c("prpasses90","passesfinalthird90","crspa90","tbpass90")
xa_var <- "xa90"


df5$PassBalance <- rowMeans(df5[, pass_vars])
# 2) PassQuality_pct : percentile 0..100 de xa90
df5 <- df5 %>%
  mutate(PassQuality_pct = percent_rank(.data[[xa_var]]) * 100)

# 3) normaliser PassBalance sur 0..100
pb_min <- min(df5$PassBalance)
pb_max <- max(df5$PassBalance)
if (pb_max == pb_min) {
  df5$PassBalance_0_100 <- 100
} else {
  df5$PassBalance_0_100 <- 100 * (df5$PassBalance - pb_min) / (pb_max - pb_min)
}

# 4) PassIndex (moyenne simple des deux composantes)
df5 <- df5 %>%
  mutate(PassIndex_weighted_raw = 0.4 * PassBalance_0_100 + 0.6 * PassQuality_pct,
         PassIndex_weighted_pct = round(percent_rank(PassIndex_weighted_raw) * 100, 2))

export <- df5 %>%
  select(Player, Squad.x, Pos.x, Comp.x, PassIndex_weighted_pct) %>%
  arrange(desc(PassIndex_weighted_pct))
write.csv2(export, "df5_PassIndex_weighted_pct.csv", row.names = FALSE, fileEncoding = "UTF-8") 
##I'm really fine with the players that are best ranked by this index




df5 <- df4

pass_vars <- c("prpasses90","passesfinalthird90","crspa90","tbpass90")
xa_var <- "xa90"
miss_var <- "passesratees90"   

df5$PassBalance <- rowMeans(df5[, pass_vars], na.rm = TRUE)

df5 <- df5 %>%
  mutate(PassQuality_pct = percent_rank(.data[[xa_var]]) * 100)

df5 <- df5 %>%
  mutate(PassesMiss_pct = (1 - percent_rank(.data[[miss_var]])) * 100)

pb_min <- min(df5$PassBalance, na.rm = TRUE)
pb_max <- max(df5$PassBalance, na.rm = TRUE)
if (is.na(pb_min) | is.na(pb_max)) {
  df5$PassBalance_pct <- NA_real_
} else if (pb_max == pb_min) {
  df5$PassBalance_pct <- 100
} else {
  df5$PassBalance_pct <- 100 * (df5$PassBalance - pb_min) / (pb_max - pb_min)
}
weights <- c(balance = 0.4, quality = 0.5, accuracy = 0.1)
#the more we value quality, the better the rank of creative players will be, the more we value accuracy, the better the rank of more defensive players who don't try difficult passes will be

df5 <- df5 %>%
  mutate(
    PassIndex_weighted_raw = weights['balance'] * PassBalance_pct +
      weights['quality'] * PassQuality_pct +
      weights['accuracy'] * PassesMiss_pct,
    PassIndex_weighted_pct = round(percent_rank(PassIndex_weighted_raw) * 100, 2)
  )

export <- df5 %>%
  select(Player, Squad.x, Pos.x, Comp.x,
         PassBalance_pct,
         PassQuality_pct, PassesMiss_pct,
         PassIndex_weighted_pct) %>%
  arrange(desc(PassIndex_weighted_pct))

write.csv2(export, "df5_PassIndex_weighted_pct_with_misses.csv", row.names = FALSE, fileEncoding = "UTF-8")

#indice de contribution au jeu par les courses
df6 <- df3
sum(is.na(df6$sca90))
unique(as.character(df6[is.na(df6$sca90), 1]))
df6 <- df6 %>% 
  filter(!is.na(sca90))
unique(as.character(df6[is.na(df6$carriesratees90), 1]))
df6 <- df6 %>% 
  filter(!is.na(carriesratees90))

df6$scapercarry <- ifelse(df6$prcarries90 == 0, 0, df6$sca90 / df6$prcarries90)


carry_vars <- c("prcarries90","prrecep90","cpa90")
miss_var   <- "carriesratees90"

df5$PassBalance <- rowMeans(df5[, pass_vars], na.rm = TRUE)

df5 <- df5 %>%
  mutate(PassQuality_pct = percent_rank(.data[[xa_var]]) * 100)

df5 <- df5 %>%
  mutate(PassesMiss_pct = (1 - percent_rank(.data[[miss_var]])) * 100)

pb_min <- min(df5$PassBalance, na.rm = TRUE)
pb_max <- max(df5$PassBalance, na.rm = TRUE)
if (is.na(pb_min) | is.na(pb_max)) {
  df5$PassBalance_pct <- NA_real_
} else if (pb_max == pb_min) {
  df5$PassBalance_pct <- 100
} else {
  df5$PassBalance_pct <- 100 * (df5$PassBalance - pb_min) / (pb_max - pb_min)
}
weights <- c(balance = 0.4, quality = 0.5, accuracy = 0.1)
#the more we value quality, the better the rank of creative players will be, the more we value accuracy, the better the rank of more defensive players who don't try difficult passes will be

df5 <- df5 %>%
  mutate(
    PassIndex_weighted_raw = weights['balance'] * PassBalance_pct +
      weights['quality'] * PassQuality_pct +
      weights['accuracy'] * PassesMiss_pct,
    PassIndex_weighted_pct = round(percent_rank(PassIndex_weighted_raw) * 100, 2)
  )

export <- df5 %>%
  select(Player, Squad.x, Pos.x, Comp.x,
         PassBalance_pct,
         PassQuality_pct, PassesMiss_pct,
         PassIndex_weighted_pct) %>%
  arrange(desc(PassIndex_weighted_pct))

write.csv2(export, "df5_PassIndex_weighted_pct_with_misses.csv", row.names = FALSE, fileEncoding = "UTF-8")