# ============================================================
# PROJET SÉRIES TEMPORELLES – Analyse de la volatilité et des
# corrélations conditionnelles : MSCI World, Énergies Renouvelables
# et Immobilier
# ETFs : IWDA.L | INRG.L | IWDP.L
# Période : 01/01/2016 – 31/12/2025 | Fréquence : hebdomadaire
# Coupure Covid : 01/03/2020
# Auteurs : ADAMOU Firdaoussi | GARANGO Bintou | TOGNIBO Hariane
# M1 Économétrie et Statistiques – AMU – Pr. Lisandro FERMIN
# ============================================================


# Installation des packages

pkgs <- c("quantmod","xts","zoo","ggplot2","tidyr","dplyr",
          "PerformanceAnalytics","corrplot","ggcorrplot",
          "moments","FinTS","rugarch","MTS","vars",
          "gridExtra","grid","tseries","nortest")

for (p in pkgs) {
  if (!require(p, character.only = TRUE, quietly = TRUE))
    install.packages(p, quiet = TRUE)
  library(p, character.only = TRUE)
}

# Environnement et paramètres généraux

# setwd("C:/Users/haria/OneDrive/Documents/Serie Temporelle")

# Création des dossiers de sortie
for (d in c("figs","outputs_ex2","outputs_garch",
            "outputs_bekk","outputs_var","tables","data"))
  dir.create(d, showWarnings = FALSE)

# Paramètres de la période
date_debut <- as.Date("2016-01-01")
date_fin   <- as.Date("2025-12-31")
date_covid <- as.Date("2020-03-01")

tickers <- c("IWDA.L", "INRG.L", "IWDP.L")
noms    <- c("MSCI_World", "Energies_Renouvelables", "Immobilier")

couleurs <- c("MSCI_World"             = "steelblue",
              "Energies_Renouvelables" = "forestgreen",
              "Immobilier"             = "darkorange")

#  Chargement des données sur Yahoo Finance  
# fallback : data.csv local

cat("══════════════════════════════════════════════\n")
cat("  CHARGEMENT DES DONNÉES\n")
cat("══════════════════════════════════════════════\n")

prix <- tryCatch({
  cat("Téléchargement Yahoo Finance...\n")
  getSymbols(tickers,
             src         = "yahoo",
             from        = date_debut,
             to          = date_fin,
             periodicity = "weekly",
             auto.assign = TRUE)

  prix_IWDA <- Ad(IWDA.L)
  prix_INRG <- Ad(INRG.L)
  prix_IWDP <- Ad(IWDP.L)

  px <- merge(prix_IWDA, prix_INRG, prix_IWDP)
  colnames(px) <- noms
  px <- na.omit(px)
  cat("Téléchargement réussi :", nrow(px), "observations.\n")

  # Export data.csv
  prix_df <- data.frame(Date = index(px), coredata(px))
  write.csv(prix_df, "data.csv", row.names = FALSE)
  cat("data.csv exporté.\n")
  px

}, error = function(e) {
  cat("Téléchargement échoué. Lecture depuis data.csv...\n")
  df      <- read.csv("data.csv", stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  df      <- df[df$Date >= date_debut & df$Date <= date_fin, ]
  px      <- xts(df[, noms], order.by = df$Date)
  px      <- na.omit(px)
  cat("data.csv chargé :", nrow(px), "observations.\n")
  px
})

# Vérification des données
cat("\n--- Vérification des valeurs manquantes ---\n")
cat("Nombre total de NA par ETF :\n")
print(colSums(is.na(as.data.frame(prix))))
cat("Nombre total de NA (toutes colonnes) :", sum(is.na(prix)), "\n")
na_lignes <- which(apply(is.na(as.data.frame(prix)), 1, any))
if (length(na_lignes) == 0) {
  cat("Aucune valeur manquante détectée.\n")
} else {
  cat("Nombre de lignes avec NA :", length(na_lignes), "\n")
  print(prix[na_lignes, ])
}

prix <- na.omit(prix)

cat("\n--- Vérification des données ---\n")
cat("Période effective  :", format(index(prix)[1]),
    "à", format(index(prix)[nrow(prix)]), "\n")
cat("Nombre de semaines :", nrow(prix), "\n")
cat("\nPremières observations :\n"); print(head(prix))
cat("\nDernières observations :\n"); print(tail(prix))
cat("\nStatistiques de base sur les prix :\n")
print(summary(as.data.frame(prix)))

# Découpage pré/post Covid
prix_pre  <- prix[index(prix) < date_covid]
prix_post <- prix[index(prix) >= date_covid]

cat("\n--- Sous-périodes ---\n")
cat("Pré-Covid  :", format(index(prix_pre)[1]),  "à",
    format(index(prix_pre)[nrow(prix_pre)]),
    "–", nrow(prix_pre),  "semaines\n")
cat("Post-Covid :", format(index(prix_post)[1]), "à",
    format(index(prix_post)[nrow(prix_post)]),
    "–", nrow(prix_post), "semaines\n")


# EXERCICE 1

cat("\n══════════════════════════════════════════════\n")
cat("  EXERCICE 1\n")
cat("══════════════════════════════════════════════\n")

# a) Présentation des ETFs

# ETF 1 : iShares Core MSCI World UCITS ETF (IWDA.L)
# Réplique l'indice MSCI World (~1 500 grandes/moyennes caps. pays développés).
# États-Unis : ~71,5% | exposition forte aux techs (Apple, Microsoft, Nvidia).
# Coté sur le London Stock Exchange (LSE) en USD.
# Dividendes capitalisés (Acc). Non éligible au PEA.
# Frais de gestion (TER) : 0,20% par an.
# Référence pour s'exposer aux marchés actions mondiaux développés.

# ETF 2 : iShares Global Clean Energy UCITS ETF (INRG.L)
# Réplique le S&P Global Clean Energy (~100 entreprises mondiales :
# éolien, solaire, hydraulique, infrastructures énergétiques propres).
# Sociétés phares : Enphase Energy, Vestas Wind, First Solar.
# Coté LSE en USD. Dividendes capitalisés. Non éligible au PEA.
# TER : 0,65% par an. ETF thématique concentré, très volatile.
# Très sensible aux politiques climatiques et aux taux d'intérêt.

# ETF 3 : iShares Developed Markets Property Yield UCITS ETF (IWDP.L)
# Réplique le FTSE EPRA Nareit Developed (~300 foncières cotées REITs,
# pays développés versant des dividendes significatifs).
# Exposition : immobilier commercial, résidentiel, industriel, data centers.
# Coté LSE en USD. Dividendes capitalisés. Non éligible au PEA.
# TER : 0,59% par an. Très sensible aux taux d'intérêt des banques centrales.

# b) Graphique comparatif normalisé à 100 $

prix_norm      <- sweep(prix, 2, as.numeric(prix[1, ]), "/") * 100
prix_norm_df   <- data.frame(Date = index(prix_norm), coredata(prix_norm))
prix_norm_long <- pivot_longer(prix_norm_df, cols = -Date,
                               names_to = "ETF", values_to = "Valeur")

p_ex1 <- ggplot(prix_norm_long, aes(x = Date, y = Valeur, color = ETF)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.numeric(date_covid),
             linetype = "dashed", color = "black", linewidth = 0.7) +
  annotate("text",
           x     = date_covid,
           y     = max(prix_norm_long$Valeur, na.rm = TRUE) * 0.93,
           label = "Covid-19\nMars 2020",
           hjust = -0.1, size = 3.5, color = "black") +
  labs(
    title    = "Évolution comparée des ETF (base 100)",
    subtitle = "Période : janvier 2016 – décembre 2025 | Fréquence hebdomadaire",
    x        = "Année",
    y        = "Valeur (base 100$)",
    color    = "ETF",
    caption  = "Source : Yahoo Finance via quantmod | ETFs : IWDA.L, INRG.L, IWDP.L"
  ) +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c(
    "MSCI_World"             = "steelblue",
    "Energies_Renouvelables" = "forestgreen",
    "Immobilier"             = "darkorange"
  ))

print(p_ex1)
ggsave("figs/Exercice1_Evolution_ETF_base100.png",
       plot = p_ex1, width = 10, height = 6, dpi = 300)

# Interprétation :
# Pré-Covid (2016 – fév. 2020) :
# Les 3 ETF évoluent de façon modérée et relativement groupée.
# MSCI World progresse régulièrement jusqu'à ~150 (+50%).
# Immobilier suit à ~130, bénéficiant d'un environnement de taux bas.
# Énergies renouvelables stagnent autour de 100, sous-performant.

# Choc Covid (mars 2020) :
# Chute simultanée et brutale des 3 ETF.
# Immobilier le plus touché (chute sous 100  perte en capital).
# MSCI World et Énergies résistent légèrement mieux (diversification).

# Post-Covid (mars 2020 – déc. 2025) :
# Énergies renouvelables : envolée spectaculaire jusqu'à ~370 en 2021
#   (plan Biden, Green Deal européen, engouement ESG), puis krach violent
#   en 2022-2023 (hausse taux pénalise secteur endetté) → ~210 fin 2025.
# MSCI World : progression régulière et soutenue → ~245 fin 2025
#   portée par les grandes valeurs technologiques américaines.
# Immobilier : stagnation entre 100-140, sévèrement pénalisé par la
#   hausse des taux directeurs comprimant les valorisations des foncières.

# Conclusion : 3 profils de risque/rendement très distincts :
# MSCI World (+145%), Énergies (+110%), Immobilier (+10%).
# Cette combinaison est particulièrement pertinente pour l'analyse
# de la volatilité, des corrélations et de la transmission des chocs.

cat("Exercice 1 terminé.\n")


# EXERCICE 2

cat("\n══════════════════════════════════════════════\n")
cat("  EXERCICE 2\n")
cat("══════════════════════════════════════════════\n")

# Calcul des log-rendements géométriques : r_t = ln(P_t / P_{t-1})

calc_rend <- function(p) {
  r <- diff(log(p))
  colnames(r) <- noms
  na.omit(r)
}

rend      <- calc_rend(prix)       # Période complète
rend_pre  <- calc_rend(prix_pre)   # Pré-Covid
rend_post <- calc_rend(prix_post)  # Post-Covid

cat("Nombre de rendements :\n")
cat("  Période complète :", nrow(rend),      "\n")
cat("  Pré-Covid        :", nrow(rend_pre),  "\n")
cat("  Post-Covid       :", nrow(rend_post), "\n")

# 2a. Statistiques descriptives

stats_desc <- function(x) {
  c(
    Moyenne_hebdo       = mean(x),
    Rendement_annuel    = mean(x) * 52,
    Volatilite_hebdo    = sd(x),
    Volatilite_annuelle = sd(x) * sqrt(52),
    Minimum             = min(x),
    Maximum             = max(x),
    Skewness            = skewness(x),
    Kurtosis_exced      = kurtosis(x) - 3   # excès : 0 si loi normale
  )
}

afficher_stats <- function(rend_data, label) {
  cat("\n══════════════════════════════════════════════\n")
  cat("  STATISTIQUES DESCRIPTIVES –", label, "\n")
  cat("══════════════════════════════════════════════\n")
  stats <- sapply(as.data.frame(rend_data), stats_desc)
  print(round(stats, 6))
  cat("\nRendements annualisés (%) :\n")
  print(round(stats["Rendement_annuel", ] * 100, 2))
  cat("Volatilités annualisées (%) :\n")
  print(round(stats["Volatilite_annuelle", ] * 100, 2))
  cat("Ratio Sharpe simplifié (sans taux sans risque) :\n")
  sharpe <- stats["Rendement_annuel", ] / stats["Volatilite_annuelle", ]
  print(round(sharpe, 3))
}

afficher_stats(rend,      "PÉRIODE COMPLÈTE (jan. 2016 – déc. 2025)")
afficher_stats(rend_pre,  "PRÉ-COVID       (jan. 2016 – fév. 2020)")
afficher_stats(rend_post, "POST-COVID      (mars 2020 – déc. 2025)")

# Interprétation : Statistiques descriptives
# RENDEMENTS ANNUALISÉS :
# Période complète : MSCI World meilleur (12.04%) > Énergies (6.35%) > Immo (1.09%)
# Pré-Covid : Énergies légèrement meilleur (10.87%) ≈ MSCI (9.94%) > Immo (6.49%)
# Post-Covid : MSCI World domine (17.36%) > Énergies (8.49%) > Immo (0.54%)
#  L'immobilier très pénalisé post-Covid par la hausse des taux

# VOLATILITÉ ANNUALISÉE :
# Énergies Renouvelables = ETF le plus risqué sur toutes les périodes
# Période complète : Énergies (31.12%) >> Immo (18.03%) > MSCI (16.77%)
# Post-Covid : Énergies explose à 34.62%, MSCI reste stable (~16.52%)
#  Ratio rendement/risque post-Covid très défavorable pour les énergies

# SKEWNESS (asymétrie) :
# Période complète : tous négatifs  queues gauches épaisses
# → pertes extrêmes plus probables que gains extrêmes
# Pré-Covid : Immobilier positif (0.663) rare phénomène, queues à droite
# Post-Covid : skewness proches de 0  distributions plus symétriques

# KURTOSIS EXCÉDENTAIRE :
# Très élevé sur période complète : MSCI (17.32), Énergies (16.55)
# distributions leptokurtiques : queues très épaisses
#  présence de chocs extrêmes (Covid mars 2020 visible ici)
# Pré-Covid : kurtosis modéré (2-5)  distributions quasi-normales
# Post-Covid : kurtosis réduit mais encore élevé pour les énergies (12.54)

# Export statistiques CSV
stats_complet <- sapply(as.data.frame(rend),      stats_desc)
stats_pre     <- sapply(as.data.frame(rend_pre),  stats_desc)
stats_post    <- sapply(as.data.frame(rend_post), stats_desc)

export_stats <- rbind(
  data.frame(Periode = "Complete",   t(round(stats_complet, 6))),
  data.frame(Periode = "Pre_Covid",  t(round(stats_pre,     6))),
  data.frame(Periode = "Post_Covid", t(round(stats_post,    6)))
)
write.csv(export_stats, "tables/stats_descriptives.csv", row.names = TRUE)
cat("tables/stats_descriptives.csv exporté.\n")

# Tableaux PNG des statistiques
exporter_tableau <- function(stats_data, label, fichier) {
  stats  <- sapply(as.data.frame(stats_data), stats_desc)
  df     <- as.data.frame(round(t(stats), 4))
  df$ETF <- rownames(df)
  df     <- df[, c("ETF", names(df)[names(df) != "ETF"])]
  df$ETF <- c("MSCI World", "Energ. Renouv.", "Immobilier")

  png(fichier, width = 1600, height = 300, res = 120)
  grid.newpage()
  pushViewport(viewport(x = 0.5, y = 0.5, width = 0.96, height = 0.80))
  grid.table(df, rows = NULL, theme = ttheme_default(base_size = 9))
  popViewport()
  grid.text(paste("Statistiques descriptives –", label),
            x = 0.5, y = 0.97,
            gp = gpar(fontsize = 11, fontface = "bold"))
  grid.text("Source : Yahoo Finance via quantmod",
            x = 0.98, y = 0.02,
            gp = gpar(fontsize = 7, col = "gray40"), just = "right")
  dev.off()
  cat("Tableau exporté :", fichier, "\n")
}

exporter_tableau(rend,      "Période complète", "figs/stats_complete.png")
exporter_tableau(rend_pre,  "Pré-Covid",        "figs/stats_pre_covid.png")
exporter_tableau(rend_post, "Post-Covid",       "figs/stats_post_covid.png")

# Test de normalité Jarque-Bera 
cat("\n══════════════════════════════════════════════\n")
cat("  TEST DE NORMALITÉ – JARQUE-BERA\n")
cat("══════════════════════════════════════════════\n")
for (lbl in c("Complète","Pré-Covid","Post-Covid")) {
  rd <- switch(lbl, "Complète" = rend, "Pré-Covid" = rend_pre, "Post-Covid" = rend_post)
  cat("\n", lbl, ":\n")
  for (i in 1:3) {
    jb <- jarque.bera.test(as.numeric(rd[, i]))
    cat("  ", noms[i], "– p-value :", round(jb$p.value, 6),
        ifelse(jb$p.value < 0.05, "→ NON-NORMAL ***", "→ Normal"), "\n")
  }
}

# Histogramme avec courbe normale superposée
par(mfrow = c(1,3))
for (i in 1:3) {
  hist(as.numeric(rend[,i]), breaks = 40,
       probability = TRUE,
       main = paste("Distribution –", noms[i]),
       xlab = "Rendement", col = "lightblue", border = "white")
  curve(dnorm(x, mean = mean(as.numeric(rend[,i])),
              sd = sd(as.numeric(rend[,i]))),
        add = TRUE, col = "red", lwd = 2)
  legend("topright", legend = "Loi normale théorique",
         col = "red", lwd = 2, cex = 0.7, bty = "n")
}
mtext("Distribution des rendements vs loi normale",
      outer = TRUE, cex = 1.1, line = -1)

par(mfrow = c(1,1))

# Graphique : séries temporelles des rendements (clustering de volatilité)
rend_df   <- data.frame(Date = index(rend), coredata(rend))
rend_long <- tidyr::pivot_longer(rend_df, cols = -Date,
                                 names_to = "ETF", values_to = "Rendement")
rend_long$Periode <- factor(
  ifelse(rend_long$Date < date_covid, "Pré-Covid", "Post-Covid"),
  levels = c("Pré-Covid", "Post-Covid"))

p_rend <- ggplot(rend_long, aes(x = Date, y = Rendement, color = ETF)) +
  geom_line(linewidth = 0.4, alpha = 0.8) +
  geom_vline(xintercept = as.numeric(date_covid),
             linetype = "dashed", color = "black", linewidth = 0.6) +
  annotate("text", x = date_covid,
           y = max(rend_long$Rendement, na.rm = TRUE) * 0.85,
           label = "Covid-19", hjust = -0.1, size = 3.2) +
  facet_wrap(~ETF, ncol = 1, scales = "free_y") +
  labs(
    title    = "Rendements hebdomadaires au fil du temps",
    subtitle = "Clustering de volatilité visible autour de mars 2020",
    x = "Date", y = "Rendement",
    caption = "Source : Yahoo Finance via quantmod"
  ) +
  theme_minimal(base_size = 12) +
  scale_color_manual(values = couleurs) +
  theme(legend.position = "none")

print(p_rend)
ggsave("figs/Exercice2a_Rendements_temporels.png",
       plot = p_rend, width = 10, height = 7, dpi = 300)

# Graphique : Distributions des rendements pré vs post-Covid
p_distrib <- ggplot(rend_long, aes(x = Rendement, fill = Periode)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.5, position = "identity") +
  geom_density(aes(color = Periode), linewidth = 0.9) +
  facet_wrap(~ETF, scales = "free", ncol = 3) +
  labs(
    title    = "Distribution des rendements hebdomadaires – Pré vs Post-Covid",
    subtitle = "Rendements géométriques (log-rendements)",
    x = "Rendement", y = "Densité",
    caption = "Source : Yahoo Finance via quantmod"
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values  = c("Pré-Covid" = "steelblue",  "Post-Covid" = "firebrick")) +
  scale_color_manual(values = c("Pré-Covid" = "steelblue3", "Post-Covid" = "firebrick3"))

print(p_distrib)
ggsave("figs/Exercice2a_Distribution_rendements.png",
       plot = p_distrib, width = 12, height = 5, dpi = 300)

# 2b. Matrice des corrélations

dir.create("outputs_ex2", showWarnings = FALSE)

donnees_corr <- list(
  list(rend,      "Période complète"),
  list(rend_pre,  "Pré-Covid"),
  list(rend_post, "Post-Covid")
)

for (i in 1:3) {
  rd      <- donnees_corr[[i]]
  mat     <- cor(as.data.frame(rd[[1]]))
  label   <- rd[[2]]
  fichier <- paste0("outputs_ex2/correlation_", gsub(" ", "_", label), ".png")

  p_corr <- ggcorrplot(mat,
                  method   = "square",
                  type     = "full",
                  lab      = TRUE,
                  lab_size = 5,
                  colors   = c("firebrick", "white", "steelblue"),
                  title    = paste("Corrélations –", label),
                  ggtheme  = theme_minimal(base_size = 13)) +
    labs(caption = "Source : Yahoo Finance via quantmod") +
    theme(plot.caption = element_text(hjust = 1, color = "gray40", size = 8),
          plot.title   = element_text(hjust = 0.5, face = "bold"))

  print(p_corr)
  ggsave(fichier, plot = p_corr, width = 7, height = 6, dpi = 150)
  cat("Corrélation exportée :", fichier, "\n")
}

# Interprétation : Matrices des corrélations
# PÉRIODE COMPLÈTE :
# MSCI World – Immobilier : corrélation modérée (0.62)
# MSCI World – Énergies : corrélation modérée (0.57)
# Énergies – Immobilier : corrélation modérée (0.56)
#  Les 3 ETF évoluent dans le même sens mais restent diversifiants

# PRÉ-COVID :
# MSCI World – Immobilier : faible (0.31) excellente diversification
# MSCI World – Énergies : modérée (0.58), stable
# Énergies – Immobilier : forte (0.68) co-évolution surprenante
#  L'immobilier offrait une très bonne diversification face au marché mondial

# POST-COVID :
# MSCI World – Immobilier : forte (0.68)  hausse significative vs pré-Covid
# MSCI World – Énergies : modérée (0.49)  légère baisse vs pré-Covid
# Énergies – Immobilier : faible (0.45)  forte baisse vs pré-Covid (0.68)

# Conclusion :
# La crise Covid a profondément modifié les structures de corrélation.
# L'immobilier est devenu plus corrélé au marché mondial post-Covid :
# la hausse des taux a pesé simultanément sur actions et foncières cotées.
# Les corrélations dynamiques seront affinées par le modèle BEKK (Exercice 4).

# 2c. Fonctions d'autocorrélation (ACF)

tracer_acf <- function(etf_index, nom_etf) {

  r_complet <- as.numeric(rend[, etf_index])
  r_pre     <- as.numeric(rend_pre[, etf_index])
  r_post    <- as.numeric(rend_post[, etf_index])

  fichier <- paste0("outputs_ex2/Exercice2c_ACF_",
                    gsub(" ", "_", nom_etf), ".png")

  # Export PNG
  png(fichier, width = 1600, height = 900, res = 120)
  par(mfrow = c(2, 3), mar = c(5, 4, 3, 2), oma = c(1, 0, 4, 0))

  acf(r_complet,   main = "ACF Rendements\nPériode complète",
      lag.max = 20, col = "gray30",    lwd = 1.5)
  acf(r_pre,       main = "ACF Rendements\nPré-Covid",
      lag.max = 20, col = "steelblue", lwd = 1.5)
  acf(r_post,      main = "ACF Rendements\nPost-Covid",
      lag.max = 20, col = "firebrick", lwd = 1.5)
  acf(r_complet^2, main = "ACF Rendements²\nPériode complète",
      lag.max = 20, col = "gray30",    lwd = 1.5)
  acf(r_pre^2,     main = "ACF Rendements²\nPré-Covid",
      lag.max = 20, col = "steelblue", lwd = 1.5)
  acf(r_post^2,    main = "ACF Rendements²\nPost-Covid",
      lag.max = 20, col = "firebrick", lwd = 1.5)

  mtext(paste("ACF et ACF des rendements au carré –", nom_etf),
        outer = TRUE, cex = 1.2, fontface = "bold", line = 2)
  mtext("Source : Yahoo Finance via quantmod",
        outer = TRUE, cex = 0.7, col = "gray40", line = 0, adj = 1)
  dev.off()

  # Affichage interactif
  par(mfrow = c(2, 3), mar = c(5, 4, 3, 2), oma = c(0, 0, 4, 0))
  acf(r_complet,   main = "ACF Rendements\nPériode complète",
      lag.max = 20, col = "gray30",    lwd = 1.5)
  acf(r_pre,       main = "ACF Rendements\nPré-Covid",
      lag.max = 20, col = "steelblue", lwd = 1.5)
  acf(r_post,      main = "ACF Rendements\nPost-Covid",
      lag.max = 20, col = "firebrick", lwd = 1.5)
  acf(r_complet^2, main = "ACF Rendements²\nPériode complète",
      lag.max = 20, col = "gray30",    lwd = 1.5)
  acf(r_pre^2,     main = "ACF Rendements²\nPré-Covid",
      lag.max = 20, col = "steelblue", lwd = 1.5)
  acf(r_post^2,    main = "ACF Rendements²\nPost-Covid",
      lag.max = 20, col = "firebrick", lwd = 1.5)
  mtext(paste("ACF et ACF² –", nom_etf),
        outer = TRUE, cex = 1.2, fontface = "bold", line = 2)
  par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1), oma = c(0,0,0,0))

  cat("ACF exportée :", fichier, "\n")
}

tracer_acf(1, "MSCI World")
tracer_acf(2, "Energies Renouvelables")
tracer_acf(3, "Immobilier")

# Interprétation : ACF des rendements et rendements au carré

# LIGNE 1 : ACF des rendements bruts
# Vérifie l'absence d'autocorrélation dans les rendements (test bruit blanc)

# MSCI World :
# Période complète et post-Covid : quasi aucun lag significatif  bruit blanc
# Pré-Covid : quelques lags légèrement significatifs mais faibles
#  Rendements du MSCI World globalement non autocorrélés

# Énergies Renouvelables :
# Période complète et post-Covid : aucun lag significatif  bruit blanc
# Pré-Covid : quelques lags significatifs (lags 8-10)  légère mémoire
#  Rendements globalement non prédictibles

# Immobilier :
# Période complète : aucun lag significatif  bruit blanc
# Pré-Covid : quelques lags faiblement significatifs
# Post-Covid : plusieurs lags significatifs (lags 1-3)  légère autocorrélation
#  Légère prédictibilité des rendements immobiliers post-Covid

# LIGNE 2 : ACF des rendements au carré (r²)
#  Détecte les effets ARCH = clustering de volatilité
#  Si lags significatifs  variance non constante GARCH justifié

# MSCI World :
# Lags 2-3 significatifs sur période complète  effets ARCH présents
#  GARCH(1,1) justifié

# Énergies Renouvelables :
# Post-Covid : lag 3 très significatif (~0.40)  clustering marqué
# Pré-Covid : nombreux lags significatifs  effets ARCH forts
#  GARCH(1,1) clairement justifié

# Immobilier :
# Lags 2-4 significatifs et élevés (~0.25-0.50)  effets ARCH les plus forts
# Post-Covid : lags 1-3 très significatifs
# GARCH(1,1) fortement justifié

# Tests formels : Ljung-Box et ARCH-LM
cat("\n══════════════════════════════════════════════\n")
cat("  TEST DE LJUNG-BOX (lag = 10) – Rendements bruts\n")
cat("══════════════════════════════════════════════\n")
for (i in 1:3) {
  cat("\n", noms[i], ":\n")
  for (lbl in c("Complète","Pré-Covid","Post-Covid")) {
    rd <- switch(lbl,
                 "Complète"   = rend,
                 "Pré-Covid"  = rend_pre,
                 "Post-Covid" = rend_post)
    lb <- Box.test(as.numeric(rd[, i]), lag = 10, type = "Ljung-Box")
    cat(" ", lbl, "– p-value :", round(lb$p.value, 4),
        ifelse(lb$p.value < 0.05, "→ Autocorrélation significative", "→ Bruit blanc"), "\n")
  }
}

cat("\n══════════════════════════════════════════════\n")
cat("  TEST ARCH-LM (lag = 5) – Justification GARCH\n")
cat("══════════════════════════════════════════════\n")
for (i in 1:3) {
  cat("\n", noms[i], ":\n")
  for (lbl in c("Complète","Pré-Covid","Post-Covid")) {
    rd <- switch(lbl,
                 "Complète"   = rend,
                 "Pré-Covid"  = rend_pre,
                 "Post-Covid" = rend_post)
    at <- ArchTest(as.numeric(rd[, i]), lags = 5)
    cat(" ", lbl, "– p-value :", round(at$p.value, 4),
        ifelse(at$p.value < 0.05, "→ Effets ARCH significatif → GARCH justifié ***", ""), "\n")
  }
}

cat("Exercice 2 terminé.\n")


# EXERCICE 3

cat("\n══════════════════════════════════════════════\n")
cat("  EXERCICE 3 – GARCH(1,1)\n")
cat("══════════════════════════════════════════════\n")

# a) Rappels théoriques

# Le modèle GARCH(1,1) (Bollerslev, 1986) modélise la variance
# conditionnelle d'une série financière :
#
#   r_t      = mu + epsilon_t           (équation de la moyenne)
#   epsilon_t = sigma_t * z_t           (z_t ~ N(0,1))
#   sigma²_t = omega + alpha*epsilon²_{t-1} + beta*sigma²_{t-1}
#
# Interprétation des paramètres :
#   omega : niveau de base de la variance (constante > 0)
#   alpha : réactivité aux chocs passés (effet ARCH)
#            alpha élevé = volatilité réagit fortement aux chocs
#   beta  : persistance de la volatilité passée (effet GARCH)
#            beta élevé = les épisodes de volatilité durent longtemps
#   alpha + beta < 1 : condition de stationnarité de la variance
#   alpha + beta ≈ 1 : IGARCH  volatilité quasi-permanente
#
# Intérêt des modèles GARCH :
#   - Capturer le clustering de volatilité (les périodes agitées restent agitées)
#   - Modéliser une mémoire longue de la variance
#   - Éviter des modèles ARCH d'ordre très élevé (parcimonie)
#   - Prévoir la volatilité future
#   - Applications : VaR, pricing d'options, gestion de portefeuille

# b) Estimation GARCH(1,1) pour les 3 ETF

spec_garch <- ugarchspec(
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(0, 0)),
  distribution.model = "norm"
)

dir.create("outputs_garch", showWarnings = FALSE)

fits_complet <- list()
fits_pre     <- list()
fits_post    <- list()

for (i in 1:3) {
  cat("\n==================================================\n")
  cat("  GARCH(1,1) :", noms[i], "\n")
  cat("==================================================\n")

  fits_complet[[i]] <- ugarchfit(spec_garch, coredata(rend[, i]),
                                 solver = "hybrid")
  fits_pre[[i]]     <- ugarchfit(spec_garch, coredata(rend_pre[, i]),
                                 solver = "hybrid")
  fits_post[[i]]    <- ugarchfit(spec_garch, coredata(rend_post[, i]),
                                 solver = "hybrid")

  cat("\n--- Période complète ---\n")
  show(fits_complet[[i]])
  cat("Persistance (alpha+beta) :",
      round(coef(fits_complet[[i]])["alpha1"] +
              coef(fits_complet[[i]])["beta1"], 4), "\n")

  cat("\n--- Pré-Covid ---\n")
  show(fits_pre[[i]])
  cat("Persistance (alpha+beta) :",
      round(coef(fits_pre[[i]])["alpha1"] +
              coef(fits_pre[[i]])["beta1"], 4), "\n")

  cat("\n--- Post-Covid ---\n")
  show(fits_post[[i]])
  cat("Persistance (alpha+beta) :",
      round(coef(fits_post[[i]])["alpha1"] +
              coef(fits_post[[i]])["beta1"], 4), "\n")
}

cat("\nEstimation GARCH terminée.\n")

# Tableaux comparatifs des coefficients
params <- c("mu", "omega", "alpha1", "beta1")

for (i in 1:3) {
  coef_c  <- coef(fits_complet[[i]])[params]
  coef_p  <- coef(fits_pre[[i]])[params]
  coef_po <- coef(fits_post[[i]])[params]

  pers_c  <- coef_c["alpha1"]  + coef_c["beta1"]
  pers_p  <- coef_p["alpha1"]  + coef_p["beta1"]
  pers_po <- coef_po["alpha1"] + coef_po["beta1"]

  df_coef <- data.frame(
    Parametre  = c(params, "alpha+beta"),
    Complet    = round(c(coef_c,  pers_c),  6),
    Pre_Covid  = round(c(coef_p,  pers_p),  6),
    Post_Covid = round(c(coef_po, pers_po), 6)
  )

  fichier_tab <- paste0("outputs_garch/", noms[i], "_coefficients_GARCH.png")
  png(fichier_tab, width = 900, height = 350, res = 120)
  grid.newpage()
  pushViewport(viewport(x = 0.5, y = 0.45, width = 0.92, height = 0.65))
  grid.table(df_coef, rows = NULL, theme = ttheme_default(base_size = 10))
  popViewport()
  grid.text(paste("Coefficients GARCH(1,1) –", noms[i]),
            x = 0.5, y = 0.93,
            gp = gpar(fontsize = 12, fontface = "bold"))
  grid.text("Source : Yahoo Finance via quantmod | Package rugarch",
            x = 0.98, y = 0.02,
            gp = gpar(fontsize = 7, col = "gray40"), just = "right")
  dev.off()
  cat("Tableau exporté :", fichier_tab, "\n")
}

# Interprétation : Coefficients GARCH(1,1)

# MSCI World :
# Période complète : alpha=0.299, beta=0.700, persistance=0.999
#    Volatilité quasi-IGARCH : les chocs ont un effet quasi-permanent
# Pré-Covid : alpha=0.322, beta=0.677, persistance=0.999
#    Forte réactivité aux chocs ET très haute persistance
# Post-Covid : alpha=0.175, beta=0.786, persistance=0.961
#    Moins réactif aux chocs mais volatilité plus persistante dans le temps
#    Le marché mondial absorbe mieux les chocs post-Covid

# Énergies Renouvelables :
# Période complète : alpha=0.287, beta=0.685, persistance=0.972
# Pré-Covid : alpha=0.084, beta=0.862, persistance=0.945
#    Faible réactivité mais volatilité très persistante (domination GARCH)
# Post-Covid : alpha=0.203, beta=0.673, persistance=0.875
#    Réactivité accrue, persistance réduite
#    mu négatif : rendement moyen légèrement négatif
#    Secteur plus instable et moins prévisible post-Covid

# Immobilier :
# Période complète : alpha=0.378, beta=0.526, persistance=0.904
# Pré-Covid : alpha≈0.000, beta≈0.994, persistance=0.994
#    Quasi-IGARCH : chocs passés n'expliquent pas la volatilité immédiate
#    Volatilité très stable mais structurellement élevée
# Post-Covid : alpha=0.246, beta=0.587, persistance=0.833
#    Inversion : volatilité devient plus réactive aux chocs récents
#    Hausse des taux crée des chocs ponctuels plus importants
#    Baisse drastique de mu (0.0012 → 0.000027) confirme quasi-stagnation

# c) Graphiques variance et écart-type conditionnels et résidus

exporter_graphiques_garch <- function(fits_list, rend_list,
                                      nom_etf, couleur) {

  labels     <- c("Période complète", "Pré-Covid", "Post-Covid")
  sigma_list <- lapply(1:3, function(i)
    xts(sigma(fits_list[[i]]), order.by = index(rend_list[[i]])))
  var_list   <- lapply(sigma_list, function(s) s^2)
  res_list_s <- lapply(1:3, function(i)
    as.numeric(residuals(fits_list[[i]], standardize = TRUE)))

  # Image 1 : Écart-type et Variance conditionnels
  fichier1 <- paste0("outputs_garch/", nom_etf,
                     "_Exercice3c_variance_ecarttype.png")
  png(fichier1, width = 1600, height = 900, res = 120)
  par(mfrow = c(2, 3), mar = c(5, 4, 3, 2), oma = c(0, 0, 4, 0))

  for (i in 1:3) {
    plot(as.zoo(sigma_list[[i]]), col = couleur, lwd = 1.2,
         main = paste("Écart-type conditionnel\n", labels[i]),
         ylab = "Écart-type", xlab = "Date")
    mtext("Source : Yahoo Finance | rugarch",
          side = 1, line = 4, cex = 0.6, col = "gray40", adj = 1)
  }
  for (i in 1:3) {
    plot(as.zoo(var_list[[i]]), col = "firebrick", lwd = 1.2,
         main = paste("Variance conditionnelle\n", labels[i]),
         ylab = "Variance", xlab = "Date")
    mtext("Source : Yahoo Finance | rugarch",
          side = 1, line = 4, cex = 0.6, col = "gray40", adj = 1)
  }

  mtext(paste("GARCH(1,1) – Variance et Écart-type conditionnel –", nom_etf),
        outer = TRUE, cex = 1.3, fontface = "bold", line = 2)
  dev.off()

  # Affichage interactif
  par(mfrow = c(2, 3), mar = c(5, 4, 3, 2), oma = c(0, 0, 4, 0))
  for (i in 1:3) plot(as.zoo(sigma_list[[i]]), col = couleur, lwd = 1.2,
                       main = paste("Écart-type –", labels[i]),
                       ylab = "Écart-type", xlab = "Date")
  for (i in 1:3) plot(as.zoo(var_list[[i]]),   col = "firebrick", lwd = 1.2,
                       main = paste("Variance –", labels[i]),
                       ylab = "Variance", xlab = "Date")
  mtext(paste("GARCH(1,1) –", nom_etf),
        outer = TRUE, cex = 1.2, fontface = "bold", line = 2)
  par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1), oma = c(0,0,0,0))
  cat("Image 1 exportée :", fichier1, "\n")

  # Image 2 : Résidus standardisés et ACF résidus²
  fichier2 <- paste0("outputs_garch/", nom_etf,
                     "_Exercice3c_residus_acf.png")
  png(fichier2, width = 1600, height = 900, res = 120)
  par(mfrow = c(2, 3), mar = c(5, 4, 3, 2), oma = c(0, 0, 4, 0))

  for (i in 1:3) {
    res_xts <- xts(res_list_s[[i]], order.by = index(rend_list[[i]]))
    plot(as.zoo(res_xts), col = couleur, lwd = 0.8,
         main = paste("Résidus standardisés\n", labels[i]),
         ylab = "Résidus", xlab = "Date")
    abline(h =  0, col = "red",  lwd = 1.5)
    abline(h =  2, col = "gray", lwd = 1, lty = 2)
    abline(h = -2, col = "gray", lwd = 1, lty = 2)
    mtext("Source : Yahoo Finance | rugarch",
          side = 1, line = 4, cex = 0.6, col = "gray40", adj = 1)
  }
  for (i in 1:3) {
    acf(res_list_s[[i]]^2,
        main    = paste("ACF Résidus²\n", labels[i]),
        lag.max = 20, col = couleur, lwd = 1.5)
    mtext("Source : Yahoo Finance | rugarch",
          side = 1, line = 4, cex = 0.6, col = "gray40", adj = 1)
  }

  mtext(paste("GARCH(1,1) – Résidus et ACF –", nom_etf),
        outer = TRUE, cex = 1.3, fontface = "bold", line = 2)
  dev.off()

  # Affichage interactif
  par(mfrow = c(2, 3), mar = c(5, 4, 3, 2), oma = c(0, 0, 4, 0))
  for (i in 1:3) {
    res_xts <- xts(res_list_s[[i]], order.by = index(rend_list[[i]]))
    plot(as.zoo(res_xts), col = couleur, lwd = 0.8,
         main = paste("Résidus –", labels[i]),
         ylab = "Résidus", xlab = "Date")
    abline(h = c(0, 2, -2),
           col = c("red","gray","gray"), lwd = c(1.5,1,1), lty = c(1,2,2))
  }
  for (i in 1:3) acf(res_list_s[[i]]^2,
                     main = paste("ACF Résidus² –", labels[i]),
                     lag.max = 20, col = couleur, lwd = 1.5)
  mtext(paste("GARCH(1,1) Résidus –", nom_etf),
        outer = TRUE, cex = 1.2, fontface = "bold", line = 2)
  par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1), oma = c(0,0,0,0))
  cat("Image 2 exportée :", fichier2, "\n")

  # Diagnostic : Ljung-Box sur les résidus standardisés
  cat("\n Diagnostic résidus GARCH –", nom_etf, "\n")
  cat("  (Ljung-Box lag=10 sur résidus² → p>0.05 = modèle bien spécifié)\n")
  for (i in 1:3) {
    lb_r  <- Box.test(res_list_s[[i]],    lag = 10, type = "Ljung-Box")
    lb_r2 <- Box.test(res_list_s[[i]]^2,  lag = 10, type = "Ljung-Box")
    cat("   ", labels[i],
        "| LB résidus p =", round(lb_r$p.value, 4),
        "| LB résidus² p =", round(lb_r2$p.value, 4),
        ifelse(lb_r2$p.value > 0.05,
               "→ Modèle bien spécifié",
               "→ Effets ARCH résiduels détectés"), "\n")
  }
}

# Estimation et graphiques pour les 3 ETF
exporter_graphiques_garch(
  fits_list = list(fits_complet[[1]], fits_pre[[1]], fits_post[[1]]),
  rend_list = list(rend[,1], rend_pre[,1], rend_post[,1]),
  nom_etf   = "MSCI_World",
  couleur   = "steelblue"
)

exporter_graphiques_garch(
  fits_list = list(fits_complet[[2]], fits_pre[[2]], fits_post[[2]]),
  rend_list = list(rend[,2], rend_pre[,2], rend_post[,2]),
  nom_etf   = "Energies_Renouvelables",
  couleur   = "forestgreen"
)

exporter_graphiques_garch(
  fits_list = list(fits_complet[[3]], fits_pre[[3]], fits_post[[3]]),
  rend_list = list(rend[,3], rend_pre[,3], rend_post[,3]),
  nom_etf   = "Immobilier",
  couleur   = "darkorange"
)

# Interprétation : Variance et écart-type conditionnels

# MSCI World :
# Pic majeur mars 2020 (Covid) visible sur toutes les périodes.
# Pré-Covid : volatilité faible et stable (~0.01-0.02).
# Post-Covid : second pic en 2022 (Ukraine + hausse taux).
# Retour progressif à la normale fin 2024-2025.

# Énergies Renouvelables :
# Volatilité structurellement plus élevée que le MSCI World.
# Pic exceptionnel en 2020, fort en 2022-2023 (correction ESG).
# Nouveau pic fin 2025 (incertitudes politiques climatiques).
# Pré-Covid : forte volatilité 2016-2017 puis décroissante.

# Immobilier :
# Résultat remarquable pré-Covid : volatilité strictement décroissante
# de 2016 à 2020  marché très stable (alpha≈0 confirme ce résultat).
# Pic brutal mars 2020 puis retour rapide à la normale.
# Post-Covid : volatilité structurellement plus haute, oscillante.

# RÉSIDUS ET ACF :
# MSCI World : résidus bien centrés sur 0, quelques valeurs extrêmes 2020.
# ACF quasi nulle  modèle bien spécifié.
# Énergies : résidus plus dispersés avec valeurs extrêmes 2020 et 2024.
# ACF acceptable  GARCH(1,1) capture l'essentiel de la dépendance.
# Immobilier : résidus très dispersés pré-Covid (~6 en 2016).
# Post-Covid : bien centrés et dans les bandes ±2.
# ACF globalement satisfaisante sur toutes les périodes.

cat("\nExercice 3 terminé. Fichiers dans outputs_garch/\n")
cat("  - 3 tableaux de coefficients (*_coefficients_GARCH.png)\n")
cat("  - 3 images variance + écart-type (*_Exercice3c_variance_ecarttype.png)\n")
cat("  - 3 images résidus + ACF (*_Exercice3c_residus_acf.png)\n")


# EXERCICE 4

cat("\n══════════════════════════════════════════════\n")
cat("  EXERCICE 4 – BEKK(1,1)\n")
cat("══════════════════════════════════════════════\n")

# a) Rappels théoriques

# Le modèle BEKK (Baba-Engle-Kraft-Kroner, 1995) généralise le GARCH
# au cadre multivarié. Il modélise la matrice de covariance conditionnelle :

#   H_t = C'C + A' * epsilon_{t-1} * epsilon'_{t-1} * A + B' * H_{t-1} * B

# Interprétation des matrices :
#   C : matrice triangulaire inférieure (niveau de base des covariances)
#        garantit H_t semi-définie positive par construction
#   A : effets ARCH croisés – capture les spillovers de chocs entre actifs
#        ex : un choc sur les énergies affecte-t-il la volatilité du MSCI ?
#        éléments hors-diagonale de A = transmissions croisées de volatilité
#   B : effets GARCH croisés – persistance des covariances conditionnelles
#        ex : la co-volatilité MSCI/Immobilier persiste-t-elle dans le temps ?

# Intérêt principal :
#   - Mesurer les transmissions de volatilité entre marchés (spillovers)
#   - Modéliser des corrélations dynamiques (variant dans le temps)
#   - Évaluer la stabilité de la diversification en période de crise
#   - Construire une matrice de covariance conditionnelle pour la VaR

# Le BEKK(1,1) bivarié est le plus utilisé car il garantit la
# semi-définie positivité de H_t par construction, contrairement
# aux modèles VECH qui peuvent violer cette contrainte.

dir.create("outputs_bekk", showWarnings = FALSE)

# Fonction d'estimation et de diagnostic BEKK

estimer_bekk <- function(rend_biv_xts, nom1, nom2, label) {

  cat("\n──────────────────────────────────────────────\n")
  cat("  BEKK(1,1) –", nom1, "vs", nom2, "–", label, "\n")
  cat("──────────────────────────────────────────────\n")

  rend_mat <- as.matrix(coredata(rend_biv_xts))
  colnames(rend_mat) <- c(nom1, nom2)
  dates <- index(rend_biv_xts)

  bekk_fit <- MTS::BEKK11(rend_mat)

  #   Passons à la lecture des paramètres
  cat("\nCoefficients BEKK(1,1) :\n")
  print(bekk_fit$estimates)

  cat("\nLecture des matrices A (effets ARCH) et B (effets GARCH) :\n")
  # MTS::BEKK11 stocke les paramètres dans estimates
  # La structure : C(3 params tri.), A(4 params 2x2), B(4 params 2x2)
  # Interprétation hors-diagonale de A :
  cat("  → Éléments hors-diagonale de A : transmissions croisées de chocs\n")
  cat("(si ≠ 0 : un choc sur l'actif 1 affecte la volatilité de l'actif 2)\n")
  cat("  → Éléments hors-diagonale de B : persistance des co-volatilités croisées\n")

  # Faisons l'extraction des séries
  H   <- bekk_fit$Sigma.t
  Tn  <- nrow(H)
  dates_plot <- if (Tn == length(dates) - 1) dates[-1] else dates[seq_len(Tn)]

  corr_cond <- H[, 2] / sqrt(H[, 1] * H[, 4])
  vol1 <- sqrt(H[, 1])
  vol2 <- sqrt(H[, 4])

  cat("\nCorrélation conditionnelle –",
      "Moy:", round(mean(corr_cond), 4),
      "| Min:", round(min(corr_cond), 4),
      "| Max:", round(max(corr_cond), 4), "\n")

  # DIAGNOSTIC des résidus BEKK
  # Résidus blanchis : u_t = D_t^{-1/2} * epsilon_t
  # doivent se comporter comme un bruit blanc
  cat("\n  ─── Diagnostic des résidus BEKK ───\n")

  # Extraction des résidus bruts du BEKK
  # bekk_fit$residuals contient les epsilon_t
  if (!is.null(bekk_fit$residuals)) {
    et <- bekk_fit$residuals
  } else {
    
    et <- rend_mat[-1, ]
  }

  # Résidus standardisés : diviser par la volatilité conditionnelle
  u_t <- matrix(NA, nrow = Tn, ncol = 2)
  if (nrow(et) == Tn) {
    u_t[, 1] <- et[, 1] / vol1
    u_t[, 2] <- et[, 2] / vol2
    colnames(u_t) <- c(nom1, nom2)

    cat("\n  Covariance des résidus blanchis (doit être ≈ I) :\n")
    print(round(cov(u_t), 4))
    cat("  → Diagonale ≈ 1 et hors-diagonale ≈ 0 → bonne spécification\n")

    # Tests de Ljung-Box sur résidus blanchis
    cat("\n  Tests de Ljung-Box sur résidus blanchis (lag = 12) :\n")
    cat("  (H0 : absence d'autocorrélation → p > 0.05 = modèle correct)\n\n")
    for (j in 1:2) {
      lb_u    <- Box.test(u_t[, j],    lag = 12, type = "Ljung-Box")
      lb_u2   <- Box.test(u_t[, j]^2, lag = 12, type = "Ljung-Box")
      cat("  ", colnames(u_t)[j],
          "| LB résidus p =",   round(lb_u$p.value,  4),
          "| LB résidus² p =",  round(lb_u2$p.value, 4),
          ifelse(lb_u2$p.value > 0.05 & lb_u$p.value > 0.05,
                 "→ Bien spécifié",
                 "→ Dépendance résiduelle détectée"), "\n")
    }
  }

  invisible(list(bekk_fit  = bekk_fit,
                 vol1      = vol1,
                 vol2      = vol2,
                 corr_cond = corr_cond,
                 dates     = dates_plot,
                 u_t       = u_t))
}

# Fonction export graphiques BEKK

exporter_graphiques_bekk <- function(res_list, nom1, nom2) {

  labels   <- c("Période complète", "Pré-Covid", "Post-Covid")
  couleur2 <- ifelse(nom2 == "Energies_Renouvelables",
                     "forestgreen", "darkorange")
  fichier  <- paste0("outputs_bekk/", nom1, "_vs_", nom2, "_BEKK.png")

  # Image principale : Vol et Corrélations conditionnelles
  png(fichier, width = 1600, height = 1400, res = 120)
  par(mfrow = c(3, 3), mar = c(3, 3, 2.5, 1), oma = c(0, 0, 3, 0))

  # Ligne 1 : Volatilité ETF 1 (MSCI World)
  for (i in 1:3) {
    plot(res_list[[i]]$dates, res_list[[i]]$vol1,
         type = "l", col = "steelblue", lwd = 1.2,
         main = paste("Vol.", nom1, "–", labels[i]),
         xlab = "Date", ylab = "Écart-type cond.")
    mtext("Source : Yahoo Finance | Package MTS",
          side = 1, line = 2, cex = 0.5, col = "gray40", adj = 1)
  }

  # Ligne 2 : Volatilité ETF 2
  for (i in 1:3) {
    plot(res_list[[i]]$dates, res_list[[i]]$vol2,
         type = "l", col = couleur2, lwd = 1.2,
         main = paste("Vol.", nom2, "–", labels[i]),
         xlab = "Date", ylab = "Écart-type cond.")
    mtext("Source : Yahoo Finance | Package MTS",
          side = 1, line = 2, cex = 0.5, col = "gray40", adj = 1)
  }

  # Ligne 3 : Corrélation conditionnelle
  for (i in 1:3) {
    moy_i   <- round(mean(res_list[[i]]$corr_cond), 3)
    ylim_i  <- range(res_list[[i]]$corr_cond) + c(-0.05, 0.05)
    plot(res_list[[i]]$dates, res_list[[i]]$corr_cond,
         type = "l", col = couleur2, lwd = 1.2,
         main = paste("Corr. conditionnelle –", labels[i]),
         xlab = "Date", ylab = "Corrélation",
         ylim = ylim_i)
    abline(h = mean(res_list[[i]]$corr_cond), col = "red",
           lty = 2, lwd = 1.5)
    legend("topright",
           legend = c("Corr. cond.",
                      paste("Moy =", moy_i)),
           col = c(couleur2, "red"), lty = c(1, 2),
           cex = 0.65, bty = "n")
    mtext("Source : Yahoo Finance | Package MTS",
          side = 1, line = 2, cex = 0.5, col = "gray40", adj = 1)
  }

  mtext(paste("BEKK(1,1) –", nom1, "vs", nom2),
        outer = TRUE, cex = 1.2, line = 1, fontface = "bold")
  dev.off()
  cat("Image BEKK exportée :", fichier, "\n")

  # Image diagnostic : ACF des résidus blanchis 
  fichier_diag <- paste0("outputs_bekk/", nom1, "_vs_", nom2,
                         "_diagnostic_residus.png")
  png(fichier_diag, width = 1600, height = 900, res = 120)
  par(mfrow = c(2, 3), mar = c(5, 4, 3, 2), oma = c(0, 0, 4, 0))

  # Ligne 1 : ACF résidus bruts
  for (i in 1:3) {
    u <- res_list[[i]]$u_t
    if (!is.null(u) && !all(is.na(u[, 1]))) {
      acf(u[, 1], lag.max = 20,
          main = paste("ACF résidus", nom1, "\n", labels[i]),
          col = "steelblue", lwd = 1.5)
    } else {
      plot.new()
      title(paste("Résidus indisponibles –", labels[i]))
    }
    mtext("Source : Yahoo Finance | MTS",
          side = 1, line = 4, cex = 0.55, col = "gray40", adj = 1)
  }

  # Ligne 2 : ACF carrés résidus
  for (i in 1:3) {
    u <- res_list[[i]]$u_t
    if (!is.null(u) && !all(is.na(u[, 1]))) {
      acf(u[, 1]^2, lag.max = 20,
          main = paste("ACF résidus²", nom1, "\n", labels[i]),
          col = couleur2, lwd = 1.5)
    } else {
      plot.new()
      title(paste("Résidus² indisponibles –", labels[i]))
    }
    mtext("Source : Yahoo Finance | MTS",
          side = 1, line = 4, cex = 0.55, col = "gray40", adj = 1)
  }

  mtext(paste("Diagnostic résidus BEKK –", nom1, "vs", nom2),
        outer = TRUE, cex = 1.2, fontface = "bold", line = 2)
  dev.off()
  cat("Image diagnostic résidus exportée :", fichier_diag, "\n")

  # Affichage interactif
  par(mfrow = c(3, 3), mar = c(3, 3, 2, 1), oma = c(0, 0, 3, 0))
  for (i in 1:3) plot(res_list[[i]]$dates, res_list[[i]]$vol1,
                      type = "l", col = "steelblue", lwd = 1.2,
                      main = paste("Vol.", nom1, "–", labels[i]),
                      xlab = "", ylab = "Vol.")
  for (i in 1:3) plot(res_list[[i]]$dates, res_list[[i]]$vol2,
                      type = "l", col = couleur2, lwd = 1.2,
                      main = paste("Vol.", nom2, "–", labels[i]),
                      xlab = "", ylab = "Vol.")
  for (i in 1:3) {
    plot(res_list[[i]]$dates, res_list[[i]]$corr_cond,
         type = "l", col = couleur2, lwd = 1.2,
         main = paste("Corr. –", labels[i]),
         xlab = "", ylab = "ρ",
         ylim = range(res_list[[i]]$corr_cond) + c(-0.05, 0.05))
    abline(h = mean(res_list[[i]]$corr_cond), col = "red", lty = 2)
    legend("topright",
           legend = paste("Moy =",
                          round(mean(res_list[[i]]$corr_cond), 3)),
           cex = 0.65, bty = "n")
  }
  mtext(paste("BEKK(1,1) –", nom1, "vs", nom2),
        outer = TRUE, cex = 1.1, line = 1)
  par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1), oma = c(0,0,0,0))
}

# b) BEKK – MSCI World vs Énergies Renouvelables

cat("\n  PAIRE 1 : MSCI World vs Énergies Renouvelables\n")

res_WE <- list(
  estimer_bekk(rend[,     c("MSCI_World","Energies_Renouvelables")],
               "MSCI_World","Energies_Renouvelables","Période complète"),
  estimer_bekk(rend_pre[, c("MSCI_World","Energies_Renouvelables")],
               "MSCI_World","Energies_Renouvelables","Pré-Covid"),
  estimer_bekk(rend_post[,c("MSCI_World","Energies_Renouvelables")],
               "MSCI_World","Energies_Renouvelables","Post-Covid")
)

exporter_graphiques_bekk(res_WE, "MSCI_World", "Energies_Renouvelables")

# Interprétation : MSCI World vs Énergies Renouvelables

# Matrices A et B :
#  Si les éléments hors-diagonale de A sont significatifs :
#   un choc sur le MSCI World affecte la volatilité des énergies et vice versa
#  Si les éléments hors-diagonale de B sont significatifs :
#   la co-volatilité entre les deux actifs est persistante dans le temps

# Volatilité conditionnelle MSCI World :
# Pic majeur en mars 2020 visible sur les 3 périodes.
# Pré-Covid : faible et stable (~0.015-0.020).
# Post-Covid : second pic en 2022 (Ukraine + hausse taux).

# Volatilité conditionnelle Énergies Renouvelables :
# Structurellement plus élevée que le MSCI World.
# Pic exceptionnel en 2020, forte instabilité 2022-2023.
# Nouveau pic fin 2025 (incertitudes politiques climatiques).

# Corrélation conditionnelle MSCI World vs Énergies :
# Moy. ≈ 0.57 période complète.
# Pré-Covid : corrélation élevée (~0.689) et relativement stable.
# Post-Covid : très volatile (0.2 à 1.0), moy. ~0.622.
#  Bénéfices de diversification très variables et imprévisibles post-Covid.
#  La corrélation converge vers 1 lors des épisodes de stress.

# c) BEKK – MSCI World vs Immobilier

cat("\n  PAIRE 2 : MSCI World vs Immobilier\n")

res_WI <- list(
  estimer_bekk(rend[,     c("MSCI_World","Immobilier")],
               "MSCI_World","Immobilier","Période complète"),
  estimer_bekk(rend_pre[, c("MSCI_World","Immobilier")],
               "MSCI_World","Immobilier","Pré-Covid"),
  estimer_bekk(rend_post[,c("MSCI_World","Immobilier")],
               "MSCI_World","Immobilier","Post-Covid")
)

exporter_graphiques_bekk(res_WI, "MSCI_World", "Immobilier")

# Interprétation : MSCI World vs Immobilier

# Volatilité conditionnelle Immobilier :
# Pic mars 2020 plus intense que pour les énergies renouvelables.
#  Grande sensibilité des foncières aux chocs de liquidité.
# Pré-Covid : faible et décroissante (taux bas favorables).
# Post-Covid : durablement élevée jusqu'en 2022, puis décroît.

# Corrélation conditionnelle MSCI World vs Immobilier :
# Moy. ≈ 0.504 période complète.
# Pré-Covid : faible (~0.385) et très variable, proche de 0 parfois.
#  Excellentes propriétés de diversification pre-2020.
# Post-Covid : forte et stable (~0.625) → corrélation structurelle NOUVELLE.
#  La hausse des taux a simultanément pesé sur actions et foncières cotées.
#  Ce résultat illustre l'apport du BEKK : révéler que les corrélations
#   ne sont pas stables et que la diversification se réduit lors des crises.

cat("\nExercice 4 terminé. Fichiers dans outputs_bekk/\n")
cat("  - MSCI_World_vs_Energies_Renouvelables_BEKK.png\n")
cat("  - MSCI_World_vs_Energies_Renouvelables_diagnostic_residus.png\n")
cat("  - MSCI_World_vs_Immobilier_BEKK.png\n")
cat("  - MSCI_World_vs_Immobilier_diagnostic_residus.png\n")


# EXERCICE 5 

cat("\n══════════════════════════════════════════════\n")
cat("  EXERCICE 5 – MODÈLE VAR\n")
cat("══════════════════════════════════════════════\n")

# a) Rappels théoriques

# Le modèle VAR (Vector AutoRegressive, Sims 1980) modélise conjointement
# plusieurs séries temporelles. Chaque variable est expliquée par ses
# propres valeurs passées ET par les valeurs passées de toutes les autres :

#   Y_t = c + A_1*Y_{t-1} + ... + A_p*Y_{t-p} + u_t

# Avec Y_t = (r_MSCI, r_Énergie, r_Immo)' vecteur des rendements
#      A_i : matrices k×k de coefficients (p = ordre du VAR)
#      u_t : vecteur de bruits blancs (matrice de covariance Σ_u)

# Trois outils d'analyse complémentaires :

# 1. Sélection de l'ordre p : critères AIC, BIC, HQ, FPE
#     choisir le p qui minimise le critère retenu

# 2. Test de causalité de Granger :
#    H0 : Y1 ne Granger-cause pas Y2 (les lags de Y1 n'améliorent pas
#    la prévision de Y2 au-delà de ses propres lags)
#     p-value < 0.05 → causalité de Granger significative

# 3. Fonction de réponse aux impulsions (IRF) :
#    Mesure la réponse dynamique d'une variable à un choc unitaire
#    sur une autre variable. Ex : comment réagit l'Immobilier à un
#    choc sur le MSCI World sur les 10 semaines suivantes ?

# 4. Décomposition de la variance (FEVD) :
#    Quelle part de la variance d'erreur de prévision d'un ETF est
#    expliquée par ses propres chocs vs les chocs des autres ?

# Intérêt : comprendre les interdépendances dynamiques entre marchés
# et les canaux de transmission des chocs financiers.

cat("\n══════════════════════════════════════════════\n")
cat("  TESTS ADF – Stationnarité (prérequis VAR)\n")
cat("══════════════════════════════════════════════\n")
cat("H0 : série non stationnaire | p < 0.05 → rejet de H0 → stationnaire\n\n")

for (i in 1:3) {
  cat(noms[i], ":\n")
  for (lbl in c("Complète","Pré-Covid","Post-Covid")) {
    rd <- switch(lbl,
                 "Complète"   = rend,
                 "Pré-Covid"  = rend_pre,
                 "Post-Covid" = rend_post)
    adf_t <- adf.test(as.numeric(rd[, i]))
    cat(" ", lbl, "– p-value ADF :", round(adf_t$p.value, 4),
        ifelse(adf_t$p.value < 0.05,
               "→ Stationnaire",
               "→ Non stationnaire"), "\n")
  }
  cat("\n")
}

# b) Estimation du modèle VAR

dir.create("outputs_var", showWarnings = FALSE)

labels_var <- c("Période complète", "Pré-Covid", "Post-Covid")

# Période complète
cat("\n==================================================\n")
cat("  VAR – PÉRIODE COMPLÈTE\n")
cat("==================================================\n")
rend_df_c <- as.data.frame(rend)
sel_c     <- VARselect(rend_df_c, lag.max = 10, type = "const")
cat("Critères de sélection (ordre optimal) :\n")
print(sel_c$selection)
ordre_c   <- max(1L, as.integer(sel_c$selection["AIC(n)"]))
cat("Ordre retenu (AIC) :", ordre_c, "\n")
var_fit_c <- VAR(rend_df_c, p = ordre_c, type = "const")
print(summary(var_fit_c))

# Test de Granger
cat("\nTest de causalité de Granger – Période complète :\n")
cat("(H0 : pas de causalité | * = p<0.05)\n\n")
for (cause in noms) {
  gt <- causality(var_fit_c, cause = cause)$Granger
  cat(" ", cause, "→ autres : p =", round(gt$p.value, 4),
      ifelse(gt$p.value < 0.05, "(*) SIGNIFICATIF", ""), "\n")
}

# Test de Portmanteau sur résidus VAR
pt_c <- serial.test(var_fit_c, lags.pt = 10, type = "PT.asymptotic")
cat("\nTest de Portmanteau (autocorrélation des résidus) :\n"); print(pt_c)

irf_list_c    <- lapply(noms, function(imp)
  irf(var_fit_c, impulse=imp, response=noms,
      n.ahead=10, boot=TRUE, runs=200, ci=0.95))
names(irf_list_c) <- noms
fevd_res_c        <- fevd(var_fit_c, n.ahead = 10)
cat("\nFEVD – Période complète :\n"); print(fevd_res_c)
res_var_complet   <- list(var_fit=var_fit_c, irf_list=irf_list_c, fevd_res=fevd_res_c)

# Pré-Covid
cat("\n==================================================\n")
cat("  VAR – PRÉ-COVID\n")
cat("==================================================\n")
rend_df_p <- as.data.frame(rend_pre)
sel_p     <- VARselect(rend_df_p, lag.max = 10, type = "const")
cat("Critères de sélection :\n"); print(sel_p$selection)
ordre_p   <- max(1L, as.integer(sel_p$selection["AIC(n)"]))
cat("Ordre retenu :", ordre_p, "\n")
var_fit_p <- VAR(rend_df_p, p = ordre_p, type = "const")
print(summary(var_fit_p))

cat("\nTest de Granger – Pré-Covid :\n")
for (cause in noms) {
  gt <- causality(var_fit_p, cause = cause)$Granger
  cat(" ", cause, "→ autres : p =", round(gt$p.value, 4),
      ifelse(gt$p.value < 0.05, "(*) SIGNIFICATIF", ""), "\n")
}

pt_p <- serial.test(var_fit_p, lags.pt = 10, type = "PT.asymptotic")
cat("\nTest de Portmanteau :\n"); print(pt_p)

irf_list_p    <- lapply(noms, function(imp)
  irf(var_fit_p, impulse=imp, response=noms,
      n.ahead=10, boot=TRUE, runs=200, ci=0.95))
names(irf_list_p) <- noms
fevd_res_p        <- fevd(var_fit_p, n.ahead = 10)
cat("\nFEVD – Pré-Covid :\n"); print(fevd_res_p)
res_var_pre <- list(var_fit=var_fit_p, irf_list=irf_list_p, fevd_res=fevd_res_p)

# Post-Covid
cat("\n==================================================\n")
cat("  VAR – POST-COVID\n")
cat("==================================================\n")
rend_df_po <- as.data.frame(rend_post)
sel_po     <- VARselect(rend_df_po, lag.max = 10, type = "const")
cat("Critères de sélection :\n"); print(sel_po$selection)
ordre_po   <- max(1L, as.integer(sel_po$selection["AIC(n)"]))
cat("Ordre retenu :", ordre_po, "\n")
var_fit_po <- VAR(rend_df_po, p = ordre_po, type = "const")
print(summary(var_fit_po))

cat("\nTest de Granger – Post-Covid :\n")
for (cause in noms) {
  gt <- causality(var_fit_po, cause = cause)$Granger
  cat(" ", cause, "→ autres : p =", round(gt$p.value, 4),
      ifelse(gt$p.value < 0.05, "(*) SIGNIFICATIF", ""), "\n")
}

pt_po <- serial.test(var_fit_po, lags.pt = 10, type = "PT.asymptotic")
cat("\nTest de Portmanteau :\n"); print(pt_po)

irf_list_po    <- lapply(noms, function(imp)
  irf(var_fit_po, impulse=imp, response=noms,
      n.ahead=10, boot=TRUE, runs=200, ci=0.95))
names(irf_list_po) <- noms
fevd_res_po        <- fevd(var_fit_po, n.ahead = 10)
cat("\nFEVD – Post-Covid :\n"); print(fevd_res_po)
res_var_post <- list(var_fit=var_fit_po, irf_list=irf_list_po, fevd_res=fevd_res_po)

res_list_var <- list(res_var_complet, res_var_pre, res_var_post)

# Interprétation : Tests de Granger
# Période complète et post-Covid :
# MSCI World Granger-cause les autres ETF (p < 0.05)
#  le marché mondial est bien le générateur de chocs dominant
# Énergies Renouvelables et Immobilier ne Granger-causent pas le MSCI World
#  asymétrie claire : actifs sectoriels récepteurs, pas émetteurs
# Pré-Covid : causalités moins nettes (marchés plus indépendants)

# Export graphiques IRF

for (j in seq_along(noms)) {
  nom_choc <- noms[j]
  fichier  <- paste0("outputs_var/Exercice5_IRF_choc_", nom_choc, ".png")

  png(fichier, width = 1600, height = 1200, res = 120)
  par(mfrow = c(length(noms), 3), mar = c(4, 4, 3, 2), oma = c(0, 0, 4, 0))

  for (k in seq_along(noms)) {
    nom_resp <- noms[k]
    for (i in 1:3) {
      irf_obj <- res_list_var[[i]]$irf_list[[nom_choc]]
      irf_val <- irf_obj$irf[[nom_choc]][, nom_resp]
      irf_low <- irf_obj$Lower[[nom_choc]][, nom_resp]
      irf_up  <- irf_obj$Upper[[nom_choc]][, nom_resp]
      horizon <- 0:(length(irf_val) - 1)

      irf_val[!is.finite(irf_val)] <- 0
      irf_low[!is.finite(irf_low)] <- 0
      irf_up[!is.finite(irf_up)]   <- 0

      ylim_range <- range(c(irf_low, irf_up), na.rm = TRUE)
      if (diff(ylim_range) < 1e-6)
        ylim_range <- ylim_range + c(-0.01, 0.01)

      plot(horizon, irf_val,
           type = "l", col = "steelblue", lwd = 1.8,
           ylim = ylim_range,
           main = paste(nom_resp, "–", labels_var[i]),
           xlab = "Horizon (semaines)", ylab = "Réponse")
      lines(horizon, irf_low, col = "gray50", lty = 2)
      lines(horizon, irf_up,  col = "gray50", lty = 2)
      abline(h = 0, col = "red", lwd = 1.2, lty = 2)
      mtext("Source : Yahoo Finance | Package vars",
            side = 1, line = 3, cex = 0.5, col = "gray40", adj = 1)
    }
  }

  mtext(paste("IRF – Choc sur", nom_choc),
        outer = TRUE, cex = 1.2, line = 1.5, fontface = "bold")
  dev.off()
  cat("IRF exportée :", fichier, "\n")
}

# Affichage interactif IRF
for (j in seq_along(noms)) {
  nom_choc <- noms[j]
  par(mfrow = c(length(noms), 3), mar = c(4,4,3,2), oma = c(0,0,4,0))
  for (k in seq_along(noms)) {
    nom_resp <- noms[k]
    for (i in 1:3) {
      irf_obj <- res_list_var[[i]]$irf_list[[nom_choc]]
      irf_val <- irf_obj$irf[[nom_choc]][, nom_resp]
      irf_low <- irf_obj$Lower[[nom_choc]][, nom_resp]
      irf_up  <- irf_obj$Upper[[nom_choc]][, nom_resp]
      horizon <- 0:(length(irf_val) - 1)
      irf_val[!is.finite(irf_val)] <- 0
      irf_low[!is.finite(irf_low)] <- 0
      irf_up[!is.finite(irf_up)]   <- 0
      ylim_r <- range(c(irf_low, irf_up), na.rm = TRUE)
      if (diff(ylim_r) < 1e-6) ylim_r <- ylim_r + c(-0.01, 0.01)
      plot(horizon, irf_val,
           type = "l", col = "steelblue", lwd = 1.5,
           ylim = ylim_r,
           main = paste(nom_resp, "–", labels_var[i]),
           xlab = "Horizon", ylab = "Réponse")
      lines(horizon, irf_low, col = "gray", lty = 2)
      lines(horizon, irf_up,  col = "gray", lty = 2)
      abline(h = 0, col = "red", lty = 2)
    }
  }
  mtext(paste("IRF – Choc sur", nom_choc),
        outer = TRUE, cex = 1.2, line = 1)
  par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1), oma = c(0,0,0,0))
}

# Export graphiques FEVD

fichier_fevd <- "outputs_var/Exercice5_FEVD.png"
couleurs_fevd <- c("steelblue", "forestgreen", "darkorange")

png(fichier_fevd, width = 1600, height = 1200, res = 120)
par(mfrow = c(length(noms), 3), mar = c(4, 4, 3, 2), oma = c(0, 0, 4, 0))
for (k in seq_along(noms)) {
  nom_etf <- noms[k]
  for (i in 1:3) {
    fevd_df <- as.data.frame(res_list_var[[i]]$fevd_res[[nom_etf]])
    barplot(t(as.matrix(fevd_df)),
            col         = couleurs_fevd,
            border      = NA,
            names.arg   = 1:nrow(fevd_df),
            main        = paste("FEVD –", nom_etf, "\n", labels_var[i]),
            xlab        = "Horizon",
            ylab        = "Part de variance",
            legend.text = colnames(fevd_df),
            args.legend = list(x = "topright", cex = 0.65, bty = "n"))
    mtext("Source : Yahoo Finance | Package vars",
          side = 1, line = 3, cex = 0.5, col = "gray40", adj = 1)
  }
}
mtext("Décomposition de la variance (FEVD)",
      outer = TRUE, cex = 1.2, line = 1, fontface = "bold")
dev.off()
cat("FEVD exportée :", fichier_fevd, "\n")

# Affichage interactif FEVD
par(mfrow = c(length(noms), 3), mar = c(4,4,3,2), oma = c(0,0,4,0))
for (k in seq_along(noms)) {
  nom_etf <- noms[k]
  for (i in 1:3) {
    fevd_df <- as.data.frame(res_list_var[[i]]$fevd_res[[nom_etf]])
    barplot(t(as.matrix(fevd_df)),
            col = couleurs_fevd, border = NA,
            names.arg   = 1:nrow(fevd_df),
            main        = paste("FEVD –", nom_etf, "–", labels_var[i]),
            xlab        = "Horizon",
            ylab        = "Part de variance",
            legend.text = colnames(fevd_df),
            args.legend = list(x = "topright", cex = 0.7, bty = "n"))
  }
}
mtext("FEVD", outer = TRUE, cex = 1.2, line = 1)
par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1), oma = c(0,0,0,0))

# Interprétation : IRF et FEVD

# IRF – Choc sur MSCI World :
# Un choc positif sur le MSCI World se transmet immédiatement
# et significativement aux deux autres ETF dès la semaine 1,
# puis s'estompe rapidement vers 0 en 2-3 semaines.
#  Confirme le rôle central du marché mondial comme émetteur de chocs.
#  L'effet est légèrement plus fort post-Covid : intégration accrue.
#  Intervalles de confiance du même signe → statistiquement significatif.

# IRF – Choc sur Énergies Renouvelables :
# Impact très limité sur le MSCI World (intervalles englobent 0).
#  Absence de causalité de Granger dans ce sens.
# L'immobilier réagit faiblement et brièvement.
# Pré-Covid : effets plus volatils, incertitude sur ce secteur émergent.

# IRF – Choc sur Immobilier :
# Effets très limités sur les autres ETF.
#  Secteur davantage récepteur qu'émetteur de chocs.
# Légère réaction négative des énergies post-Covid :
#  compétition pour les capitaux entre deux secteurs très endettés.

# FEVD :
# MSCI World : >95% expliqué par ses propres chocs  actif autonome.
# Énergies : ~70% propres chocs, ~30% MSCI World  sensibilité structurelle.
# Immobilier : ~55% propres chocs, ~43% MSCI World  forte dépendance.
#  Cette dépendance de l'immobilier au MSCI est stable avant et après Covid.
#  Caractère structurel (pas conjoncturel) de la relation.

cat("\n══════════════════════════════════════════════\n")
cat("  TOUS LES EXERCICES TERMINÉS\n")
cat("══════════════════════════════════════════════\n")
cat("\nFichiers exportés :\n")
cat("  figs/              : Ex.1 graphique normalisé | Ex.2 distributions\n")
cat("                       Ex.2 rendements temporels | tableaux stats\n")
cat("  outputs_ex2/       : corrélations | ACF par ETF\n")
cat("  outputs_garch/     : coefficients GARCH | variance | résidus\n")
cat("  outputs_bekk/      : BEKK volatilités+corrélations | résidus\n")
cat("  outputs_var/       : IRF par choc | FEVD\n")
cat("  tables/            : stats_descriptives.csv\n")
cat("  data.csv           : données exportées\n")

