# Projet pour la SAÉ 5.EMS.01 

####Analyse des ventes de produits dans un réseau de magasins

# 1) CHARGEMENT DES LIBRAIRIES

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readr)

#2)IMPORTATION DES DONNÉES 

ventes <- read.csv("C:/Users/dieye/Downloads/data_ventes_2022_2024.csv",
                   stringsAsFactors = FALSE, encoding = "UTF-8")

finances <- read.csv("C:/Users/dieye/Downloads/donnees_financieres_magasins_2022_2024.csv",
                     stringsAsFactors = FALSE, encoding = "UTF-8")


#3)HARMONISATION DES DATES ET NOMS
ventes$Date <- ymd(ventes$Date)
finances$date <- ymd(finances$date)

names(finances)[names(finances) == "date"] <- "Date"
names(finances)[names(finances) == "magasin"] <- "Magasin"

ventes$Promotion <- as.factor(ventes$Promotion)


#4)DIAGNOSTIC DES VALEURS MANQUANTES
missing_ventes <- colSums(is.na(ventes))
print(missing_ventes)

missing_finances <- colSums(is.na(finances))
print(missing_finances)

cat("Pourcentage de NA dans Quantité_Vendue :",
    round(100 * missing_ventes["Quantité_Vendue"] / nrow(ventes), 2), "%\n")


#5)CRÉATION DES VARIABLES TEMPORELLES

jours_fr <- c("dimanche","lundi","mardi","mercredi","jeudi","vendredi","samedi")
jours_ordre <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")

ventes <- ventes %>%
  mutate(
    Annee = year(Date),
    Mois = month(Date),
    Jour = day(Date),
    JourSemaine = factor(jours_fr[wday(Date)], levels = jours_ordre),
    Weekend = as.factor(ifelse(JourSemaine %in% c("samedi","dimanche"),
                               "Weekend","Semaine")),Semaine = week(Date) )

#Correction / harmonisation : variable Week-End pour les graphiques

ventes <- ventes %>%
  mutate(
    # Version numérique 0/1 
    Week_End = ifelse(JourSemaine %in% c("samedi", "dimanche"), 1, 0),
    
    # Version texte (celle qu’on utilisera dans les graphiques stylés)
    Type_Jour = ifelse(Week_End == 1, "Weekend", "Semaine")
  )

#Vérification rapide 
table(ventes$Type_Jour)


#6)DÉTECTION DES OUTLIERS (IQR)
Q1 <- quantile(ventes$Quantité_Vendue, 0.25, na.rm = TRUE)
Q3 <- quantile(ventes$Quantité_Vendue, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

borne_inf <- Q1 - 1.5 * IQR_val
borne_sup <- Q3 + 1.5 * IQR_val

nb_outliers <- sum(ventes$Quantité_Vendue < borne_inf |
                     ventes$Quantité_Vendue > borne_sup, na.rm = TRUE)

cat("Q1 =", Q1, " Q3 =", Q3, " IQR =", IQR_val, "\n")
cat("Bornes [", borne_inf, ",", borne_sup, "]\n")
cat("Nombre d'outliers :", nb_outliers,
    " (", round(100 * nb_outliers / nrow(ventes), 2), "% )\n")


#7)IMPUTATION HIÉRARCHIQUE PAR MÉDIANE 
medians_pmj <- ventes %>%
  filter(!is.na(Quantité_Vendue)) %>%
  group_by(Produit, Magasin, JourSemaine) %>%
  summarise(med_pmj = median(Quantité_Vendue), .groups = "drop")

medians_pm <- ventes %>%
  filter(!is.na(Quantité_Vendue)) %>%
  group_by(Produit, Magasin) %>%
  summarise(med_pm = median(Quantité_Vendue), .groups = "drop")

medians_p <- ventes %>%
  filter(!is.na(Quantité_Vendue)) %>%
  group_by(Produit) %>%
  summarise(med_p = median(Quantité_Vendue), .groups = "drop")

median_global <- median(ventes$Quantité_Vendue, na.rm = TRUE)

ventes <- ventes %>%
  left_join(medians_pmj, by = c("Produit","Magasin","JourSemaine")) %>%
  left_join(medians_pm, by = c("Produit","Magasin")) %>%
  left_join(medians_p, by = "Produit") %>%
  mutate(Quantité_Vendue = coalesce(Quantité_Vendue,
                                    med_pmj, med_pm, med_p, median_global)) %>%
  select(-med_pmj, -med_pm, -med_p)

cat("NA restants dans Quantité_Vendue :",
    sum(is.na(ventes$Quantité_Vendue)), "\n")


#8)CONVERSIONS ET NOUVELLES VARIABLES


ventes$Quantité_Vendue <- as.integer(ventes$Quantité_Vendue)
ventes$Prix_Unitaire <- as.numeric(ventes$Prix_Unitaire)

ventes$Produit  <- as.factor(ventes$Produit)
ventes$Magasin  <- as.factor(ventes$Magasin)


# Chiffre d'affaires
ventes <- ventes %>%
  mutate(CA = Quantité_Vendue * Prix_Unitaire)

#9) STATISTIQUES DESCRIPTIVES (APRÈS IMPUTATION)
stats_produit <- ventes %>%
  group_by(Produit) %>%
  summarise(
    Moyenne = round(mean(Quantité_Vendue, na.rm = TRUE), 2),
    Mediane = round(median(Quantité_Vendue, na.rm = TRUE), 2),
    Min = min(Quantité_Vendue, na.rm = TRUE),
    Max = max(Quantité_Vendue, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
print(stats_produit)

stats_magasin <- ventes %>%
  group_by(Magasin) %>%
  summarise(
    Moyenne = round(mean(Quantité_Vendue, na.rm = TRUE), 2),
    n = n(),
    .groups = "drop"
  )
print(stats_magasin)

stats_promotion <- ventes %>%
  group_by(Promotion) %>%
  summarise(
    Moyenne = round(mean(Quantité_Vendue, na.rm = TRUE), 2),
    n = n(),
    .groups = "drop"
  )
print(stats_promotion)

stats_annee <- ventes %>%
  group_by(Annee) %>%
  summarise(
    Moyenne = round(mean(Quantité_Vendue, na.rm = TRUE), 2),
    Mediane = round(median(Quantité_Vendue, na.rm = TRUE), 2),
    Total = sum(Quantité_Vendue, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(stats_annee)


#2.ANALYSE DESCRIPTIVE 
#2.2 Thème graphique 
c_bleu <- "#2E86AB"
c_rouge <- "#E94F37"
c_vert  <- "#2A9D8F"

theme_simple <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

#2.3 TENDANCES SAISONNIÈRES
##G1 – Évolution des ventes dans le temps
ventes_jour <- ventes %>%
  group_by(Date) %>%
  summarise(Quantite_Totale = sum(Quantité_Vendue), .groups = "drop")

p1 <- ggplot(ventes_jour, aes(x = Date, y = Quantite_Totale)) +
  geom_line(color = c_bleu, linewidth = 0.6) +
  geom_smooth(method = "loess", se = TRUE,
              color = c_rouge, linewidth = 1.2,
              fill = c_rouge, alpha = 0.2) +
  labs(title = "Tendances saisonnières",
       subtitle = "Évolution des ventes dans le temps",
       x = "Date",
       y = "Quantité totale vendue") +
  theme_simple

print(p1)

##G2 – Ventes moyennes par mois
noms_mois <- c("Janv.", "Févr.", "Mars", "Avr.", "Mai", "Juin",
               "Juil.", "Août", "Sept.", "Oct.", "Nov.", "Déc.")
ventes_mois <- ventes %>%
  group_by(Mois) %>%
  summarise(Moyenne = mean(Quantité_Vendue), .groups = "drop") %>%
  mutate(Mois_lib = factor(noms_mois[Mois], levels = noms_mois))

p2 <- ggplot(ventes_mois, aes(x = Mois_lib, y = Moyenne)) +
  geom_col(fill = c_bleu, alpha = 0.85) +
  labs(title = "Tendances saisonnières par mois",
       x = "Mois",
       y = "Quantité moyenne vendue") +
  theme_simple +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)


# 2.4 EFFET DES PROMOTIONS

#G3 : Effet des promotions 

c_bleu <- "#2E86AB"   # bleu
c_orange <- "#F4A261" # orange

effet_promo <- ventes %>%
  group_by(Promotion) %>%
  summarise(Moyenne = mean(Quantité_Vendue), .groups = "drop")

p3 <- ggplot(effet_promo, aes(x = Promotion, y = Moyenne, fill = Promotion)) +
  geom_col(width = 0.5, alpha = 0.9) +
  geom_text(aes(label = round(Moyenne, 0)),
            vjust = -0.4, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("0" = c_bleu, "1" = c_orange), guide = "none") +
  labs(title = "Effet des promotions",
       x = "Promotion (0 = Non, 1 = Oui)",
       y = "Quantité moyenne vendue") +
  theme_simple

print(p3)


# 2.5 SEMAINE VS WEEK-END
## G5 – Ventes moyennes : semaine vs week-end

stats_weekend <- ventes %>%
  group_by(Week_End) %>%
  summarise(Moyenne = mean(Quantité_Vendue), .groups = "drop") %>%
  mutate(Type = ifelse(Week_End == 1, "Weekend", "Semaine"))

p5 <- ggplot(stats_weekend, aes(x = Type, y = Moyenne, fill = Type)) +
  geom_col(width = 0.5, alpha = 0.9) +
  geom_text(aes(label = round(Moyenne, 0)),
            vjust = -0.4, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Semaine" = c_bleu, "Weekend" = c_vert),
                    guide = "none") +
  labs(title = "Différence semaine / week-ends",
       x = "",
       y = "Quantité moyenne vendue") +
  theme_simple

print(p5)

#11) RATIO DE COMPÉTITION (par magasin)
ventes_moyennes_jour <- ventes %>%
  group_by(Date, Magasin) %>%
  summarise(
    Ventes_Totales = sum(Quantité_Vendue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Date) %>%
  mutate(
    Ventes_Moyenne_Autres =
      (sum(Ventes_Totales) - Ventes_Totales) / (n() - 1)
  ) %>%
  ungroup() %>%
  mutate(
    Ratio_Competition = Ventes_Totales / Ventes_Moyenne_Autres
  )

# Vérification
head(ventes_moyennes_jour)
summary(ventes_moyennes_jour$Ratio_Competition)

#Distrubition du ratio par magasin
p6 <- ggplot(ventes_moyennes_jour,
              aes(x = Magasin, y = Ratio_Competition, fill = Magasin)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.1, alpha = 0.5, outlier.alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "red", linewidth = 1) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 3, fill = "white") +
  labs(
    title = "Ratio de compétition par magasin",
    subtitle = "Ratio = Ventes magasin / Moyenne des autres (1 = moyenne)",
    x = "Magasin",
    y = "Ratio de compétition"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

print(p6)


#12) FUSION AVEC DONNÉES FINANCIÈRES + PRÉPARATION GLM
data_complete <- ventes %>%
  left_join(finances, by = c("Date","Magasin")) %>%
  left_join(ventes_moyennes_jour %>%
              select(Date, Magasin, Ratio_Competition),
            by = c("Date","Magasin"))

data_complete <- data_complete %>%
  mutate(
    Promotion_Binaire = ifelse(Promotion == "Oui", 1, 0),
    JourSemaine_Num = as.numeric(JourSemaine),
    Mois_Facteur = as.factor(Mois),
    Annee_Facteur = as.factor(Annee)
  ) %>%
  group_by(Magasin) %>%
  mutate(
    Chiffre_Affaires_Norm =
      (chiffre_affaires - mean(chiffre_affaires, na.rm = TRUE)) /
      sd(chiffre_affaires, na.rm = TRUE),
    Budget_Pub_Norm =
      (budget_publicite - mean(budget_publicite, na.rm = TRUE)) /
      sd(budget_publicite, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Saison = as.factor(case_when(
      Mois %in% c(3,4,5) ~ "Printemps",
      Mois %in% c(6,7,8) ~ "Été",
      Mois %in% c(9,10,11) ~ "Automne",
      TRUE ~ "Hiver"
    ))
  )

data_model <- data_complete %>%
  filter(!is.na(chiffre_affaires) &
           !is.na(budget_publicite) &
           !is.na(masse_salariale))

cat("Lignes prêtes pour la modélisation :", nrow(data_model), "\n")

##MODÉLISATION GLM
# La variable Quantité_Vendue est une variable de comptage,
# on utilise donc une régression de Poisson (lien log).
res_glm1 <- glm(
  Quantité_Vendue ~ Promotion_Binaire,
  data = data_model,
  family = poisson(link = "log")
)

res_glm2 <- glm(
  Quantité_Vendue ~ Promotion_Binaire + Type_Jour,
  data = data_model,
  family = poisson(link = "log")
)

res_glm3 <- glm(
  Quantité_Vendue ~ Promotion_Binaire + Type_Jour + Saison,
  data = data_model,
  family = poisson(link = "log")
)

res_glm4 <- glm(
  Quantité_Vendue ~ Promotion_Binaire + Type_Jour + Saison +
    Ratio_Competition,
  data = data_model,
  family = poisson(link = "log")
)

res_glm5 <- glm(
  Quantité_Vendue ~ Promotion_Binaire + Type_Jour + Saison +
    Ratio_Competition + Prix_Unitaire + JourSemaine,
  data = data_model,
  family = poisson(link = "log")
)

# Modèle avec interaction Promotion × Week-end (très intéressant économiquement)
res_glm6 <- glm(
  Quantité_Vendue ~ Promotion_Binaire * Type_Jour +
    Saison + Ratio_Competition + Prix_Unitaire,
  data = data_model,
  family = poisson(link = "log")
)

# Modèle le plus complet (avec finances normalisées)
res_glm7 <- glm(
  Quantité_Vendue ~ Promotion_Binaire * Type_Jour +
    Saison + Ratio_Competition +
    Prix_Unitaire +
    Budget_Pub_Norm + Chiffre_Affaires_Norm + masse_salariale,
  data = data_model,
  family = poisson(link = "log")
)

#Comparaison des modèles par AIC 
AIC_vec <- c(
  AIC(res_glm1),
  AIC(res_glm2),
  AIC(res_glm3),
  AIC(res_glm4),
  AIC(res_glm5),
  AIC(res_glm6),
  AIC(res_glm7)
)

noms_mod <- c(
  "Promotion",
  "+ Week-end",
  "+ Saison",
  "+ Compétition",
  "+ Prix & Jour",
  "+ Interaction",
  "+ Finances"
)

comparaison_aic <- data.frame(
  Modèle = noms_mod,
  AIC = AIC_vec,
  Delta_AIC = AIC_vec - min(AIC_vec)
) %>%
  arrange(AIC)

meilleur_idx <- which.min(AIC_vec)
meilleur_glm <- list(
  res_glm1, res_glm2, res_glm3,
  res_glm4, res_glm5, res_glm6, res_glm7
)[[meilleur_idx]]

print(comparaison_aic)
cat("\nModèle retenu :", noms_mod[meilleur_idx],
    " (AIC =", round(AIC_vec[meilleur_idx], 0), ")\n")

summary(meilleur_glm)

#Prédictions et qualité du modèle
data_model$Quantite_Predite <- predict(meilleur_glm, type = "response")
data_model$Residus <- data_model$Quantité_Vendue - data_model$Quantite_Predite

mae <- mean(abs(data_model$Quantite_Predite -
                  data_model$Quantité_Vendue), na.rm = TRUE)

cor_pred <- cor(
  data_model$Quantite_Predite,
  data_model$Quantité_Vendue,
  use = "complete.obs"
)

cat("MAE :", round(mae, 2), "\n")
cat("Corrélation prédictions/observations :",
    round(cor_pred, 4), "\n")

summary(data_model$Quantité_Vendue)

#Graphiques de qualité
#Prédictions vs Observations
bins_obs <- seq(
  0,
  max(data_model$Quantité_Vendue, na.rm = TRUE) * 1.01,
  length.out = 18
)

data_model$Tranche_Obs <- cut(
  data_model$Quantité_Vendue,
  breaks = bins_obs,
  include.lowest = TRUE
)

qualite_pred <- data_model %>%
  group_by(Tranche_Obs) %>%
  summarise(
    Ventes_Observees_Moy = mean(Quantité_Vendue, na.rm = TRUE),
    Ventes_Predites_Moy = mean(Quantite_Predite, na.rm = TRUE),
    .groups = "drop"
  )

p7 <- ggplot(qualite_pred,
              aes(x = Ventes_Observees_Moy,
                  y = Ventes_Predites_Moy)) +
  geom_point(size = 4, color = "steelblue", alpha = 0.9) +
  geom_line(linewidth = 1, color = "steelblue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Prédictions vs observations",
    subtitle = sprintf(
      "Corrélation = %s | MAE = %s",
      round(cor_pred, 3),
      round(mae, 2)
    ),
    x = "Ventes observées (moy. tranche)",
    y = "Ventes prédites (moy. tranche)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p7)

#Erreur moyenne par tranche de ventes prédites
bins_pred <- seq(
  0,
  max(data_model$Quantite_Predite, na.rm = TRUE) * 1.01,
  length.out = 15
)

data_model$Tranche_Pred <- cut(
  data_model$Quantite_Predite,
  breaks = bins_pred,
  include.lowest = TRUE
)

err_by_bin <- data_model %>%
  group_by(Tranche_Pred) %>%
  summarise(
    Vente_Predite_Moy = mean(Quantite_Predite, na.rm = TRUE),
    Erreur_Moyenne = mean(Residus, na.rm = TRUE),
    .groups = "drop"
  )

p8 <- ggplot(err_by_bin,
              aes(x = Vente_Predite_Moy, y = Erreur_Moyenne)) +
  geom_col(fill = "coral2", alpha = 0.85) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  labs(
    title = "Erreur du modèle par tranche",
    subtitle = "Barre > 0 : sous-estimation ; < 0 : surestimation",
    x = "Ventes prédites (moy. tranche)",
    y = "Erreur moyenne"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p8)

