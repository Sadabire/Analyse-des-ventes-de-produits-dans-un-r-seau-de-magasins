# Analyse des ventes dans un réseau de magasins — Modélisation statistique avancée

Projet d'analyse statistique des ventes de produits alimentaires dans un réseau de six magasins sur la période 2022–2024. L'objectif est d'identifier les facteurs influençant les quantités vendues et de formuler des recommandations opérationnelles à partir d'une modélisation par régression de Poisson.

Ce travail a été réalisé dans le cadre de la **SAE 5.EMS.01** à l'**IUT Clermont Auvergne — Université Clermont Auvergne**, sous la direction de Mme Emilie SOHIER.

---

## Contexte et problématique

Dans un secteur de distribution alimentaire où la concurrence entre enseignes est forte, comprendre ce qui fait vendre — ou pas — est un enjeu stratégique réel. Ce projet part de deux bases de données : une base de ventes (produit, magasin, date, promotion, prix, quantité vendue) et une base financière par magasin (chiffre d'affaires, budget publicité, masse salariale, dépenses de fonctionnement).

La question centrale est la suivante : quels facteurs — promotions, saisonnalité, type de jour, compétition entre magasins, ressources financières — expliquent le mieux les variations de quantité vendue ?

---

## Données

- **Volume** : 40 860 observations sur 6 magasins et 10 produits
- **Période** : 2022 à 2024
- **Variables** : produit, magasin, date, promotion (binaire), prix unitaire, quantité vendue, indicateurs financiers par magasin
- **Valeurs manquantes** : 2 040 observations sans quantité vendue (4,99%) — imputées par médiane hiérarchique (produit × magasin × jour de semaine)

---

## Méthodologie

### Prétraitement

- Diagnostic et imputation des valeurs manquantes par médiane hiérarchique à quatre niveaux
- Création de variables temporelles : année, mois, saison, jour de semaine, indicateur week-end
- Détection des valeurs atypiques par la méthode IQR — 758 observations identifiées (1,86%), conservées car correspondant à des pics de ventes réels
- Construction d'un **ratio de compétition** entre magasins : ventes d'un magasin / moyenne des autres magasins le même jour

### Analyse descriptive

Principaux résultats :

- Le Pain et le Lait sont les produits les plus vendus en moyenne (77,7 et 73,4 unités)
- Le Magasin_6 est le plus performant (73 unités/jour en moyenne), le Magasin_3 le moins performant (55,7)
- Les ventes progressent d'année en année : 60,6 en 2022, 66,6 en 2023, 73,5 en 2024
- **Effet promotion** : 62 unités sans promotion → 92 unités avec promotion (+48%)
- **Effet week-end** : 61 unités en semaine → 74 unités le week-end (+21%)
- **Saisonnalité** : creux en été (août-octobre), pics en début d'année et au printemps

### Modélisation statistique

La variable cible étant une variable de comptage (entiers positifs), un **GLM de Poisson avec lien logarithmique** a été choisi. Sept modèles ont été construits en ajoutant progressivement des variables explicatives, puis comparés via le critère AIC.

| Modèle | AIC | Delta AIC |
|---|---|---|
| + Variables financières | 813 320 | 0 |
| + Interaction promotion × week-end | 821 492 | 8 172 |
| + Prix & Jour de semaine | 821 496 | 8 176 |
| + Ratio de compétition | 821 773 | 8 453 |
| + Saison | 848 186 | 34 866 |
| + Week-end | 998 963 | 185 643 |
| Promotion seule | 1 017 860 | 204 540 |

Le modèle retenu intègre : promotion, week-end, saison, ratio de compétition, prix unitaire, budget publicité, chiffre d'affaires, masse salariale et l'interaction promotion × week-end.

### Résultats du modèle retenu

Tous les coefficients sont statistiquement significatifs (p < 0,05) :

- La **promotion** augmente les ventes de manière significative
- Les ventes sont plus élevées au **printemps et en hiver** qu'en automne
- Le **ratio de compétition** a l'effet le plus fort : un magasin qui surperforme ses concurrents vend davantage
- Les **variables financières** (CA, masse salariale, budget pub) ont toutes un effet positif
- L'**interaction promotion × week-end** est significative : l'effet des promotions est amplifié le week-end

### Qualité du modèle

- **MAE** : 24,78 unités — le modèle capture les tendances générales mais reste imprécis au niveau individuel
- **Corrélation prédictions / observations** : 0,543 — relation positive mais modérée
- Le modèle est plus adapté à l'**analyse explicative** qu'à la prévision opérationnelle fine

---

## Recommandations

- Cibler les promotions en priorité **le week-end** où leur effet est amplifié
- Adapter les stocks aux **variations saisonnières** — renforcer les approvisionnements au printemps et en hiver
- Diffuser les bonnes pratiques des **magasins les plus performants** (Magasin_2 et Magasin_6) aux plus faibles
- Allouer davantage de ressources aux magasins à fort potentiel de croissance

---

## Technologies utilisées

- **Langage** : R
- **Méthodes** : GLM Poisson, critère AIC, imputation par médiane hiérarchique, détection IQR, analyse saisonnière
- **Visualisation** : ggplot2 — séries temporelles, tendances saisonnières, prédictions vs observations

---

## Modalités du projet

| Élément | Détail |
|---|---|
| Type | Projet académique — SAE 5.EMS.01 |
| Module | Modélisation statistique avancée |
| Établissement | IUT Clermont Auvergne — Université Clermont Auvergne |
| Restitution | Rapport écrit |
| Mode de travail | Groupe de 4 étudiants |
| Année | 2025–2026 |

---

## Compétences mobilisées

- Prétraitement de données — imputation hiérarchique, détection d'anomalies IQR
- Analyse exploratoire et statistique descriptive — saisonnalité, effets promotionnels, compétition
- Modélisation statistique avancée — GLM Poisson, sélection de modèles par AIC
- Construction d'indicateurs métier — ratio de compétition, interaction entre variables
- Évaluation de modèles — MAE, corrélation, graphiques de résidus
- Formulation de recommandations opérationnelles à partir de résultats statistiques

---

## Mots-clés

`GLM` `Poisson` `Modélisation statistique` `R` `Analyse des ventes` `Saisonnalité` `AIC` `Régression` `Data Mining` `SAE`
