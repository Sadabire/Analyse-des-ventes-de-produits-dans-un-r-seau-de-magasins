# Analyse-des-ventes-de-produits-dans-un-r-seau-de-magasins
Projet pour la SAE BUT 3 science de données

```markdown
# 📊 Analyse des ventes — Projet SAE 5 (Modélisation statistique avancée)

## 🎯 Objectif du projet
Analyser les ventes de produits alimentaires dans un réseau de magasins (2022–2024) afin de :
- comprendre les tendances des ventes,
- identifier les facteurs influençant la demande,
- modéliser statistiquement les ventes,
- formuler des recommandations opérationnelles.

## 📂 Données utilisées
Deux bases de données principales :
- **Ventes** : produit, magasin, date, promotion, prix unitaire, quantité vendue  
- **Finances** : chiffre d’affaires, budget publicité, masse salariale, dépenses

## 🛠️ Méthodologie
1. **Nettoyage des données**
   - Traitement des valeurs manquantes (imputation par médiane)
   - Détection des valeurs atypiques (IQR)

2. **Analyse descriptive**
   - Évolution des ventes dans le temps (forte saisonnalité)
   - Effet des promotions (ventes plus élevées)
   - Différence semaine vs week-end (week-end plus favorable)
   - Indicateur de compétition entre magasins

3. **Modélisation**
   - Régression de **Poisson (GLM)**
   - Variables : promotion, week-end, saison, compétition, prix, variables financières
   - Sélection du meilleur modèle via **AIC**

## 🔍 Résultats clés
- Les promotions augmentent significativement les ventes.
- L’effet est encore plus fort le week-end.
- La saisonnalité joue un rôle important.
- Les magasins mieux dotés financièrement performent mieux.
- Le modèle est pertinent pour l’analyse explicative (moins précis pour prévisions très fines).

## 💡 Recommandations
- Cibler davantage les promotions le week-end.
- Adapter les stocks selon la saisonnalité.
- Diffuser les bonnes pratiques des magasins les plus performants.
- Optimiser l’allocation des ressources financières.

## 👥 Membres du groupe
- Habibath BELLO  
- Batté Naïmatou KONATE  
- Saânbèterfaa Joël DABIRE  
- Sokhna Awa Bousso SYLLA  

Encadrante : Mme SOHIER Emilie
```
