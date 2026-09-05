# Analyse de la volatilité et des corrélations entre actifs financiers (Immobilier, Énergies Renouvelables, MSCI World)

## Contexte et objectif
Projet réalisé en groupe (3 personnes) dans le cadre du Master, portant sur l'étude des dynamiques de volatilité et de dépendance entre trois classes d'actifs (ETF) : le MSCI World, l'immobilier et les énergies renouvelables, avant et après la crise du COVID-19.

**Contribution personnelle (Bintou Daouda Garango)** : partie analytique — interprétation des modèles, analyse des résidus, comparaison des actifs, rédaction des conclusions, organisation des sections, relecture et préparation du rendu final.

## Méthodologie
- Modélisation de la volatilité par modèles **GARCH**
- Estimation des corrélations conditionnelles dynamiques via des modèles **BEKK(1,1)**
- Analyse des interdépendances par modèle **VAR** (fonctions de réponse aux impulsions, tests de causalité de Granger, décomposition de la variance - FEVD)
- Outil : **R**

## Résultats clés
- Les rendements sont globalement non autocorrélés, mais leurs variances montrent une forte persistance, justifiant l'usage des modèles GARCH.
- La corrélation MSCI World / immobilier passe d'environ 0,385 avant 2020 à 0,625 après 2020 : la hausse des taux post-pandémie a créé une nouvelle dépendance structurelle entre ces deux classes d'actifs.
- La corrélation MSCI World / énergies renouvelables (≈0,57 en moyenne) devient beaucoup plus volatile après 2020 (entre 0,2 et 1,0), rendant les bénéfices de diversification de ce secteur difficiles à anticiper.
- Le MSCI World agit comme générateur dominant de chocs sur le système (confirmé par Granger et le FEVD, où il explique plus de 95 % de sa propre variance), tandis que l'immobilier dépend à 45 % des chocs venus du marché mondial, une proportion stable dans le temps.
- **Implication pratique** : la diversification traditionnelle MSCI World / immobilier, efficace en période calme, perd de son intérêt depuis 2020 ; les énergies renouvelables restent une piste de diversification à long terme, sous réserve d'accepter une forte volatilité liée aux politiques énergétiques et aux cycles géopolitiques.

## Contenu du dépôt
- Script R (analyse et modélisation)
- Rapport complet (PDF)

## Conclusion
Ce projet illustre l'intérêt des modèles de volatilité conditionnelle et de dépendance dynamique pour la gestion de portefeuille, en montrant que les corrélations entre actifs ne sont pas stables dans le temps et doivent être réévaluées après un choc macroéconomique majeur.
