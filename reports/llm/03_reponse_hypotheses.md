Les tableaux soutiennent une modélisation des **log-prix avec une première différenciation** et des **rendements sans différenciation supplémentaire**. Ils motivent aussi l’étude de distributions à queues épaisses. Ils ne suffisent pas à choisir les ordres des modèles ni à établir la présence d’effets GARCH.

### 1. Hypothèses de modèles à tester

| Composante | Candidats | Justification |
|---|---|---|
| Moyenne univariée | ARIMA(0,1,0) sur les log-prix, avec et sans dérive | Référence simple compatible avec les résultats de stationnarité |
| Dépendance des rendements | Petits modèles ARMA(p,q), équivalents à des ARIMA(p,1,q) des log-prix | À tester si les ACF/PACF révèlent une dépendance temporelle |
| Moyenne multivariée | VAR sur les rendements de GLD et SLV | Permet de tester une information prédictive croisée, non établie par les tableaux |
| Relation de long terme | VECM, uniquement si une cointégration est détectée | La non-stationnarité individuelle ne démontre pas une relation d’équilibre |
| Volatilité | GARCH(1,1) avec innovations de Student, si des effets ARCH sont détectés | Les queues épaisses rendent la Student pertinente comme candidate |
| Volatilité asymétrique | GJR-GARCH ou EGARCH | À envisager si les diagnostics montrent une réponse différente aux chocs positifs et négatifs |

Une moyenne empirique positive ne démontre pas une dérive significative. De même, l’asymétrie négative des rendements **ne prouve pas une asymétrie de la volatilité conditionnelle**.

### 2. Diagnostics nécessaires

- **ARIMA :** examiner ACF/PACF des rendements, comparer des ordres modestes par AIC/BIC, vérifier la convergence, puis tester les résidus avec Ljung–Box, Jarque–Bera et ARCH-LM.
- **SARIMA :** rechercher d’abord une saisonnalité reproductible, par graphiques, décomposition et autocorrélations aux retards pertinents. Une semaine de cinq séances ne suffit pas à justifier une saisonnalité.
- **VAR :** vérifier l’alignement des séries, sélectionner les retards par critères d’information et contrôler la stabilité lorsque le modèle comporte des retards. Examiner la blancheur multivariée, la normalité et les effets ARCH des résidus. Une causalité de Granger indique une dépendance prédictive, pas une causalité économique.
- **VECM :** confirmer que les séries sont compatibles avec une intégration I(1), tester la cointégration avec une spécification déterministe justifiée et vérifier la sensibilité aux retards et aux ruptures.
- **GARCH :** examiner les rendements ou résidus au carré et appliquer ARCH-LM avant estimation. Après estimation, vérifier convergence, contraintes des paramètres et diagnostics des résidus standardisés et de leurs carrés. Évaluer également les prévisions de variance ou la couverture des intervalles hors échantillon.

Les fortes valeurs extrêmes doivent être vérifiées avant toute correction ; elles peuvent représenter des mouvements authentiques.

### 3. Comparaison de LSTM et GRU sans fuite d’information

Conserver les derniers 10 % comme test final et effectuer les choix d’architecture uniquement dans les premiers 90 %, avec une validation temporelle à historique croissant.

Pour chaque découpage :

1. Ajuster le scaler uniquement sur les observations utilisées pour entraîner le réseau.
2. Construire chaque fenêtre avec des observations antérieures à sa cible.
3. Utiliser une validation interne pour l’arrêt anticipé et un bloc ultérieur distinct pour l’évaluation.
4. Réinitialiser les poids des réseaux entre les découpages.

Comparer tous les modèles sur les **mêmes origines, cibles et horizons**, avec un protocole identique d’actualisation des observations et de réestimation des paramètres. Pour une prévision récursive à plusieurs séances, réinjecter les prévisions plutôt que les rendements futurs réels.

Inclure une référence de rendement nul, correspondant à un prix inchangé. Calculer MSE, RMSE et MAE sur les rendements ou les prix, en précisant l’unité ; réserver de préférence la MAPE aux prix positifs, car elle devient instable pour des rendements proches de zéro. Documenter le coût de calcul et la sensibilité aux graines aléatoires. Un test de Diebold–Mariano doit tenir compte du chevauchement des horizons.

### 4. Conclusions établies et hypothèses restantes

**Les tableaux montrent :**

- Une dispersion plus élevée pour SLV : son écart-type quotidien est de 1,820814 %, contre 0,977263 % pour GLD.
- Une asymétrie empirique négative et un excès de kurtosis positif pour les deux actifs.
- Des résultats ADF et KPSS concordants avec des log-prix non stationnaires et des rendements stationnaires. Cela soutient une intégration I(1) des log-prix, sans constituer une preuve définitive.

Les p-values ADF affichées à `0,000000` sont arrondies. Les valeurs KPSS de `0,01` et `0,10` peuvent correspondre aux bornes des tables utilisées ; elles ne doivent pas être interprétées comme des probabilités exactes.

**Restent à vérifier :** les ordres AR/MA, une éventuelle saisonnalité, les effets ARCH, l’asymétrie conditionnelle, la cointégration, les relations prédictives croisées et la supériorité des réseaux. Les tableaux descriptifs et de stationnarité ne permettent pas, à eux seuls, de conclure sur ces points.