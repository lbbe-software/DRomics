# TODO list

## Top priority
1. [ ] Faire en sorte que l'on puisse appliquer plot(f, items = "unseul", BMDoutput = bootstrapfaitjusteaveccetitem)
1. [ ] Find a way to give an example in the DRomicsInterpreter shiny app
1. [ ] Add an example in the vignette where the need is obvious to work on log scale for bmdplot, sensitivity plot AND change the default for log scale in each plot (fit or BMD) AND add a warning 
of this modification at the launch of the package ?
1. [ ] Add an explanation of minBMD and or an option to define it in Shiny
1. [ ] Gérer le souci des décimales dans le sensitivityplot (taille de points) à partir d'ex. Ellis (pas de souci vu par Aurélie) et faire un meilleur choix des valeurs à afficher (sur ech log si effectifs très diff)
1. [ ] dans les curvesplot en option ajouter un point là où la BMD est atteinte 
1. [ ] Visualisation optionnelle par lignes verticales des doses testées (tous les graphes avec la dose en x)
1. [ ] Ajouter dans vignette ou FAQ ex.d'utilisation de l'option "median.and.IQR"
1. [ ] Retravailler les test_that
1. [ ] Ajouter des colonnes à la sortie de DRomics via bmdboot (defined.BMD.zSD, finite.CI.BMD.zSD, ...)
1. [ ] Mettre l'option scaling par défaut à TRUE dans le package (comme c'est déjà fait dans l'appli shiny) et l'indiquer dans la vignette
1. [ ] Donnez la possibilité d'ajouter le nom de pathways à côté des points sur les bmdplot en ECDF
1. [ ] AJouter la publi PCI partout quand elle sera sortie
1. [ ] Reformuler le keep all experimental levels ?
1. [ ] Faire un outil de type diag de Venn (ou montrer dans doc) qui compare deux résultats de itemselect()


## Shorter term 

1. [ ] Changer le jeu de données exemple RNAseq, mettre un de Gwinn avec plus de concentrations
1. [ ] Proposer une figure par groupe avec médiane et intervalle interquartile, ou moyenne et 2 SD ? + indice de similarité de forme ? FAIT en partie dans sensitivityplot() - A COMPLETER ?
1. [ ] Transform sensitivityplot to return as an invisible object the numerical summaries and to do other plots (boxplots) - impossible - write a function sensitivitycalc that will be called internally by sensitivityplot
1. [ ] Ajouter une fonction et calcul de correlation non signée 2 à 2 des courbes fittées DR au sein de chaque groupe et un graphe associé (prototype in the share)
1. [ ] Ajouter une méthode de clustering type WGCNA basée sur cette pairwise unsigned correlation
1. [ ] Add Danio rerio data in the package as another example for the functions for Dromics results interpretation (without enrichment and/or with outlier - elimination as in the paper (IRSN) ou un autre ex. 
1. [ ] Ajouter des métriques en sortie de DRomics pour utilisation potentielle en interprétation (cf. M2 Ellis, …)
1. [ ] Ajouter deux sous-niveaux dans la vignette à laquelle on accède depuis GitHub
1. [ ] Réécrire avec plus de fonctions les appels à nls 
1. [ ] envisager d'utiliser une autre procédure plus performante (cf. pb de port décrit dans la doc de nls) 
1. [ ] Implémenter d'autres modèle d'erreur (données continues censurées et données binaires)
1. [ ] Donner la possibilité dans selectgroups de sélectionner sur une p(q)value d'enrichissement 


## Of less priority
1. [ ] Change the examples of DRomics data results (triclosan) to ass the column yatdosemax
1. [ ] Ajouter un test sur les outliers pour les cas excessifs cf. transcripto rainettes 2018 (implémenter sur chaque item, dès la vérification des données, sans prendre en compte la dose-réponse, une détection basée sur le Z-score modifié de Iglewicz, B., & Hoaglin, D. C. (1993). How to detect and handle outliers (Vol. 16). Asq Press. (1115 citations google scholar)) - cf. point 84 lié et réglé en partie par choix de la méthode de transfo à vst si nb samples > 30 (cf. Love)
1. [ ] Regarder si rlog et vst laissent les 0 à une valeur commune (ties)
1. [ ] Trouver un sous jeu de données exemple ou on arrive à calculer les IC pour des courbes probit
1. [ ] Tester sur de mauvais jeux de données la programmation défensive faite sur les fonctions d’importation des données
1. [ ] Ajouter une fonction proteomicdata avec ex. IRSN quand on aura un jeu de données avec assez de doses
1. [ ] Donner diverses options d’un plot de sensibilité par pathway (boxplot, autre stat résumées…) – fonction englobante sensitivityplot  avec plusieurs groupes (ex. article diuron) en résolvant le pb de l'ordre des items - FAIT EN PARTIE - A VALIDER PAR FLORIANE ET ELISE
1. [ ] Retravailler le modèle sigmoide, Lprobit et Gausprobit avec e = 0
1. [ ] Utilisation du vartrend pour alerte : pb avec microarray very small sample data A DISCUTER AVEC ELISE !
1. [ ] Essayer de mettre le filtre sur les résidus meantrend avant la sélection car parfois on doit enlever des items pour lesquels un des modèle allait. A DISCUTER AVCE ELISE !


## Longer term 
1. [X] Faire une deuxième appli shiny prenant en entrée l’annotation et une sortie de workflow
1. [ ] Calculer une BMD bis de l’autre côté du contrôle ? PAS SIMPLE NI A FAIRE NI A GERER ET QUELLE UTILITE ? PAS SUR QUE CE SOIT UNE BONNE IDEE
1. [ ] Programmation défensive si nb d’items trop petit sur le plot de sensibilité par groupe – mais quoi faire exactement ? Quantile fait de l’interpolation linéaire et sort un résultat quoiqu’il arrive
1. [ ] Regarder que faire avec des données de comptages de metabarcoding
1. [ ] Voir ce qu’on peut faire avec des données CT de RTQPCR (2ème phase après screening RNAseq) – mettre commentaire sur échelle (log delta Ct ?) sur sur normalisation avec les gènes de ménage.
1. [ ] Faire un boostrap avec modèle libre pour avoir notamment l'incertitude sur la trend -  pas raisonnable - plutôt faire du LOO et donner comme indice le % de fois ou la même trend est donnés on est content si c'est 100% - CA RESTE TRES COMPLIQUE A IMPLEMENTER !!!!!


## Evaluation of DRomics
1. [ ] Tester la robustesse des BMD à une dégradation du plan d’expérience
1. [ ] Reprendre un jeu de données de la littérature sur lequel une approche clustering a été utilisée et comparer avec notre approche
1. [ ] Tester FASTBMD et BMDExpress sur des jeux de données sans 0 et sans réplicat (OK sur DRomics)
