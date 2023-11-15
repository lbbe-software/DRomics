# TODO list

## Top priority
1. [X] Retravailler les test_that (ML)
1. [ ] Add an example in the vignette where the need is obvious to work on log scale for bmdplot, sensitivity plot AND change the default for log scale in each plot (fit or BMD) AND add a warning (ML and A pour mise en place warning). Dans shiny appels à bmdplot(BMD_log_transfo = TRUE), bmdplotwithgradient(BMD_log_transfo = TRUE), sensitivityplot(BMD_log_transfo = TRUE), plot.drcfit(dose_log_transfo = TRUE), plotfit2pdf(dose_log_transfo = TRUE), targetplot(dose_log_transfo = TRUE), et nouvel argument BMD_log_transfo par défaut à TRUE à 
gérer dans les appels à plot.bmdcalc() (et plot.bmdboot() mais pas dans shiny). 
PAR CONTRE EN ATTENTE  car plus délicat à gérer - faudrait mettre une val par défaut à xmin : curvesplot(dose_log_transfo = TRUE)
1. [X] Mettre l'option scaling par défaut à TRUE dans le package (comme c'est déjà fait dans l'appli shiny) et l'indiquer dans la vignette (ML - still to include in the vignette)
1. [X] Mettre un message à l'ouverture du package (startupmessage) pour indiquer les options par défaut changées (ML - sent to Aurélie - A)
1. [X] Retravailler les xlab et ylab notamment mettre scaled signal ou scaled y si scaling dans curvesplot et dans bmdplotwithgradient dans légende scaled_signal (ML)
1. [X] Tenter d'ajouter en optionnel une transparence sur curvesplot(). Was already available. I just added more examples in ?curvesplot
1. [X] Ajouter dans vignette ou FAQ ex. d'utilisation de l'option "median.and.IQR" (ML)
1. [X] Faire un outil de type diag de Venn (ou montrer dans vignette pour ne pas dépendre du package utilisé) qui compare deux résultats de itemselect() (ML)
1. [ ] Find a way to give an example in the DRomicsInterpreter shiny app
of this modification at the launch of the package ? Ajouter un encart en haut de la page step 1, avec le lien vers les 4 fichiers qu'on utilise en formation (qui sont dans le package) et une petite explication (A et ML)
1. [ ] Ajouter un ou deux sous-niveaux dans la vignette à laquelle on accède depuis GitHub (A) IMPOSSIBLE à cause des configurations de pkgdown ! Restructurer la vignette pour que la navigation soit plus simple - en parallèle de la rédaction de la FAQ dans laquelle basculeront des morceaux de la vignette en faisant attention à maintenir les liens de l'article PCI
1. [X] Add an explanation of minBMD in the vignette, in ?bmdcalcl and in the step 4 of Shiny app. 1 (ML)
1. [X] Gérer le souci des décimales dans le sensitivityplot (taille de points) : faire un meilleur choix des valeurs à afficher (sur ech log si effectifs très diff) (ML)
1. [X] Dans les curvesplot, en option, ajouter un point là où la BMD est atteinte (ML - still to add an example in the vignette)
1. [ ] Ajouter des colonnes à la sortie de DRomics via bmdboot (defined.BMD.zSD, finite.CI.BMD.zSD, ...) (ML) Ellis et Sophie y sont favorables mais je diffère car ça m'embête de les ajouter et de ne pas les prendre en compte dans selectgroups(), alors qu'en pratique il me paraît nécessaire de faire a minima une sélection sur BMDdefined. A REDISCUTER !!!!!!!!!!!!!!!!
1. [X] Ajouter la publi PCI partout quand elle sera sortie (ML et A)
1. [X] Mettre un bouton d'aide i à côté du keep all experimental levels dans shiny (appli DRomicsInterpreter, step 2, helplabel2step2 dans global.R) (ML, A)
1. [ ] Faire en sorte que l'on puisse appliquer plot(f, items = "unseul", BMDoutput = bootstrapfaitjusteaveccetitem) (ML)
1. [X] Visualisation optionnelle par lignes verticales des doses testées (ajout ex. dans la vignette) (ML)
1. [X] Donnez la possibilité d'ajouter le nom de pathways à côté des points sur les sensitivity pour un seul niv exp plutôt que sur l'axe des y (en alternative) - mettre un code exemple dans la vignette car trop lourd à gérer dans la fonction. (A)
1. [ ] Add Danio rerio data in the package as another example for the functions for Dromics results interpretation (with or without enrichment and/or with outlier - elimination as in the paper (IRSN) ou un autre ex. (ML)
1. [X] Mettre sur le share un fichier de test sur des gros jeux de données, à tester sur une VM de l'IFB de temps en temps, avec fichiers stockés ailleurs, pour le moment sur SeaFile (ML)
1. [ ] Ajouter des arguments line.alpha et line.size et point.alpha à sensitivityplot(ML)
1. [ ] Ajouter des options "boxplot" dans BMDsummary de sensitivityplot(A)
1. [ ] dans les applis shiny et la vignette enlever les fonds gris avec un +theme_bw() quand le theme n'est pas défini dans la fonction (ML puis A)
1. [ ] Ajouter une fonction selectitems permettant de filtrer les sorties de DRomics notamment sur la base des résultats du bootstrap (par défaut on ne garde 
que les items avec BMD et IC de la BMD définis, pour la BMD-zSD par défaut) - penser à faire de la prog défensive si les utilisateurs ne mettent pas le bootstrap, filtre possible sur autre chose.... (ML)
1. [ ] revoir l'argument remove.infinite de plot.bmdboot, sa valeur par défaut et son fonctionnement, pour que ce soit harmonisé avec les valeurs par défaut 
de selectitems (pb avec xfold, vérifier que ça marche bien) ou enlever cette fonctionnalité et considérer que c'est fait avec selectitems et quand ce n'est aps fait gérer graphiquement les infinis
1. [ ] Suite a explorations d'Ellis, envisager la sortie des curvesplot en format
plotly, avec ajout d'un argument étiquette des points différent de l'id 
de la courbe si possible (pour cas plusieurs courbes avec même gène id mais pas même id de base - transcript)

## Shorter term 

1. [ ] Changer le jeu de données exemple RNAseq, mettre un de Gwinn avec plus de concentrations (ML)
1. [ ] Proposer des alternatives à sensitivityplot (moyenne et 2 SD, boxplot, + indice de similarité de forme) (ML)
1. [ ] Transform sensitivityplot to return as an invisible object the numerical summaries and to do other plots (boxplots) - impossible - write a function sensitivitycalc that will be called internally by sensitivityplot (ML)
1. [ ] Ajouter une fonction et calcul de correlation non signée 2 à 2 des courbes fittées DR au sein de chaque groupe et un graphe associé (prototype in the share) (ML)
1. [ ] Ajouter une méthode de clustering type WGCNA basée sur cette pairwise unsigned correlation (ML)
1. [ ] Ajouter des métriques en sortie de DRomics pour utilisation potentielle en interprétation (cf. M2 Ellis, …)
1. [ ] Réécrire avec plus de fonctions les appels à nls (ML)
1. [ ] Envisager d'utiliser une autre procédure plus performante (cf. pb de port décrit dans la doc de nls) (ML)
1. [ ] Implémenter d'autres modèle d'erreur (données continues censurées et données binaires) (ML)
1. [ ] Démarrer une FAQ (ML et Elise)
1. [ ] Programmation défensive si nb d’items trop petit sur le plot de sensibilité par groupe – mais quoi faire exactement ? Quantile fait de l’interpolation linéaire et sort un résultat quoiqu’il arrive - mettre juste un warning


## Of less priority
1. [ ] Change the examples of DRomics data results (triclosan) to add the column yatdosemax
1. [X] Ajouter un test sur les outliers pour les cas excessifs cf. transcripto rainettes 2018 (implémenter sur chaque item, dès la vérification des données, sans prendre en compte la dose-réponse, une détection basée sur le Z-score modifié de Iglewicz, B., & Hoaglin, D. C. (1993). How to detect and handle outliers (Vol. 16). Asq Press. (1115 citations google scholar)) - cf. point 84 lié et réglé en partie par choix de la méthode de transfo à vst si nb samples > 30 (cf. Love)
1. [ ] Regarder si rlog et vst laissent les 0 à une valeur commune (ties)
1. [ ] Trouver un sous jeu de données exemple ou on arrive à calculer les IC pour des courbes probit - option enlevée - à vérifier ?
1. [ ] Tester sur de mauvais jeux de données la programmation défensive faite sur les fonctions d’importation des données
1. [ ] Ajouter un jeu de données protéomiques (IRSN) dans le package et en exemple
1. [ ] Retravailler le modèle sigmoide, Lprobit et Gausprobit avec e = 0
1. [ ] Utilisation du vartrend pour alerte : pb avec microarray very small sample data A DISCUTER AVEC ELISE !
1. [ ] Essayer de mettre le filtre sur les résidus meantrend avant la sélection car parfois on doit enlever des items pour lesquels un des modèle allait. A DISCUTER AVCE ELISE !
1. [ ] Donner la possibilité dans selectgroups de sélectionner sur une p(q)value d'enrichissement (ML) - Elise et Sophie trouve cela compliqué à expliquer


## Longer term 
1. [X] Faire une deuxième appli shiny prenant en entrée l’annotation et une sortie de workflow
1. [ ] Calculer une BMD bis de l’autre côté du contrôle ? PAS SIMPLE NI A FAIRE NI A GERER ET QUELLE UTILITE ? PAS SUR QUE CE SOIT UNE BONNE IDEE
1. [ ] Regarder que faire avec des données de comptages de metabarcoding
1. [ ] Voir ce qu’on peut faire avec des données CT de RTQPCR (2ème phase après screening RNAseq) – mettre commentaire sur échelle (log delta Ct ?) sur sur normalisation avec les gènes de ménage.
1. [ ] Faire un boostrap avec modèle libre pour avoir notamment l'incertitude sur la trend -  pas raisonnable - plutôt faire du LOO et donner comme indice le % de fois ou la même trend est donnés on est content si c'est 100% - CA RESTE TRES COMPLIQUE A IMPLEMENTER !!!!!


## Evaluation of DRomics
1. [ ] Tester la robustesse des BMD à une dégradation du plan d’expérience
1. [ ] Reprendre un jeu de données de la littérature sur lequel une approche clustering a été utilisée et comparer avec notre approche
1. [ ] Tester FASTBMD et BMDExpress sur des jeux de données sans 0 et sans réplicat (OK sur DRomics)
