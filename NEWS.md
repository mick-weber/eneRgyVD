# eneRgyVD 1.1.0

* Ajout d'un jeu de données `regener_cons_ae_year` qui est la version simplifiée de `regener_cons_ae_use/aff` permettant un affichage en barplot par année, en complément des diagrammes alluviaux précédemment proposés

* Meilleure gestion des petits écrans (laptop, mobile, etc.) pour la sidebar ainsi que pour le groupement/déploiement de la barre de navigation principale selon la largeur d'écran disponible

* Meilleure intégration des scripts javascript dans des fichiers dédiés plutôt que dans le header HTML du fichier app_ui.R

# eneRgyVD 1.0.0

## Suivi des modifications du profil énergétique

### MàJ 2025-02-01

* Migration profil climatique (nouvelle UI)

* Ajout données canopée, dangers naturels, taux de motorisation, qualité de desserte TP, électrification des véhicules de tourisme

* Ajout données gaz énergie

### MàJ 2024-10-10

#### Distribution d'électricité

* Ajout données 2018-2022 (résultats de l'enquête 2024 au 2024-10-10)

### MàJ 2024-04-30

#### Chaleur des bâtiments 

* Correctif 30.04.2024 : modification des indices de besoin de chaleur avec un impact global de +2.5% de consommation pour le canton de Vaud : dû à un correctif du traitement des données de consommation au bâtiment (gaz/CAD) utilisées pour la génération des IBC

* Ajustement de la méthodologie RegEner et ajout de l'année 2023. L'année 2022 
change rétroactivement avec la nouvelle méthode.

  * Augmentation d'environ 4% de la SRE (et donc besoins et consommation) du à des
    correctifs mineurs et à l'amélioration de l'attribution de chauffages à des bâtiments
    dont on soupçonne fortement qu'il est chauffé par un bâtiment voisin (salles de sport, etc.)
  
  * Correctif de la variable `ZIE` : certains bâtiments étaient recensés selon l'ancienne
    législation OPair
  
  * Suppression des valeurs de SRE pour les bâtiments non chauffés (`AFF_SIA_1 = 0`) ce qui pouvait
    induire en erreur lors de certains calculs
    
  * Ajout de la variable `ETAT_REGENER` pour traçer l'année reflétée par le RegEner. Cela
    facilite notamment l'historisation des différentes années.

* Onglet `Autres` : ajout de la distinction entre rénovation légère et lourde

#### Production d'électricité

* Ajout données 2023 (juillet 2024)

* Ajustement mineur de la méthodologie impliquant des modifications rétroactives 
de l'estimation de production d'électricité photovoltaïque, ainsi que des modifications
négligeables pour les autres catégories

#### Distribution d'électricité

* Ajout de nouvelles données de distribution d'électricité. Les données proviennent
des GRD et le traitement est assuré par la DGE-DIREN. On ne parle pas de consommation car
l'autoconsommation (notamment photovoltaïque) n'est pas inclue dans la distribution.

#### Consommation de gaz naturel

* Ajout de nouvelles données de consommation de gaz naturel. Les données proviennent
des GRD et le traitement est assuré par la DGE-DIREN

#### Subventions

* Ajout de données de subvention des bâtiments pour l'isolation et le
remplacement des chauffages (Programme Bâtiments)
