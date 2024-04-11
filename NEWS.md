# eneRgyVD 0.1.0

<style type="text/css", rel="stylsheet">
/* add custom css if needed */
</style>

## Suivi des modifications du profil énergétique

### Mise à jour janvier 2024 (à venir)

#### Chaleur des bâtiments 

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
