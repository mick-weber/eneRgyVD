# This file stores all the long text sentences to display in each thematic module
# This makes utils_helpers.R easier to navigate (these were initially stored in it)
# !RULE! : these objects should NOT be used in utils_helpers.R since they are not sourced there !

## Require disclaimers ----

### Sentence related to communes selection ----

# when 0 commune is selected for an output display request :
req_communes_phrase <- "Sélectionner au moins une commune pour générer un résultat"

# when 1+ commune is selected but not available in the dataset :
req_communes_not_available <- "Aucune donnée n'est disponible pour la sélection actuelle"


## Methodological disclaimers ----
# Used in all methodological accordions in each thematic module

generic_method_warning <- "La méthode utilisée pour obtenir ces résultats est sujette à amélioration au fil des années. Pour en savoir plus sur cette donnée, cliquez sur le boutton"

specific_elec_warning <- "Si des données sont visiblement manquantes ou erronées, merci de prendre contact afin qu'une évaluation du problème puisse être faite.
Les données par secteurs sont spécialement dépendantes de la qualité de l'information fournie par les distributeurs."

specific_ng_warning <- "Si des données sont visiblement manquantes ou erronées, merci de prendre contact afin qu'une évaluation du problème puisse être faite.
Les données par secteurs sont spécialement dépendantes de la qualité de l'information fournie par les distributeurs."

specific_rgr_warning <- "Ces données dépendent notamment de la qualité de l'information qui figure dans les registres cantonal et fédéral des bâtiments, en particulier pour les agents énergétiques.
La DGE-DIREN se rend disponible pour accompagner des communes qui souhaiteraient procéder à une amélioration des données énergétiques figurant dans le registre."

specific_subsidies_warning <- "Ces données sont issues d'un traitement des données du Programme bâtiments et concernent exclusivement les subventions versées (avec achèvement de travaux).
Les subventions promises pour lesquels les travaux n'ont pas été effectués ne sont donc pas comptabilisées."



## Links and mail ----
# Used for mod_about_the_app.R and/or contact notificationMenu in ui.R

mail_address <- "pecc@vd.ch"
link_diren <- "https://www.vd.ch/djes/dge/dge-diren"
link_dge <- "https://www.vd.ch/toutes-les-autorites/departements/departement-de-la-jeunesse-de-lenvironnement-et-de-la-securite-djes/direction-generale-de-lenvironnement-dge/"
link_pecc <- "https://www.vd.ch/pecc" # shortcut link redirecting to https://www.vd.ch/etat-droit-finances/communes/climat-et-durabilite/plan-energie-et-climat-communal-pecc
link_ocdc <- "https://www.vd.ch/dfa/office-cantonal-de-la-durabilite-et-du-climat-ocdc"
link_github <- "https://github.com/mick-weber/eneRgyVD"

### Geoportail links ----

## Title complements (when useful) ----
### Energie ----


title_complement_elec_cons <- shiny::tags$p(style = "width:70vw;font-size:1.1rem;",
                                     shiny::HTML("Ces données sont issues des relevés des compteurs fournis par les gestionnaires de réseau de distribution (voir détails méthodologiques).
                                          <br>
                                          La <strong>distribution</strong> ne doit pas être confondue à la <strong>consommation finale</strong> car il manque l'autoconsommation (notamment photovoltaïque) ou encore l'électricité du réseau des CFF.
                                          L'autoconsommation estimée est disponible dans la <strong>table des données de production d'électricité</strong>."))

title_complement_elec_prod <- shiny::tags$p(style = "width:70vw;font-size:1.1rem;",
                                     shiny::HTML("Ces données sont le résultat d'un traitement des données issues de l'organisme de certification des garanties d'origine <a href = 'https://pronovo.ch/fr/'> Pronovo</a> (voir détails méthodologiques).
                                                 La production comprend une <strong>estimation de l'autoconsommation</strong>, informations disponibles dans la table des données."))

title_complement_ng_cons <- shiny::tags$p(style = "width:70vw;font-size:1.1rem;",
                                   shiny::HTML("Ces données sont issues des relevés des compteurs fournis par les gestionnaires de réseau de distribution. Elles ne
                                   comprennent pas les livraisons de gaz naturel hors-réseau, par exemple via des citernes remplies par transport routier.
                                        <br>
                                        L'exactitude des distinctions sectorielles ainsi que certaines variations interannuelles peuvent s'expliquer par la qualité des données fournies (voir détails méthodologiques).
                                               La catégorie <i>Indéfini</i> correspond à une quantité qui n'a pas pu être catégorisée dans <i>Ménages</i> ou <i>Hors-ménages</i>."))

title_complement_regener_needs <- shiny::tags$p(style = "width:70vw;font-size:1rem;",
                                         "Ces données illustrent la répartition des besoins énergétiques théoriques (et corrigés climatiquement) pour la chaleur des bâtiments, soit l'eau chaude sanitaire et chauffage des locaux.",
                                         shiny::strong("Ne sont pas compris la chaleur des procédés industriels et l'électricité pour un usage autre que calorifique."),
                                         "Pour plus d'informations, notamment sur les besoins optimisés, voir les détails méthodologiques. ",
                                         "La qualité des données s'améliorant d'année en année, certaines ruptures sont inévitables et peuvent expliquer des évolutions contre-intuitives vis-à-vis de l'assainissement du parc immobilier.
                                          Pour cette raison, il est recommandé de n'utiliser que la dernière année disponible, et d'exploiter les années antérieures avec précaution uniquement lorsque nécessaire."
                                         #create_geoportail_tag(link = 'https://www.geo.vd.ch', text = 'geo.vd.ch')
                                         )

title_complement_regener_cons <-  shiny::tags$p(style = "width:70vw;font-size:1rem;",
                                         "Ces données illustrent comment la consommation de différents agents énergétiques se répartit pour satisfaire les besoins en chaleur du bâtiment (chauffage et eau chaude sanitaire) selon l'usage ou l'affectation principale des bâtiments.",
                                         shiny::strong("La chaleur de procédés et l'électricité pour un usage autre que calorifique ne sont pas compris."),
                                         "Il s'agit d'estimations théoriques (et corrigées climatiquement) fondées sur des données empiriques. Les communes jouent notamment un rôle central pour garantir que les données reflètent bien la réalité des agents énergétiques en vigueur.",
                                         "La qualité des données s'améliorant d'année en année, certaines ruptures sont inévitables et peuvent expliquer des évolutions contre-intuitives vis-à-vis de l'assainissement du parc immobilier.
                                          Pour cette raison, il est recommandé de n'utiliser que la dernière année disponible, et d'exploiter les années antérieures avec précaution uniquement lorsque nécessaire."
                                         #create_geoportail_tag(link = 'https://www.geo.vd.ch', text = 'geo.vd.ch')
                                         )

title_complement_regener_misc <- shiny::tags$p(style = "width:70vw;font-size:1.1rem;",
                                        "Ces données reflètent quelques estimations structurelles du parc immobilier chauffé de chaque commune. La qualité des données s'améliorant d'année en année, certaines ruptures sont inévitables et peuvent expliquer des évolutions contre-intuitives vis-à-vis de l'assainissement du parc immobilier.
                                        Pour cette raison, il est recommandé de n'utiliser que la dernière année disponible, et d'exploiter les années antérieures avec précaution uniquement lorsque nécessaire.",
                                        shiny::br(),
                                        #create_geoportail_tag(link = 'https://www.geo.vd.ch', text = 'geo.vd.ch')
                                        )

title_complement_subsidies_building <- shiny::tags$p(style = "width:70vw;font-size:1rem;",
                                              "Ces données illustrent le nombre cumulé de bâtiments ayant reçu des subventions du Programme Bâtiment vaudois depuis 2017.
                                         Les données précédant 2017 ne sont pas inclues mais représentent une part négligeable des subventions versées jusqu'à aujourd'hui.",
                                              shiny::strong("L'état à la fin de chaque année est présenté, en cumulant les subventions des années précédentes."),
                                              "Le total des subventions versées d'une année ne peut donc pas être inférieur au total de l'année précédente.
                                         La SRE correspond à la surface de référence énergétique estimée des bâtiments ayant reçu une subvention.
                                         Pour simplifier, le terme 'chauffage renouvelable' englobe également les pompes à chaleur (PAC) et le chauffage à distance (CAD).")

title_complement_subsidies_measure <- shiny::tags$p(style = "width:70vw;font-size:1.1rem;",
                                             "Ces données illustrent le nombre annuel de bâtiments ayant fait l'objet d'une subvention payée, par type et année depuis 2017.
                                             Un bâtiment peut réapparaître plusieurs fois au fil des années s'il reçoit de nouvelles subventions",
                                             shiny::strong("il ne faut donc pas interpréter un bâtiment subventionné comme un bâtiment systématiquement nouvellement subventionné."),
                                             "Une vision cumulée par bâtiments subventionnés est disponible dans l'onglet adjacent.")



### Mobilité ----

title_complement_part_voit_elec <- shiny::tags$p(style = "width:70vw;font-size:1.1rem;",
                                                 "La part de voitures électriques dans le canton de Vaud mesure la proportion de voitures entièrement électriques (BEV)
parmi l'ensemble du parc. Cet indicateur suit l'évolution de la motorisation décarbonnée de la population vaudoise.",
                                                 tags$br(),
                                                 create_geoportail_tag(link = 'https://cartostatdgmr.vd.ch/#c=indicator&i=carbu_ofs.part_elec&s=2023&view=map1',
                                                                       text = 'cartostatdgmr.vd.ch')
)

title_complement_taux_motorisation <- shiny::tags$p(style = "width:70vw;font-size:1.1rem;",
                                                    "Le taux de motorisation mesure le nombre de voitures de tourisme pour 1’000 habitants. Cet indicateur reflète l’évolution du parc automobile des Vaudois.
Pour l'interprétation des données, une comparaison avec la moyenne cantonale ou avec des communes au profil similaire peut être réalisée.",
                                                    tags$br(),
                                                    create_geoportail_tag(link = 'https://cartostatdgmr.vd.ch/#c=indicator&i=voitures.voit_1000hab&s=2023&view=map1',
                                                                          text = 'cartostatdgmr.vd.ch')
)

title_complement_qualite_desserte <- shiny::tags$p(style = "width:70vw;font-size:1.1rem;",
                                                   "L'indice synthétique de la qualité de desserte de la population ou des emplois par les transports publics mesure la fréquence des transport situés respectivement à proximité du domicile ou du lieu de travail. Cet indice est exprimé en pourcentage : 100 % représentant une desserte optimale où l'ensemble des habitants ou des emplois se situent dans des zones très bien desservies.
Pour l'interprétation des données, une comparaison avec la moyenne cantonale ou avec des communes au profil similaire peut être réalisée.",
                                                   tags$br(),
                                                   create_geoportail_tag(link = 'https://cartostatdgmr.vd.ch/#c=indicator&i=qdtp_pop.pop_qdtp_is&s=2024&view=map1',
                                                                         text = 'cartostatdgmr.vd.ch')
)

### Adaptation ----

title_complement_surface_canopee <- shiny::tags$p(style = "width:70vw;font-size:1.1rem;",
                                                  "Cet indicateur permet de chiffrer la part d'espace couvert par une végétation arborée de plus de 3m de hauteur dans l’espace bâti.
Une arborisation importante de l’espace bâti permet notamment de limiter les îlots de chaleur, d’améliorer la qualité de l’air et d’offrir à une multitude d’espèces un refuge et de la nourriture. Dans le contexte actuel de changement climatique et d’érosion de la biodiversité, il est donc primordial de renforcer la couverture canopée dans l’espace bâti.
Pour l'interprétation des données, une comparaison avec la moyenne cantonale ou avec des communes au profil similaire peut être réalisée. "
)

title_complement_batiment_danger <- shiny::tags$p(style = "width:70vw;font-size:1.1rem;",
                                                  "L’indicateur du nombre de bâtiments exposé à des dangers naturels est obtenu par croisement spatial des données sur les bâtiments et celles sur les zones de dangers naturels. Les bâtiments sont sommés selon le degré de danger pour obtenir le nombre total par commune.",
                                                  tags$br(),
                                                  create_geoportail_tag(link = 'https://www.cdn.vd.ch',
                                                                        text = 'cdn.vd.ch')
)
