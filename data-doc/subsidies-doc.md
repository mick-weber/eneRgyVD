<!--- Content retrieved by 'generate_doc_accordion_panels()' in fct_helpers.R & utils_helpers.R -->
<!--- Don't add linebreaks within paragraphs or use <br> tags inline, add empty line at the end, prefer plain HTML for links -->
# Subventions bâtiments

## D'où viennent les données ?

De la plateforme IWF utilisée pour la gestion des subventions du Programme bâtiments. Uniquement les subventions payées sont intégrées dans ces statistiques ; les demandes de subventions octroyées mais dont la réalisation des travaux n'ont pas été constatés, ne sont pas comprises. <a href='https://www.leprogrammebatiments.ch/fr/cantons/vaud/' target='_blank'>Plus de détails sur les subventions du Programme bâtiments vaudois</a>.

## Qui traite ces données ?

La DGE-DIREN récupère les données du Programme bâtiments via une API. Elle procède ensuite au traitement et à la réalisation de statistiques communales.

## Quand est-ce que les données sont mises à jour ?

Les données sont disponibles quelques mois après la fin d'une année civile. 

## À quoi faut-il faire attention ?

Les mesures indirectes du Programme bâtiments ne sont pas intégrées dans ces statistiques. De très légères différences statistiques peuvent apparaître en comparaison des informations fournies directement par le Programme bâtiments car la méthode diffère. Il se peut également que des subventions ne puissent pas être rattachées à un bâtiment, et donc à une commune ce qui les exclut de ces statistiques. L'exemple ci-dessous permet de bien comprendre la différence entre la vue par travaux subventionnés, et la vue cumulée du parc subventionné.
<br><br>
<em>Un bâtiment reçoit sa première subvention d'isolation partielle (M01) en 2018. En 2020, il en reçoit une nouvelle et reçoit également la subvention de bonus d'isolation (M01 + M14) car il atteint le seuil des 90% de surface conformément isolées. Cette même année, une pompe à chaleur est également installée avec la subvention M07.
</em>
<br><br>
Dans la <strong>vue par travaux subventionnés </strong> le bâtiment va se retrouver 1x en 2018 sous la catégorie dans la catégorie <em>Isolation partielle</em>, puis 2x en 2020 : 1x pour le couple M01/M14 sous la catégorie <em>Isolation globale</em> et 1x dans la catégorie <em>Chauffage renouvelable</em>. À noter également que lorsqu'une seule subvention d'isolation est remplie pour deux bâtiments adjacents (numéros EGID), on considère dans cette statistique que deux travaux distincts ont été réalisés.
<br>
Dans la vue <strong>vue cumulée du parc subventionné</strong>, le bâtiment ne peut pas se retrouver plus d'une fois par année : il figurera en 2018 ainsi qu'en 2019 dans la catégorie <em>Isolation partielle</em>, puis en 2020 il passera dans la catégorie <em>Isolation globale + chauffage renouvelable</em>. 
