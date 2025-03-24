$( document ).ready(function() {
  
  // Shiny Custom Message Handler
  Shiny.addCustomMessageHandler("startIntro", function(message) {
    introJs()
      .setOptions({
        steps: [
          {
            intro: "Bienvenue dans ce tour guidé du <b>profil climatique</b> !<br>Parcourez ces quelques étapes pour mieux comprendre comment utiliser cette application."
          },
          {
            element: "#map",
            intro: "<b>Communes 1/2</b><br>Pour commencer, sélectionnez une ou plusieurs communes sur la carte.",
            position: "left"
          },
          {
            element: "#introjs_select_communes",
            intro: "<b>Communes 2/2</b><br>Vous pouvez également choisir des communes dans la barre latérale.<br>Vous y trouverez également les <strong>valeurs cantonales</strong> pour d\'éventuelles comparaisons.",
            position: "right"
          },
          {
            element: "#nav",
            intro: "<b>Données 1/2</b><br> Naviguez ensuite dans les différents sujets ici. C\'est dans ces onglets que vous trouverez les données propres à chaque thème !",
            tooltipClass: "introjs-center-tooltip"
          },
          {
            element: "#introjs_toc_accordion",
            intro: "<b>Données 2/2</b><br> Ou explorez directement les données disponibles dans ce menu. Chaque onglet contient des graphiques et des tables qui peuvent facilement être exportés.",
            position: "left"
          },
          {
            element: "#introjs_download_all",
            intro: "<b>Droit au but !</b><br>Vous pouvez aussi directement télécharger toutes les données de l\'application au format Excel, pour autant qu\'au moins une commune soit sélectionnée.",
            position: "right"
          },
          {
            intro: "D\'autres fonctionnalités comme l\'importation de communes via un fichier ou le changement d\'unités permettent de vous simplifier la vie."
          },
          {
            intro: "Voilà pour l\'essentiel, nous espérons que vous trouverez cette application utile.<br><br>Bonne visite ! &#x1F389;"
          }
        ],
        nextLabel: "Suivant",
        prevLabel: "Précédent",
        skipLabel: "Passer",
        doneLabel: "Terminé",
        showProgress: true,
        showBullets: true,
      })
      .start();
  });
  
});
