$( document ).ready(function() {

  // Shiny Custom Message Handler to toggle the dropdown
  Shiny.addCustomMessageHandler('toggleDropdown', function(msg) {
    $('.dropdown-menu').removeClass('show');
  });

});
