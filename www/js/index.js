$(document).ready(function() {
  $(".content-wrapper").css("min-height", "1800px");
  $('#sidebarItemExpanded li:first').addClass('active');
})

$(document).on('click', '.modal-backdrop', function() {
  $('#meuModal').modal('hide');
});