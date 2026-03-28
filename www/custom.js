$(document).on('click', '.panchayat-link', function(e) {
  e.preventDefault();
  var block = $(this).data('block');
  var panchayat = $(this).data('panchayat');
  Shiny.setInputValue('navigate_to_panchayat', {
    block: block,
    panchayat: panchayat,
    nonce: Math.random()
  }, {priority: 'event'});
});
