(function () {
  // Kill a child
  $('form').on('click', '.child button', function () {
    $(this).closest('.child').remove();
  })
  
  // Add a child
  $('form button.add-child').on('click', function () {
    var field = Form().child('');
    $('form .children').append(field.root);
  })
})();