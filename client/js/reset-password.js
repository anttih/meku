function resetPassword() {
  localize()
  $.ajaxSetup({ dataType: 'json', processData: false, contentType: 'application/json' })

  var $form = $('#reset-password').submit(function(e) { e.preventDefault() })
  var $password = $form.find('input[name="password"]').on('input', checkInput)
  var $passwordConfirmation = $form.find('input[name="password-confirmation"]').on('input', checkInput)
  var $feedback = $form.find('.feedback')
  var $button = $form.find('button')

  var resetHash = location.hash.substring(1)

  $form.on('validate', function () {
    var tooShortPassword = $form.find('input[name="password"]').hasClass('invalid')
    var notSamePasswords = ($password.val() !== $passwordConfirmation.val())

    $button.prop('disabled', tooShortPassword || notSamePasswords)

    var feedback = []

    if (tooShortPassword) {
      feedback.push($('<div>').i18nText('Salasanan tulee olla vähintään kuusi merkkiä pitkä.'))
    }

    if (notSamePasswords) {
      feedback.push($('<div>').i18nText('Salasanat eivät täsmää.'))
    }

    if (feedback.length > 0) {
      $feedback.html(feedback)
      $feedback.slideDown()
    } else {
      $feedback.slideUp()
    }
  })

  $button.click(function() {
    $button.prop('disabled', true)
    $.post('/reset-password', JSON.stringify({ resetHash: resetHash, password: $password.val() }))
      .done(function() {
        window.location = '/'
      })
      .fail(function() {
        $feedback.i18nText('Virhe salasanan vaihtamisessa.').slideDown()
      })
  })

  return { show: show }

  function show() {
    $form.show()
    if (resetHash) {
      $.get('/check-reset-hash/' + resetHash).fail(function() { window.location = '/' })
    } else {
      window.location = '/'
    }
  }

  function checkInput() {
    $(this).toggleClass('invalid', ($(this).val().length < 6)).trigger('validate')
  }
}
