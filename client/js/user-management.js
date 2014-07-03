function userManagementPage() {
  var $page = $('#user-management-page')
  var $userList = $page.find('.user-list')
  var $userNameQuery = $page.find('#user-name-query')
  var dateFormat = 'DD.MM.YYYY'

  var $newUserType = $page.find('.new-user input[name="new-user-type"]')
  $newUserType.select2({
    data: _.map(enums.userRoles, function(roleValue, roleKey) {
      return { id: roleKey, text: roleValue.name }
    }),
    minimumResultsForSearch: -1
  }).select2('val', 'user')

  $page.on('show', function(event, userId) {
    updateLocationHash(userId || '')
    $userList.empty()
    $.get('/users', function(users) {
      renderUsers(users)
      if (userId) {
        var $selected = $userList.find('.result[data-id=' + userId + ']')
        openDetails($selected)
        var top = $selected.offset().top - 25
        $('body,html').animate({ scrollTop: top })
      }
    })
  })

  $userList.on('click', '.result', function() {
    var $this = $(this)
    if ($this.hasClass('selected')) {
      closeDetails()
    } else {
      closeDetails()
      openDetails($this)
    }
  })

  $page.find('.new-user button').on('click', function() {
    var $newUserForm = renderNewUserForm($newUserType.select2('val'))

    closeDetails()
    $userList.addClass('selected').prepend($newUserForm)
    $newUserForm.slideDown()
  })

  $userNameQuery.on('input', function() {
    var searchString = $(this).val().toLowerCase()

    $userList.find('.result').each(function() {
      var name = $(this).children('.name').text().toLowerCase()
      if (_.contains(name, searchString)) {
        $(this).show()
      } else {
        $(this).hide()
      }
    })
  })

  function updateLocationHash(userId) {
    location.hash = '#kayttajat/' + userId
  }

  function renderUsers(users) {
    _(users).sortBy('name').map(renderUser).forEach(function(u) { $userList.append(u) })
  }

  function renderUser(user) {
   return $('<div>', { class: 'result', 'data-id': user._id })
     .data('user', user).data('id', user._id)
     .append($('<span>', { class: 'name' }).text(user.name))
     .append($('<span>', { class: 'role' }).html(enums.util.userRoleName(user.role) || '<i class="icon-warning-sign"></i>'))
     .append($('<span>', { class: 'cert-end '}).html('<i class="icon-warning-sign"></i>'))
  }

  function openDetails($row) {
    var user = $row.data('user')
    var $userDetails = renderExistingUserDetails(user)
    $row.addClass('selected').after($userDetails)
    updateLocationHash(user._id)
    $userDetails.slideDown()
  }

  function closeDetails() {
    $userList.find('.result.selected').removeClass('selected')
    $userList.find('.user-details').slideUp(function() { $(this).remove() })
    updateLocationHash('')
  }

  function renderNewUserForm(role) {
    var $detailTemplate = renderUserDetails(null, role)

    $detailTemplate.submit(function(event) {
      event.preventDefault()

      var $this = $(this)
      var userData = getUserData($this)
      userData.role = role
      userData.active = true

      if (enums.util.isClassifier(role)) {
        userData = _.merge(userData, getClassifierData($this))
      }

      $.post('/users/new', JSON.stringify(userData), function(newUser) {
        $userList.find('.result.selected').data('user', newUser)
        var $user = renderUser(newUser).css('display', 'none')
        $userList.prepend($user)
        $user.slideToggle()
        closeDetails()
      })
    })

    return $detailTemplate
  }

  function renderExistingUserDetails(user) {
    var $detailTemplate = renderUserDetails(user)

    $detailTemplate.submit(function(event) {
      event.preventDefault()

      var $this = $(this)
      var userData = getUserData($this)

      if (enums.util.isClassifier(user.role)) {
        userData = _.merge(userData, getClassifierData($this))
      }

      $.post('/users/' + user._id, JSON.stringify(userData), function(updatedUser) {
        var selected = $userList.find('.result.selected')
        selected.data('user', updatedUser)
        selected.find('span.name').text(updatedUser.name)
        $userList.find('.result.selected').data('user', updatedUser)
        closeDetails()
      })
    })

    return $detailTemplate
  }

  function renderUserDetails(user, role) {
    var $detailTemplate = $('#templates').find('.user-details').clone()
    var isNewUser = user == null
    var isClassifier = enums.util.isClassifier(role) || user && enums.util.isClassifier(user.role)

    if (isClassifier) {
      $detailTemplate.find('.classifier-details').removeClass('hide')
    }

    if (isNewUser) {
      if (isClassifier) {
        initSearch2Autocomplete($detailTemplate.find('input[name=employers]'), employersSearch)
      }

      $detailTemplate.find('.modify-only').remove()
      $detailTemplate.find('input:required:disabled').prop('disabled', false)
    } else {
      if (isClassifier) {
        var $employers = $detailTemplate.find('input[name=employers]')

        initSearch2Autocomplete($employers, employersSearch)
        $employers.trigger('setVal', user.employers).end()
      }

      populate($detailTemplate, user)
    }

    $detailTemplate.find('form').on('input', _.throttle(function() { $(this).trigger('validate') }, 200))

    $detailTemplate.find('form').on('validate', function() {
      $(this).find('button[type=submit]').prop('disabled', !this.checkValidity())
    })

    $detailTemplate.find('input').on('blur', function() {
      $(this).addClass('touched')
    })

    $detailTemplate.find('input[name=username]').on('input', function() {
      var $username = $(this)
      validateUsername($username, $detailTemplate)
    })

    return $detailTemplate.css('display', 'none')

    function employersSearch(term) {
        return '/accounts/search?q=' + encodeURIComponent(term) + '&roles=Classifier' // todo: only classifier?
    }

    function initSearch2Autocomplete($element, path) {
      select2Autocomplete({
        $el: $element,
        path: path,
        toOption: idNamePairToSelect2Option,
        fromOption: select2OptionToIdNamePair,
        multiple: true
      })
    }

    function populate($element, user) {
      var cStartDate = user.certificateStartDate ? moment(user.certificateStartDate).format(dateFormat) : ''
      var cEndDate = user.certificateEndDate ? moment(user.certificateEndDate).format(dateFormat) : ''

      $element.find('input[name=name]').val(user.name).end()
        .find('input[name=email]').val(user.emails[0]).end()
        .find('input[name=username]').val(user.username).end()
        .find('input[name=active]').prop('checked', user.active).end()
        .find('input[name=phoneNumber]').val(user.phoneNumber).end()
        .find('input[name=certificateStartDate]').val(cStartDate).end()
        .find('input[name=certificateEndDate]').val(cEndDate).end()
        .find('textarea[name=comment]').val(user.comment).end()
    }
  }

  function getUserData($details) {
    return {
      name: $details.find('input[name=name]').val(),
      emails: [ $details.find('input[name=email]').val() ],
      username: $details.find('input[name=username]').val(),
      active: $details.find('input[name=active]').prop('checked'),
      phoneNumber: $details.find('input[name=phoneNumber]').val()
    }
  }

  function getClassifierData($details) {
    return {
      certificateStartDate: $details.find('input[name=certificateStartDate]').val(),
      certificateEndDate: $details.find('input[name=certificateEndDate]').val(),
      employers: $details.find('input[name=employers]').select2('data').map(select2OptionToIdNamePair),
      comment: $details.find('textarea[name=comment]').val()
    }
  }

  var usernameValidator = _.debounce((function() {
    var getLatestAjax = switchLatestDeferred()
    return function(username, $username, $detailTemplate) {
      getLatestAjax($.get('/users/exists/' + username), $username.siblings('i.icon-spinner'))
        .done(function(data) {
          $username.get(0).setCustomValidity(data.exists ? 'Username taken' : '')
          $username.removeClass('pending')
          $detailTemplate.find('form').trigger('validate')
        })
      }
  })(), 300)

  function validateUsername($username, $detailTemplate) {
    $username.get(0).setCustomValidity('Checking username')
    $username.addClass('touched')
    var username = $username.val().trim()
    if (username) {
      $username.addClass('pending')
      usernameValidator(username, $username, $detailTemplate)
    }
  }
}
