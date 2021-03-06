$(setup)

function setup() {
  $.ajaxSetup({dataType: "json", processData: false, contentType: "application/json"})

  var search = searchPage()
  var details = movieDetails()

  if (location.hash.indexOf('#/movies/') == 0) {
    var _id = location.hash.substring(9)
    $.get('/movies/' + _id).done(showClassification)
  }

  $('.new-movie').click(function() {
    $.post('/movies/new').done(function(movie) {
      location.hash = '#/movies/'+movie._id
      showClassification(movie)
    })
  })

  function showClassification(movie) {
    search.disable()
    details.show(movie)
  }
}

function searchPage() {
  var $input = $('#search-input')
  var $results = $('#search-results')

  $input.throttledInput(function() {
    var q = $('#search-input').val().trim()
    if (q == '') {
      $results.empty()
    } else {
      $.get('/movies/search/'+q).done(function(results) {
        var html = results.map(function(result) {
          return $('<div>')
            .append($('<h3>').text(result.name.join(', ')))
            .append($('<span>').text(result['name-fi'].join(', ')))
            .append($('<span>').text(result['name-sv'].join(', ')))
        })
        $results.html(html)
      })
    }
  })

  function disable() {
    $input.val('').attr('disabled', 'true')
    $results.empty()
  }

  return { disable: disable }

}

function movieDetails() {
  var $form = $('#movie-details')
  var $summary = $form.find('.summary')
  var $submit = $form.find('button[name=register]')

  renderClassificationCriteria()

  $form.find('select').on('change', function(e) {
    saveMovieField($form.data('id'), $(this).attr('name'), $(this).val())
  })

  $form.on('validation', function() {
    if ($form.find(".required.invalid").length === 0) {
      $submit.removeAttr('disabled')
    } else {
      $submit.attr('disabled', 'disabled')
    }
  })

  // validations
  $form.find('.required').on('keyup change validate', validate(isNotEmpty))
  $form.find('.duration').on('keyup change validate', validate(isValidDuration))

  $form.find('input[type=text], textarea').throttledInput(function(txt) {
    if ($(this).hasClass('invalid') && $(this).val().length > 0) return false
    var value = txt
    if ($(this).data('type') == 'number') {
      value = parseInt(txt)
    }
    saveMovieField($form.data('id'), $(this).attr('name'), value)
  })

  $form.find('.categories li').click(function() {
    $(this).addClass('selected').siblings().removeClass('selected')
    $form.find('.category-criteria ol').hide().eq($(this).index()).show()
  })

  $form.find('input[name="classifications.0.safe"]').change(function() {
    var safe = $(this).is(':checked')
    $form.find('.category-container').toggle(!safe)
    saveMovieField($form.data('id'), $(this).attr('name'), safe)
  })

  $form.find('.category-criteria input[type=checkbox]').change(function() {
    var ids = $form.find('.category-criteria input[type=checkbox]:checked').map(function(i, e) { return $(e).data('id') }).get()
    saveMovieField($form.data('id'), 'classifications.0.criteria', ids)
  })

  $summary.on('click', '.name', function() { $summary.toggleClass('hidden') })
  $summary.on('dragstart', '.warnings .warning', function(e) {
    var $e = $(this)
    e.originalEvent.dataTransfer.effectAllowed = 'move'
    e.originalEvent.dataTransfer.setData('text/plain', this.outerHTML)
    $form.find('.summary .drop-target').not($e.next()).addClass('valid')
    setTimeout(function() { $e.add($e.next()).addClass('dragging') }, 0)
  })
  $summary.on('dragenter', '.warnings .drop-target.valid', function(e) {
    e.preventDefault()
    return true
  })
  $summary.on('dragover', '.warnings .drop-target.valid', function(e) {
    $(this).addClass('active')
    e.preventDefault()
  })
  $summary.on('dragleave', '.warnings .drop-target.valid', function(e) {
    $(this).removeClass('active')
    e.preventDefault()
  })
  $summary.on('dragend', '.warnings .warning', function(e) {
    $(this).add($(this).next()).removeClass('dragging')
    $form.find('.summary .drop-target').removeClass('valid').removeClass('active')
  })
  $summary.on('drop', '.warnings .drop-target', function(e) {
    e.preventDefault()
    e.originalEvent.dataTransfer.dropEffect = 'move'
    $form.find('.summary .drop-target.valid').removeClass('valid')
    $form.find('.summary .dragging').remove()
    $(this).replaceWith([
      $('<span>', { class:'drop-target' }),
      $(e.originalEvent.dataTransfer.getData('text/plain')),
      $('<span>', { class:'drop-target' })
    ])
  })

  function show(movie) {
    $('.new-movie').attr('disabled', 'true')
    var classification = movie.classifications[0]

    $form.data('id', movie._id).show()
      .find('input[name="name.0"]').val(movie.name[0]).end()
      .find('input[name="name-fi.0"]').val(movie['name-fi'][0]).end()
      .find('input[name="name-sv.0"]').val(movie['name-sv'][0]).end()
      .find('input[name=country]').val(movie.country).end()
      .find('input[name=year]').val(movie.year).end()
      .find('select[name=genre]').val(movie.genre).end()
      .find('textarea[name=synopsis]').val(movie.synopsis).end()
      .find('input[name="classifications.0.buyer"]').val(classification.buyer).end()
      .find('input[name="classifications.0.billing"]').val(classification.billing).end()
      .find('select[name="classifications.0.format"]').val(classification.format).end()
      .find('input[name="classifications.0.duration"]').val(classification.duration).end()
      .find('input[name="classifications.0.safe"]').check(classification.safe).end()

    $form.find('input.country').select2({
      data: countryMatcher(),
      placeholder: "Valitse..."
    })
    $form.find('input.country').on('change', function(e) {
      saveMovieField($form.data('id'), $(this).attr('name'), $(this).val())
    })

    selectAutocomplete({
      $el: $form.find('input[name="production-companies"]'),
      val: movie['production-companies'] || [],
      path: '/production-companies/',
      toOption: companyToSelect2Option,
      fromOption: select2OptionToCompany,
      multiple: true,
    })

    selectAutocomplete({
      $el: $form.find('input[name="directors"]'),
      val: movie['directors'] || [],
      path: '/directors/',
      multiple: true,
      allowAdding: true
    })

    selectAutocomplete({
      $el: $form.find('input[name="actors"]'),
      val: movie['actors'] || [],
      path: '/actors/',
      multiple: true,
      allowAdding: true,
      termMinLength: 3
    })

    selectAutocomplete({
      $el: $form.find('input[name="classifications.0.buyer"]'),
      val: movie.classifications[0].buyer,
      path: '/accounts/',
      toOption: companyToSelect2Option,
      fromOption: select2OptionToCompany,
    })

    selectAutocomplete({
      $el: $form.find('input[name="classifications.0.billing"]'),
      val: movie.classifications[0].billing,
      path: '/accounts/',
      toOption: companyToSelect2Option,
      fromOption: select2OptionToCompany,
    })

    $form.find('.category-container').toggle(!classification.safe)
    $form.find('.category-criteria input').removeAttr('checked')

    classification.criteria.forEach(function(id) {
      $form.find('input[name=criteria-'+id+']').check(true)
    })
    $form.find('.category-criteria textarea').val()
    Object.keys(classification['criteria-comments'] || {}).forEach(function(id) {
      $form.find('textarea[name="classifications.0.criteria-comments.'+id+'"]').val(classification['criteria-comments'][id])
    })
    $form.find('.required').trigger('validate')
    updateSummary(movie)
  }

  function selectAutocomplete(opts) {
    var defaults = {
      toOption: function(x) { return {id: x, text: x} },
      fromOption: function(x) { return x.id },
      multiple: false,
      allowAdding: false,
      termMinLength: 0
    }

    opts = _.merge(defaults, opts)

    var $select = opts.$el

    function createSearchChoice(term, data) {
      if (_.indexOf(data, term) === -1) {
        return {id: term, text: term}
      }
    }

    $select.select2({
      query: function(query) {
        if ($.trim(query.term).length >= opts.termMinLength) {
          return $.get(opts.path + query.term).done(function(data) {
            return query.callback({results: data.map(opts.toOption)})
          })
        }
        return query.callback({results: []})
      },
      initSelection: function(element, callback) {
        var val = opts.multiple ? opts.val.map(opts.toOption) : opts.toOption(opts.val)
        return callback(val)
      },
      multiple: opts.multiple,
      placeholder: "Valitse...",
      createSearchChoice: opts.allowAdding ? createSearchChoice : undefined
    })

    $select.on('change', function(e) {
      var data = $(this).select2('data')
      var val = opts.multiple ? data.map(opts.fromOption) : opts.fromOption(data)
      saveMovieField($form.data('id'), $(this).attr('name'), val)
    })

    $select.select2('val', opts.val)
    $select.trigger('validate')
  }

  function companyToSelect2Option(x) {
    return {id: x._id, text: x.name} 
  }

  function select2OptionToCompany(x) {
    return {_id: x.id, name: x.text}
  }

  function renderClassificationCriteria() {
    var $categories = ['violence', 'sex', 'anxiety', 'drugs'].map(function(category) {
      var criteria = classificationCriteria.filter(function(c) { return c.category == category })
      var $criteria = criteria.map(function(c) {
        return $('<li>')
          .append($('<textarea>', { name:'classifications.0.criteria-comments.' + c.id }))
          .append($('<input>', { type: 'checkbox', name:'criteria-' + c.id, 'data-id': c.id } ))
          .append($('<span>', { class:'agelimit agelimit-' + c.age }))
          .append($('<span>').text(c.id + ' ' + c.title))
          .append($('<small>').text(c.description))
      })

      return $('<ol>', { class: category }).append($criteria)
    })
    $form.find('.category-criteria').html($categories)
  }

  function saveMovieField(id, field, value) {
    $.post('/movies/' + id, JSON.stringify(keyValue(field, value))).done(updateSummary)
  }

  function updateSummary(movie) {
    var classification = classificationSummary(movie.classifications[0])
    var warnings = [$('<span>', { class:'drop-target' })].concat(classification.warnings.map(function(w) { return $('<span>', { class:'warning ' + w, draggable:true }).add($('<span>', { class:'drop-target' })) }))
    $summary
      .find('.year').text(movie.year || '-').end()
      .find('.name').text(movie.name.join(', ') || '-').end()
      .find('.name-fi').text(movie['name-fi'].join(', ') || '-').end()
      .find('.name-sv').text(movie['name-sv'].join(', ') || '-').end()
      .find('.synopsis span').text(movie.synopsis || '-').end()
      .find('.country span').text(movie.country || '-').end()
      .find('.directors span').text((movie.directors).join(', ') || '-').end()
      .find('.actors span').text((movie.actors).join(', ') || '-').end()
      .find('.agelimit').attr('src', 'images/agelimit-'+classification.age+'.png').end()
      .find('.warnings').html(warnings).end()
  }

  function classificationSummary(classification) {
    if (classification.safe) return { age:'S', warnings:[] }
    var criteria = classification.criteria.map(function(id) { return classificationCriteria[id - 1] })
    var maxAgeLimit = criteria.reduce(function(accum, c) {
      var ageLimit = c.age
      if (ageLimit == 'S') return accum
      if (accum == 'S') return ageLimit
      return parseInt(ageLimit) > accum ? ageLimit : accum
    }, 'S')
    var warnings = criteria
      .filter(function(c) { return c.age == maxAgeLimit })
      .map(function(c) { return c.category })
      .reduce(function(accum, c) { if (accum.indexOf(c) == -1) accum.push(c); return accum }, [])
    return { age: maxAgeLimit, warnings: warnings }
  }

  return { show: show }
}

function keyValue(key, value) {
  var data = {}
  data[key] = value
  return data
}

function isNotEmpty(val) {
  return (val.trim().length) > 0
}

function isValidDuration(txt) {
  return /(?:(\d+)?:)?(\d+):(\d+)$/.test(txt)
}

function validate(f) {
  return function(e) {
    var $el = $(this)
    if (f($el.val())) {
      $el.removeClass('invalid')
    } else {
      $el.addClass('invalid')
    }
    $el.trigger('validation')
  }
}

$.fn.throttledInput = function(fn) {
  return $(this).each(function() {
    var prev = undefined
    var timeout = null
    var $input = $(this).on('keyup', function() {
      var txt = $input.val()
      var that = this
      if (timeout) clearTimeout(timeout)
      timeout = setTimeout(function() {
        if (prev == txt) return
        prev = txt
        fn.call(that, txt)
      }, 400)
    })
  })
}

$.fn.check = function(on) {
  return on ? $(this).prop('checked', 'checked') : $(this).removeProp('checked')
}

function countryMatcher() {
  var countries = ['Afganistan', 'Alankomaat', 'Albania', 'Algeria', 'Arabiemiirikunnat', 'Argentiina', 'Australia', 'Bangladesh', 'Belgia', 'Bermuda', 'Bhutan', 'Bhutan', 'Bolivia', 'Bosnia-Hertsegovina', 'Brasilia', 'Bulgaria', 'Chile', 'Costa Rica', 'Ecuador', 'Egypti', 'El Salvador', 'Espanja', 'Etelä-Afrikka', 'Etelä-Korea', 'Etiopia', 'Filippiinit', 'Fär-saaret', 'Grönlanti', 'Guatemala', 'Guinea', 'Hongkong', 'Indonesia', 'Intia', 'Irak', 'Iran', 'Irlanti', 'Islanti', 'Iso-Britannia', 'Israel', 'Italia', 'Itä-Saksa', 'Itävalta', 'Jamaika', 'Japani', 'Jordania', 'Jugoslavia', 'Kamerun', 'Kanada', 'Kenia', 'Kiina', 'Kolumbia', 'Kreikka', 'Kroatia', 'Kuuba', 'Kypros', 'Laos', 'Latvia', 'Libanon', 'Lichtenstein', 'Liettua', 'Luxemburg', 'Malesia', 'Mali', 'Malta', 'Marokko', 'Mauritania', 'Mauritius', 'Meksiko', 'Monaca', 'Mongolia', 'Mosambik', 'Muu maa', 'Namibia', 'Nepal', 'Neuvostoliitto', 'Nicaragua', 'Niger', 'Nigeria', 'Norja', 'Pakistan', 'Paraguay', 'Peru', 'Pohjois-Korea', 'Portugali', 'Puerto Rico', 'Puola', 'Ranska', 'Romania', 'Ruotsi', 'Saksa', 'Sambia', 'Senegal', 'Serbia / Tsekkoslovakia', 'Singapore', 'Slovakia', 'Slovenia', 'Sri Lanka', 'Sudan', 'Suomi', 'Sveitsi', 'Syyria', 'Taiwan', 'Tansania', 'Tanska', 'Thaimaa', 'Tsekinmaa', 'Tunisia', 'Turkki', 'Unkari', 'Uruguay', 'Uusi-Seelanti', 'Valko-Venäjä', 'Vatikaanivaltio', 'Venezuela', 'Venäjä', 'Vietnam', 'Viro', 'Yhdysvallat', 'Zimbabwe']
  return countries.map(function(x) { return {id: x, text: x}})
  //return function(q, callback) {
  //  var regexp = new RegExp('(^| )' + q, 'i')
  //  var result = countries.filter(function(s) { return regexp.test(s) }).map(function(s) { return { value: s } })
  //  callback(result)
  //}
}

var classificationCriteria = [
  { id:1,  category: 'violence', age: '18', title: "Erittäin voimakasta väkivaltaa", description: "Fiktiivistä, realistista ja erittäin veristä ja yksityiskohtaista tai erittäin pitkäkestoista ja yksityiskohtaista tai erittäin pitkäkestoista ja sadistista ihmisiin tai eläimiin kohdistuvaa väkivaltaa" },
  { id:2,  category: 'violence', age: '18', title: "Erittäin voimakasta väkivaltaa", description: "Aitoa ja yksityiskohtaisesti tai selväpiirteisesti sekä viihteellisesti tai ihannoiden esitettyä ihmisiin tai eläimiin kohdistuvaa väkivaltaa." },
  { id:3,  category: 'violence', age: '18', title: "Erittäin voimakasta väkivaltaa", description: "Fiktiivistä, selväpiirteisesti ja pitkäkestoisesti esitettyä seksiin liittyvää väkivaltaa (raiskaus, insesti, pedofilia)" },
  { id:4,  category: 'violence', age: '16', title: "Voimakasta väkivaltaa", description: "Fiktiivistä tai aitoa yksityiskohtaista ja realistista tai hallitsevaa tai pitkäkestoista ihmisiin tai eläimiin kohdistuvaa väkivaltaa." },
  { id:5,  category: 'violence', age: '16', title: "Voimakasta väkivaltaa", description: "Fiktiivistä tai aitoa yksityiskohtaisesti ja korostetusti tai yksityiskohtaisesti ja viihteellistetysti esitettyä ihmisiin tai eläimiin kohdistuvaa väkivallan tai onnettomuuksien seurausten kuvausta." },
  { id:6,  category: 'violence', age: '16', title: "Voimakasta väkivaltaa", description: "Fiktiivistä, esitystavaltaan selvästi yliampuvaa tai parodista, veristä ja yksityiskohtaista tai pitkäkestoista ja yksityiskohtaista tai pitkäkestoista ja sadistista ihmisiin tai eläimiin kohdistuvaa väkivaltaa." },
  { id:7,  category: 'violence', age: '16', title: "Voimakasta väkivaltaa", description: "Aitoa yksityiskohtaisesti ta selväpiirteisesti esitettyä väkivaltaa, jossa uhrin kärsimykset tai väkivallan seuraukset tuodaan realistisesti esille." },
  { id:8,  category: 'violence', age: '16', title: "Voimakasta väkivaltaa", description: "Seksiin liittyvää fiktiivistä väkivaltaa, jossa uhrin kärsimys tulee selvästi esiin ja väkivalta on tarinan kannalta perusteltua tai voimakkaita viittauksia alaikäisiin kohdistuvaan seksuaaliseen väkivaltaan tai hyväksikäyttöön." },
  { id:9,  category: 'violence', age: '12', title: "Väkivaltaa", description: "Ei erityisen yksityiskohtaista tai ei hallitsevasti lapsiin, eläimiin tai lapsi-päähenkilön perheenjäseniin kohdistuvaa tai tarinan kannalta perusteltu yksittäinen, yksityiskohtainen ihmisiin tai eläimiin kohdistuva väkivaltakohtaus." },
  { id:10, category: 'violence', age: '12', title: "Väkivaltaa", description: "Epärealistisessa, etäännytetyssä yhteydessä esitettyä (joko epärealistinen väkivalta ihmis- tai eläinmäisiä hahmoja kohtaan tai realistinen väkivalta selkeän kuvitteellisia hahmoja kohtaan tai historiallinen, kulttuurinen jne. etäännytys!)" },
  { id:11, category: 'violence', age: '12', title: "Väkivaltaa", description: "Seksuaaliseen väkivaltaan viittaavaa (raiskaus, insesti, pedofilia)." },
  { id:12, category: 'violence', age: '7',  title: "Lievää väkivaltaa", description: "Epärealistista tai komediallista tai animaatio- tai slapstick-komediassa esitettyä yliampuvaa tai vähäistä väkivaltaa." },
  { id:13, category: 'violence', age: '7',  title: "Lievää väkivaltaa", description: "Yksittäinen, lievähkö ja lyhytkestoinen realistinen väkivaltakohtaus tai selkeät, mutta lievät tai lyhytkestoiset väkivaltaviitteet." },
  { id:14, category: 'violence', age: 'S',  title: "Väkivaltaa tai vain hyvin lievää väkivaltaa", description: "Kuvaohjelmassa ei ole lainkaan väkivaltaa tai se on vain hyvin lievää." },
  { id:15, category: 'sex',      age: '18', title: "Erittäin yksityiskohtaista seksuaalista sisältöä", description: "Hallitsevaa ja seksikohtauksissa sukuelimiä selväpiirteisesti näyttävää." },
  { id:16, category: 'sex',      age: '16', title: "Avointa seksuaalista sisältöä", description: "Avointa, mutta yksityiskohdiltaan peiteltyä kuvausta tai yksityiskohtainen, yksittäinen ja lyhyt seksikohtaus." },
  { id:17, category: 'sex',      age: '12', title: "Seksuaalista sisältöä", description: "Peiteltyjä seksikohtauksia tai runsaasti selkeitä seksiviitteitä." },
  { id:18, category: 'sex',      age: '12', title: "Seksuaalista sisältöä", description: "Yksittäinen avoin, mutta yksityiskohdiltaan peitelty seksikuvaus (seksikohtaus)." },
  { id:19, category: 'sex',      age: '7',  title: "Lievää seksuaalista sisältöä", description: "Lieviä seksuaalisia viittauksia tai yksittäisiä verhotusti esitettyjä eroottissävyisiä kohtauksia." },
  { id:20, category: 'sex',      age: 'S',  title: "Vain hyvin lievää seksuaalista sisältöä", description: "Halailua, syleilyä tai suudelmia tai alastomuutta muussa kuin seksuaalisessa kontekstissa." },
  { id:21, category: 'anxiety',  age: '18', title: "Erittäin voimakasta ahdistusta herättävää sisältöä", description: "Hallitsevaa, erittäin järkyttävää, yksityiskohtaista kuvausta ihmisiin tai eläimiin kohdistuvista julmuuksista tai perversioista." },
  { id:22, category: 'anxiety',  age: '18', title: "Erittäin voimakasta ahdistusta herättävää sisältöä", description: "Aitoa ongelmattomana tai ihannoiden esitettyä itseä tai muita vahingoittavaa, vakavasti henkeä uhkaavaa ja hengenvaarallista käyttäytymistä." },
  { id:23, category: 'anxiety',  age: '16', title: "Voimakasta ahdistusta herättävää sisältöä", description: "Ihmisiin tai eläimiin kohdistuvaa järkyttävää ja ahdistusta herättävää, pitkäkestoista ja intensiivistä kuoleman, vakavan väkivallan tai psyykkisen hajoamisen uhkaa. Myös itsemurhan ihannointi. Yliluonnolliseen liittyvää voimakasta ahdistavuutta. Yliluonnolliseen liittyvää voimakasta ahdistavuutta." },
  { id:24, category: 'anxiety',  age: '16', title: "Voimakasta ahdistusta herättävää sisältöä", description: "Runsaasti realistisia ja yksityiskohtaisia (makaabereja) kuvia silpoutuneista, pahoin vahingoittuneista tai mädäntyneistä ruumiista tai väkivallan uhreista." },
  { id:25, category: 'anxiety',  age: '16', title: "Voimakasta ahdistusta herättävää sisältöä", description: "Aitoa, ihannoivasti esitettyä itseä tai muita vahingoittavaa käyttäytymistä." },
  { id:26, category: 'anxiety',  age: '12', title: "Melko voimakasta ahdistusta herättävää sisältöä", description: "Ihmisiin tai eläimiin kohdistuvaa lyhytkestoista tai ei-hallitsevaa väkivallan tai kuoleman uhkaa tai kaltoin kohtelun tai psyykkisen kärsimyksen kuvausta. Menetysten, esim. perheenjäsenten sairauden tai kuoleman, voimakkaan surun, sekavuustilan tai itsemurhan kuvauksia." },
  { id:27, category: 'anxiety',  age: '12', title: "Melko voimakasta ahdistusta herättävää sisältöä", description: "Ahdistusta herättäviä luonnonmullistusten, onnettomuuksien, katastrofien tai konfliktien ja niihin kytkeytyvän kuoleman uhan tai uhrien kuvauksia." },
  { id:28, category: 'anxiety',  age: '12', title: "Melko voimakasta ahdistusta herättävää sisältöä", description: "Voimakkaita, äkillisiä ja yllättäviä ahdistusta, pelkoa tai kauhua herättäviä ääni- ja kuvatehosteita tai pitkäkestoista piinaavaa uhkaa. Yliluonnolliseen liittyvää melko voimakasta ahdistavuutta." },
  { id:29, category: 'anxiety',  age: '12', title: "Melko voimakasta ahdistusta herättävää sisältöä", description: "Yksittäisiä realistisia ja yksityiskohtaisia kuvauksia silpoutuneista, pahoin vahingoittuneista tai mädäntyneistä ruumiista tai väkivallan uhreista." },
  { id:30, category: 'anxiety',  age: '12', title: "Melko voimakasta ahdistusta herättävää sisältöä", description: "Aitoa, itseä tai muita vahingoittavaa käyttäytymistä." },
  { id:31, category: 'anxiety',  age: '7',  title: "Lievää ahdistusta herättävää sisältöä", description: "Melko lieviä ja lyhytkestoisia kauhuelementtejä, pientä pelottavuutta tai jännittävyyttä tai väkivallan uhkaa esimerkiksi animaatiossa tai fantasiassa (hirviöhahmoja, muodonmuutoksia, synkähköä visuaalista kuvastoa, lyhytkestoisia takaa-ajoja tai kohtalaisia äänitehosteita)." },
  { id:32, category: 'anxiety',  age: '7',  title: "Lievää ahdistusta herättävää sisältöä", description: "Lasten universaaleja pelkoja käsitteleviä kuvauksia tai tilanteita (esimerkiksi yksin jääminen, vanhemmista eroon joutuminen, pimeä, eksyminen tai läheisen menettäminen)." },
  { id:33, category: 'anxiety',  age: '7',  title: "Lievää ahdistusta herättävää sisältöä", description: "Dokumentaarista ihmisiin/eläimiin kohdistuvaa  lyhytkestoista uhkaa ilman tehosteita." },
  { id:34, category: 'anxiety',  age: 'S',  title: "Vain hyvin lievää ahdistavaa sisältöä", description: "Hyvin lieviä ja lyhytkestoisia pelottavia tai jännittäviä elementtejä, jotka ratkeavat hyvin nopeasti positiiviseen suuntaan." },
  { id:35, category: 'drugs',    age: '18', title: "Ihannoivaa erittäin vaarallisten huumeiden käyttöä", description: "Hallitsevaa ja ihannoivassa valossa yksityiskohtaisesti esitettyä erittäin vaarallisten huumeiden käyttöä." },
  { id:36, category: 'drugs',    age: '16', title: "Huumeiden käyttöä", description: "Huumeiden realistista ja yksityiskohtaista ongelmakäyttöä tai yksittäisiä ongelmattomia tai ihannoivia huumeiden käytön kuvauksia." },
  { id:37, category: 'drugs',    age: '12', title: "Huumeiden ei-hallitsevaa käyttöä / alaikäisten alkoholin käyttöä", description: "Tällä tarkoitetaan huumeiden viitteellistä tai vähäistä käyttöä tai alaikäisten korostettua, viihteellistä tai ongelmatonta alkoholin käyttöä." }
]
