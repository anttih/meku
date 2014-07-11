var express = require('express')
var _ = require('lodash')
var async = require('async')
var path = require('path')
var moment = require('moment')
var mongoose = require('mongoose')
var schema = require('./schema')
var Program = schema.Program
var User = schema.User
var Account = schema.Account
var InvoiceRow = schema.InvoiceRow
var enums = require('../shared/enums')
var utils = require('../shared/utils')
var proe = require('../shared/proe')
var classification = require('../shared/classification')
var xml = require('./xml-import')
var sendgrid  = require('sendgrid')(process.env.SENDGRID_USERNAME, process.env.SENDGRID_PASSWORD);
var builder = require('xmlbuilder')
var bcrypt = require('bcrypt')
var CronJob = require('cron').CronJob

express.static.mime.define({ 'text/xml': ['xsd'] })

var app = express()

app.use(express.json())
app.use(nocache)
app.use(express.cookieParser(process.env.COOKIE_SALT || 'secret'))
app.use(authenticate)
app.use(express.static(path.join(__dirname, '../client')))
app.use('/shared', express.static(path.join(__dirname, '../shared')))

mongoose.connect(process.env.MONGOHQ_URL || 'mongodb://localhost/meku')

app.post('/login', function(req, res, next) {
  var username = req.body.username
  var password = req.body.password
  if (!username || !password) return res.send(403)
  User.findOne({ username: username, active: { $ne: false } }, function(err, user) {
    if (err) return next(err)
    if (!user) return res.send(403)
    if (user.certificateEndDate && moment(user.certificateEndDate).isBefore(moment()) ) {
      user.update({ active: false }, respond(res, next, 403))
    } else {
      user.checkPassword(password, function(err, ok) {
        if (err) return next(err)
        if (!ok) return res.send(403)
        setLoginCookie(res, user)
        res.send({})
      })
    }
  })
})

function setLoginCookie(res, user) {
  res.cookie('user', {
    _id: user._id.toString(),
    username: user.username,
    name: user.name,
    role: user.role,
    email: _.first(user.emails)
  }, { signed: true })
}

app.post('/logout', function(req, res, next) {
  res.clearCookie('user')
  res.send({})
})

app.post('/forgot-password', function(req, res, next) {
  var username = req.body.username

  if (!username) return res.send(403)

  User.findOne({ username: username, active: { $ne: false } }, function(err, user) {
    if (err) return next(err)
    if (!user) return res.send(403)

    if (!user.emails) {
      console.log(user.username + ' has no email address')
      return res.send(500)
    }

    var subject = 'Ohjeet salasanan vaihtamista varten'
    var text = 'Tämän linkin avulla voit vaihtaa salasanasi: '

    if (user.resetHash) {
      sendHashLinkViaEmail(user, subject, text, respond(res, next, {}))
    } else {
      createAndSaveHash(user, function(err) {
        if (err) return next(err)
        sendHashLinkViaEmail(user, subject, text, respond(res, next, {}))
      })
    }
  })
})

function sendHashLinkViaEmail(user, subject, text, callback) {
  var hostUrl = isDev() ? 'http://localhost:3000' : 'https://meku.herokuapp.com'
  var url = hostUrl + '/reset-password.html#' + user.resetHash
  var emailData = {
    recipients: user.emails,
    subject: subject,
    body: text + '<a href="' + url + '">' + url + '</a>'
  }

  sendEmail(emailData, callback)
}

function createAndSaveHash(user, callback) {
  bcrypt.genSalt(1, function (err, s) {
    user.resetHash = new Buffer(s, 'base64').toString('hex')
    user.save(callback)
  })
}

app.get('/check-reset-hash/:hash', function(req, res, next) {
  User.findOne({ resetHash: req.params.hash, active: { $ne: false } }, function(err, user) {
    if (err) return next(err)
    if (!user) return res.send(403)
    if (user.password) return res.send({ name: user.name })
    res.send({ newUser: true, name: user.name })
  })
})

app.post('/reset-password', function(req, res, next) {
  var resetHash = req.body.resetHash
  if (resetHash) {
    User.findOne({ resetHash: resetHash, active: { $ne: false } }, function (err, user) {
      if (err) return next(err)
      if (!user) return res.send(403)
      user.password = req.body.password
      user.resetHash = null
      user.save(function (err, user) {
        if (err) return next(err)
        setLoginCookie(res, user)
        res.send({})
      })
    })
  } else {
    return res.send(403)
  }
})

app.get('/public/search/:q?', function(req, res, next) {
  search(Program.publicFields, req, res, next)
})
app.get('/programs/search/:q?', function(req, res, next) {
  var fields = utils.hasRole(req.user, 'kavi') ? null : { 'classifications.comments': 0 }
  search(fields, req, res, next)
})

function search(responseFields, req, res, next) {
  var page = req.query.page || 0
  var filters = req.query.filters || []
  Program.find(query(), responseFields).skip(page * 100).limit(100).sort('name').exec(respond(res, next))

  function query() {
    var terms = req.params.q
    var q = { deleted: { $ne:true } }
    var nameQuery = toMongoArrayQuery(terms)
    if (nameQuery) {
      if (nameQuery.$all.length == 1 && parseInt(terms) == terms) {
        q.$or = [{ allNames:nameQuery }, { sequenceId: terms }]
      } else {
        q.allNames = nameQuery
      }
    }
    if (filters.length > 0) q.programType = { $in: filters }
    return q
  }
}

app.get('/programs/drafts', function(req, res, next) {
  var term = utils.keyValue('draftClassifications.' + req.user._id, {$exists: true})
  Program.find(term).exec(function(err, programs) {
    if (err) return next(err)
    res.send(programs.map(function(p) {
      var draft = p.draftClassifications[req.user._id]
      return {_id: p._id, name: p.name, creationDate: draft.creationDate}
    }))
  })
})

app.get('/programs/recent', function(req, res, next) {
  var ObjectId = mongoose.Types.ObjectId
  Program.aggregate({$match: {"classifications.author._id": ObjectId(req.user._id) }})
    .unwind("classifications")
    .project({"registrationDate": "$classifications.registrationDate"})
    .sort('-registrationDate')
    .limit(1)
    .exec(function(err, recents) {
      if (err) return next(err)
      async.map(recents, function(p, callback) {
        return Program.findById(p._id, callback)
      }, respond(res, next))
    })
})

app.delete('/programs/drafts/:id', function(req, res, next) {
  var unset = utils.keyValue('draftClassifications.' + req.user._id, "")
  Program.findByIdAndUpdate(req.params.id, {$unset: unset}, null, respond(res, next))
})

app.get('/programs/:id', function(req, res, next) {
  Program.findById(req.params.id, respond(res, next))
})

app.post('/programs/new', function(req, res, next) {
  var programType = parseInt(req.body.programType)
  if (!enums.util.isDefinedProgramType(programType)) return res.send(400)
  var data = { draftClassifications: {}, programType: programType }
  data.draftClassifications[req.user._id] = Program.createNewClassification(req.user)
  new Program(data).save(respond(res, next))
})

app.post('/programs/:id/register', function(req, res, next) {
  Program.findById(req.params.id, function(err, program) {
    if (err) return next(err)

    var newClassification = program.draftClassifications[req.user._id]
    newClassification.registrationDate = new Date()
    newClassification.status = 'registered'
    newClassification.author = { _id: req.user._id, name: req.user.name }

    delete program.draftClassifications[req.user._id]
    program.classifications.unshift(newClassification)
    program.markModified('draftClassifications')

    verifyTvSeries(program, function(err) {
      if (err) return next(err)
      program.save(function(err) {
        if (err) return next(err)
        verifyTvSeriesClassification(program, function(err) {
          if (err) return next(err)
          addInvoicerows(newClassification, function(err, _) {
            if (err) return next(err)
            sendEmail(classification.registrationEmail(program, newClassification, req.user), function(err) {
              if (err) return next(err)
              updateMetadataIndexes(program, function() {
                return res.send(program)
              })
            })
          })
        })
      })
    })

    function addInvoicerows(currentClassification, callback) {
      var seconds = durationToSeconds(currentClassification.duration)

      if (classification.isReclassification(program, currentClassification)) {
        if (enums.isOikaisupyynto(currentClassification.reason) && enums.authorOrganizationIsKavi(currentClassification)) {
          InvoiceRow.fromProgram(program, 'reclassification', seconds, 74 * 100).save(callback)
        } else if (!utils.hasRole(req.user, 'kavi')) {
          InvoiceRow.fromProgram(program, 'registration', seconds, 725).save(callback)
        } else {
          callback()
        }
      } else {
        InvoiceRow.fromProgram(program, 'registration', seconds, 725).save(function(err, saved) {
          if (err) return next(err)
          if (utils.hasRole(req.user, 'kavi')) {
            // duraation mukaan laskutus
            var classificationPrice = classification.price(program, seconds)
            InvoiceRow.fromProgram(program, 'classification', seconds, classificationPrice).save(callback)
          } else {
            callback()
          }
        })
      }
    }
  })
})

app.post('/programs/:id/reclassification', function(req, res, next) {
  Program.findById(req.params.id, function(err, program) {
    if (err) next(err)
    getRegistrationEmailsFromPreviousClassification(program, function(err, emails) {
      if (err) return next(err)
      var newClassification = Program.createNewClassification(req.user)
      newClassification.registrationEmailAddresses = emails
      if (!program.draftClassifications) program.draftClassifications = {}
      program.draftClassifications[req.user._id] = newClassification
      program.markModified('draftClassifications')
      program.save(respond(res, next))
    })
  })

  function getRegistrationEmailsFromPreviousClassification(program, callback) {
    var ids = _.uniq(_.reduce(program.classifications, function(acc, c) {
      return acc.concat(c.buyer && c.buyer._id ? [c.buyer._id] : []).concat(c.billing && c.billing._id ? [c.billing._id] : [])
    }, []))

    if (ids.length > 0) {
      Account.find({_id: {$in: ids}}).select('emailAddresses').exec(function(err, accounts) {
        if (err) return callback(err)
        var emails = _(accounts).map(function(a) { return a.emailAddresses }).flatten()
          .map(function(e) { return {email: e, manual: false}}).value()
        callback(null, emails)
      })
    } else {
      callback(null, [])
    }
  }
})

app.post('/programs/:id/categorization', function(req, res, next) {
  Program.findById(req.params.id, function(err, program) {
    if (err) return next(err)
    program.programType = parseInt(req.body.programType)
    if (enums.util.isTvEpisode(program)) {
      program.series = {
        _id: req.body.series._id,
        name: req.body.series.name
      }
      program.episode = req.body.episode
      program.season = _.isEmpty(req.body.season) ? null : req.body.season
    }

    verifyTvSeries(program, function(err) {
      if (err) return next(err)
      program.save(function(err, saved) {
        if (err) return next(err)
        verifyTvSeriesClassification(program, function(err) {
          if (err) return next(err)
          res.send(saved)
        })
      })
    })
  })
})

app.post('/programs/:id', function(req, res, next) {
  Program.findByIdAndUpdate(req.params.id, req.body, null, function(err, program) {
    if (err) return next(err)
    program.populateAllNames(function(err) {
      if (err) return next(err)
      program.save(respond(res, next))
    })
  })
})

app.get('/series/search/:query', function(req, res, next) {
  var q = { programType: 2 }
  var parts = toMongoArrayQuery(req.params.query)
  if (parts) q.allNames = parts
  Program.find(q, { name: 1 }).lean().limit(20).sort('name').exec(function(err, data) {
    if (err) return next(err)
    res.send(_.map(data, function(d) { return { _id: d._id, name: d.name[0] } }))
  })
})

app.get('/accounts/search', function(req, res, next) {
  var roles = req.query.roles ? req.query.roles.split(',') : []
  var q = { roles: { $in: roles }}
  var userId = req.query.user_id

  if (!userId && !utils.hasRole(req.user, 'kavi')) {
    q['users._id'] = req.user._id
  } else if (userId) {
    q['users._id'] = userId
  }

  if (req.query.q && req.query.q.length > 0) q.name = new RegExp("^" + req.query.q, 'i')
  Account.find(q).sort('name').limit(50).exec(respond(res, next))
})

app.get('/accounts/:id', function(req, res, next) {
  Account.findById(req.params.id, respond(res, next))
})

app.get('/users', function(req, res, next) {
  var roleFilters = req.query.filters
  User.find(roleFilters ? { role: { $in: roleFilters }} : {}, respond(res, next))
})

app.delete('/users/:id', function(req, res, next) {
  if (!utils.hasRole(req.user, 'root')) return res.send(403)
  User.findByIdAndRemove(req.params.id, respond(res, next))
})

app.get('/users/exists/:username', function(req, res, next) {
  User.findOne({ username: req.params.username }, function(err, user) {
    if (err) return next(err)
    res.send({ exists: !!user })
  })
})

function userHasRequiredFields(user) {
  return (user.username != '' && user.emails[0].length > 0 && user.name != '')
}

app.post('/users/new', function(req, res, next) {
  if (userHasRequiredFields(req.body)) {
    new User(req.body).save(function(err, user) {
      if (err) return next(err)

      createAndSaveHash(user, function(err) {
        if (err) return next(err)

        var subject = 'Käyttäjätunnusten aktivointi'
        var text = 'Tämän linkin avulla pääset aktivoimaan käyttäjätunnuksesi: '

        sendHashLinkViaEmail(user, subject, text, respond(res, next, user))
      })
    })
  } else {
    return res.send(500)
  }
})

app.post('/users/:id', function(req, res, next) {
  User.findByIdAndUpdate(req.params.id, req.body, function(err, user) {
    if (err) return next(err)
    res.send(user)
  })
})

app.get('/actors/search/:query', queryNameIndex('Actor'))
app.get('/directors/search/:query', queryNameIndex('Director'))
app.get('/productionCompanies/search/:query', queryNameIndex('ProductionCompany'))

app.get('/invoicerows/:begin/:end', function(req, res, next) {
  var format = "DD.MM.YYYY"
  var begin = moment(req.params.begin, format)
  var end = moment(req.params.end, format).add('days', 1)
  InvoiceRow.find({registrationDate: {$gte: begin, $lt: end}}).sort('registrationDate').exec(respond(res, next))
})

app.post('/proe', express.urlencoded(), function(req, res, next) {
  var dates = { begin: req.body.begin, end: req.body.end }
  var invoiceIds = req.body.invoiceId
  if (!Array.isArray(invoiceIds)) invoiceIds = [invoiceIds]
  InvoiceRow.find({ _id: { $in: invoiceIds }}).sort('registrationDate').lean().exec(function(err, rows) {
    var accountIds = _(rows).map(function(i) { return i.account._id.toString() }).uniq().value()
    Account.find({ _id: { $in: accountIds } }).lean().exec(function(err, accounts) {
      var accountMap = _.indexBy(accounts, '_id')

      function accountId(row) { return row.account._id }
      function toNamedTuple(pair) { return { account: accountMap[pair[0]], rows: pair[1] } }
      function accountName(tuple) { return tuple.account.name }

      var data = _(rows).groupBy(accountId).pairs().map(toNamedTuple).sortBy(accountName).value()
      var result = proe(dates, data)
      res.setHeader('Content-Disposition', 'attachment; filename=proe-'+dates.begin+'-'+dates.end+'.txt')
      res.setHeader('Content-Type', 'text/plain')
      res.send(result)
    })
  })
})

app.post('/xml/v1/programs/:token', authenticateXmlApi, function(req, res, next) {
  var now = new Date()
  var account = { _id: req.account._id, name: req.account.name }
  var root = builder.create("ASIAKAS")
  req.resume()
  var xmlDoc = ""
  req.on('data', function(chunk) { xmlDoc += chunk })
  req.on('end', function() {
    var doc = { xml: xmlDoc.toString('utf-8'), date: now, account: account }
    new schema.XmlDoc(doc).save(function(err) {
      if (err) console.error(err)
    })
  })

  xml.readPrograms(req, function(err, programs) {
    if (programs.length == 0) {
      writeError('Yhtään kuvaohjelmaa ei voitu lukea', root)
      return res.send(root.end({ pretty: true, indent: '  ', newline: '\n' }))
    }

    async.eachSeries(programs, handleXmlProgram, function(err) {
      if (err) {
        writeError('Järjestelmävirhe: ' + err, root)
        console.error(err)
      }
      res.set('Content-Type', 'application/xml');
      res.send(err ? 500 : 200, root.end({ pretty: true, indent: '  ', newline: '\n' }))
    })
  })

  function writeError(err, parent) {
    parent.ele('VIRHE', err)
  }

  function handleXmlProgram(data, callback) {
    var program = data.program
    var ele = root.ele('KUVAOHJELMA')
    ele.ele('ASIAKKAANTUNNISTE', program.externalId)

    if (data.errors.length > 0) {
      return writeErrAndReturn(data.errors)
    }

    if (!utils.isValidYear(program.year)) {
      return writeErrAndReturn("Virheellinen vuosi: " + program.year)
    }

    program.customersId = { account: req.account._id, id: program.externalId }
    Program.findOne({ customersId: program.customersId }, { _id: 1 }).lean().exec(function(err, duplicate) {
      if (err) return callback(err)

      if (duplicate) {
        return writeErrAndReturn("Kuvaohjelma on jo olemassa asiakkaan tunnisteella: " + program.externalId)
      }

      verifyValidAuthor(program, function (err) {
        if (err) return callback(err)
        verifyParentProgram(program, function (err) {
          if (err) return callback(err)
          ele.ele('STATUS', 'OK')
          var p = new Program(program)
          p.classifications[0].status = 'registered'
          p.classifications[0].creationDate = now
          p.classifications[0].registrationDate = now
          p.classifications[0].billing = account
          p.classifications[0].buyer = account
          p.populateAllNames(function (err) {
            if (err) return callback(err)
            p.save(function (err) {
              if (err) return callback(err)
              verifyTvSeriesClassification(p, function(err) {
                if (err) return callback(err)
                var seconds = durationToSeconds(_.first(p.classifications).duration)
                InvoiceRow.fromProgram(p, 'registration', seconds, 725).save(function (err, saved) {
                  if (err) return callback(err)
                  updateActorAndDirectorIndexes(p, callback)
                })
              })
            })
          })
        })
      })
    })

    function verifyValidAuthor(program, callback) {
      var userName = program.classifications[0].author.name
      var user = _.find(req.account.users, { name: userName })
      if (user) {
        program.classifications[0].author._id = user._id
        User.findOne({ username: userName, active: true }, { _id: 1 }).lean().exec(function(err, doc) {
          if (err) return callback(err)
          if (doc) {
            return callback()
          } else {
            return writeErrAndReturn("Virheellinen LUOKITTELIJA: " + userName)
          }
        })
      } else {
        return writeErrAndReturn("Virheellinen LUOKITTELIJA: " + userName)
      }
    }

    function verifyParentProgram(program, callback) {
      if (!enums.util.isTvEpisode(program)) return callback()
      var parentName = program.parentTvSeriesName.trim()
      Program.findOne({ programType: 2, name: parentName }, function(err, parent) {
        if (err) return callback(err)
        if (!parent) {
          createParentProgram(program, parentName, callback)
        } else {
          program.series = { _id: parent._id, name: parent.name[0] }
          callback()
        }
      })
    }

    function writeErrAndReturn(errors) {
      ele.ele('STATUS', 'VIRHE')
      if (!_.isArray(errors)) errors = [errors]
      errors.forEach(function(msg) { writeError(msg, ele) })
      callback()
    }
  }
})

function createParentProgram(program, parentName, callback) {
  var parent = new Program({ programType: 2, name: [parentName] })
  parent.populateAllNames(function(err) {
    if (err) return callback(err)
    parent.save(function(err, saved) {
      if (err) return callback(err)
      program.series = { _id: saved._id, name: saved.name[0] }
      callback()
    })
  })
}

if (isDev()) {
  var liveReload = require('express-livereload')
  liveReload(app, { watchDir: path.join(__dirname, '../client') })
}

var server = app.listen(process.env.PORT || 3000, function() {
  console.log('Listening on port ' + server.address().port)
})

var checkExpiredCerts = new CronJob('0 0 0 * * *', function() {
  User.find({ $and: [
    { certificateEndDate: { $lt: new Date() }},
    { active: { $ne: false }},
    {'emails.0': { $exists: true }}
  ]}, function(err, users) {
    if (err) throw err

    users.forEach(function(user) {
      user.update({ active: false }, logError)

      sendEmail({
        recipients: [ user.emails[0] ],
        subject: 'Luokittelusertifikaattisi on vanhentunut',
        body: 'Luokittelusertifikaattisi on vanhentunut ja sisäänkirjautuminen tunnuksellasi on estetty.'
      }, logError)
    })
  })
})
checkExpiredCerts.start()

var checkCertsExpiringSoon = new CronJob('0 0 1 * * *', function() {
  User.find({ $and: [
    { certificateEndDate: { $lt: moment().add(3, 'months').toDate() }},
    { active: { $ne: false }},
    {'emails.0': { $exists: true }},
    { certExpiryReminderSent: { $ne: true }}
  ]}, function(err, users) {
    if (err) throw err
    users.forEach(function(user) {
      sendEmail({
        recipients: [ user.emails[0] ],
        subject: 'Luokittelusertifikaattisi on vanhentumassa',
        body: 'Luokittelusertifikaattisi on vanhentumassa ' + moment(user.certificateEndDate).format('DD.MM.YYYY') +
          '. Uusithan sertifikaattisi, jotta tunnustasi ei suljeta.'
      }, function(err) {
        if (err) console.error(err)
        else user.update({ certExpiryReminderSent: true }, logError)
      })
    })
  })
})
checkCertsExpiringSoon.start()

function nocache(req, res, next) {
  res.header('Cache-Control', 'private, no-cache, no-store, must-revalidate')
  res.header('Expires', '-1')
  res.header('Pragma', 'no-cache')
  next()
}

function authenticate(req, res, next) {
  var whitelist = [
    'GET:/index.html', 'GET:/public.html', 'GET:/templates.html', 'GET:/public/search/',
    'GET:/vendor/', 'GET:/shared/', 'GET:/images/', 'GET:/style.css', 'GET:/js/', 'GET:/xml/schema',
    'POST:/login', 'POST:/logout', 'POST:/xml', 'POST:/forgot-password', 'GET:/reset-password.html',
    'POST:/reset-password', 'GET:/check-reset-hash'
  ]
  var url = req.method + ':' + req.path
  if (url == 'GET:/') return next()
  var isWhitelistedPath = _.any(whitelist, function(p) { return url.indexOf(p) == 0 })
  if (isWhitelistedPath) return next()
  var cookie = req.signedCookies.user
  if (cookie) {
    req.user = cookie
    return next()
  }
  return res.send(403)
}

function authenticateXmlApi(req, res, next) {
  req.pause()
  Account.findOne({apiToken: req.params.token}).lean().exec(function(err, data) {
    if (data) {
      req.account = data
      return next()
    } else {
      res.send(403)
    }
  })
}

function sendEmail(data, callback) {
  var email = new sendgrid.Email({ from: 'no-reply@kavi.fi', subject: data.subject, html: data.body })
  data.recipients.forEach(function(to) { email.addTo(to) })

  if (process.env.NODE_ENV === 'production') {
    sendgrid.send(email, callback)
  } else {
    console.log('email (suppressed): ', email)
    return callback()
  }
}

function isDev() {
  return process.env.NODE_ENV == undefined || process.env.NODE_ENV == 'development'
}

function updateActorAndDirectorIndexes(program, callback) {
  schema.Actor.updateWithNames(program.actors, function() {
    schema.Director.updateWithNames(program.directors, callback)
  })
}
function updateMetadataIndexes(program, callback) {
  schema.ProductionCompany.updateWithNames(program.productionCompanies, function() {
    updateActorAndDirectorIndexes(program, callback)
  })
}

function durationToSeconds(duration) {
  if (!duration) return 0
  var parts = /(?:(\d+)?:)?(\d+):(\d+)$/.exec(duration)
    .slice(1).map(function (x) { return x === undefined ? 0 : parseInt(x) })
  return (parts[0] * 60 * 60) + (parts[1] * 60) + parts[2]
}

function queryNameIndex(schemaName) {
  return function(req, res, next) {
    var q = {}
    var parts = toMongoArrayQuery(req.params.query)
    if (parts) q.parts = parts
    schema[schemaName].find(q, { name: 1 }).limit(100).sort('name').exec(function(err, docs) {
      if (err) return next(err)
      res.send(_.pluck(docs || [], 'name'))
    })
  }
}

function toMongoArrayQuery(string) {
  var words = (string || '').trim().toLowerCase()
  return words ? { $all: words.split(/\s+/).map(toTerm) } : undefined

  function toTerm(s) {
    if (/^".+"$/.test(s)) {
      return s.substring(1, s.length - 1)
    } else {
      return new RegExp('^' + utils.escapeRegExp(s))
    }
  }
}

function respond(res, next, overrideData) {
  return function(err, data) {
    if (err) return next(err)
    res.send(overrideData || data)
  }
}

function verifyTvSeriesClassification(program, callback) {
  if (!enums.util.isTvEpisode(program)) return callback()
  Program.updateTvSeriesClassification(program.series._id, callback)
}

function verifyTvSeries(program, callback) {
  if (enums.util.isTvEpisode(program) && program.series._id == null) {
    createParentProgram(program, program.series.name.trim(), callback)
  } else return callback()
}

function logError(err) {
  if (err) console.error(err)
}
