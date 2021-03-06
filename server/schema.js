var mongoose = require('mongoose')

var classification = {
  author: String,
  buyer: {_id: mongoose.Schema.Types.ObjectId, name: String},
  billing: {_id: mongoose.Schema.Types.ObjectId, name: String},
  format: String,
  duration: String, // for now matches a regexp in the client
  safe: Boolean,
  criteria: [Number],
  comments: {}
}

var Movie = exports.Movie = mongoose.model('movies', {
  'emeku-id': { type: String, index: true },
  name: {type: [String], index: true},
  'name-fi': {type: [String], index: true},
  'name-sv': {type: [String], index: true},
  deleted: Boolean,
  country: String,
  year: Number,
  'production-companies': [{_id: mongoose.Schema.Types.ObjectId, name: String}],
  genre: String,
  directors: {type: [String], index: true},
  actors: {type: [String], index: true},
  synopsis: String,
  classifications: [classification]
})

var ProductionCompany = exports.ProductionCompany = mongoose.model('production_companies', {
  name: {type: String, index: true}
})

var Account = exports.Account = mongoose.model('accounts', {
  'emeku-id': String,
  name: {type: String, index: true}
})

