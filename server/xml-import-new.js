var xml = require('xml-object-stream'),
    validateProgram = require('XmlValidation').validateProgram

exports.readPrograms = function (body, callback) {
  var stream = xml.parse(body)
  var programs = []

  stream.each('KUVAOHJELMA', function(xml) {
    programs.push(validateProgram(xml))
  })

  stream.on('end', function() {
    return callback(null, programs)
  })
}
