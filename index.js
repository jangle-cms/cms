const jangle = require('@jangle/api')
const express = require('express')
const path = require('path')

module.exports = {
  start: (config) =>
    jangle.start(config)
      .then(app => {
        app.use('/public', express.static(path.join(__dirname, 'public')))
        app.get('*', (_req, res) =>
          res.sendFile(path.join(__dirname, 'public', 'index.html'))
        )
        return app
      }),
  Schema: jangle.Schema
}
