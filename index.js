const path = require('path')
const express = require('express')
const jangle = require('@jangle/api')

module.exports = {
  start: (config) =>
    jangle.start(config)
      .then(app => {
        app.use(express.static(path.join(__dirname, 'public')))
        app.get('*', (_req, res) =>
          res.sendFile(path.join(__dirname, 'public', 'index.html'))
        )
      })
      .catch(console.error)
}
