const path = require('path')
const express = require('express')
const jangle = require('@jangle/api')
const Schema = jangle.Schema

jangle.start({
  core: {
    lists: {
      BlogPost: new Schema({
        title: String
      })
    }
  }
})
  .then((app) => {
    app.use('/public', express.static(path.join(__dirname, 'public')))
    app.get('*', (_req, res) =>
      res.sendFile(path.join(__dirname, 'public', 'index.html'))
    )
  })
  .catch(console.error)
