const path = require('path')
const express = require('express')
const jangle = require('@jangle/api')
const { Schema } = jangle

module.exports = {
  start: (config) =>
    Promise.resolve(transform(config))
      .then(jangle.start)
      .then(app => {
        const url = getFullUrl(config)
        if (config.prefix) {
          app.use(config.prefix, express.static(path.join(__dirname, 'public')))
          app.get(config.prefix + '*', (_req, res) =>
            res.sendFile(path.join(__dirname, 'public', 'index.html'))
          )
        } else {
          app.use(express.static(path.join(__dirname, 'public')))
          app.get('*', (_req, res) =>
            res.sendFile(path.join(__dirname, 'public', 'index.html'))
          )
        }
        console.info(`Jangle CMS ready at ${url}`)
        return app
      })
      .catch(console.error),
  types: {
    relationship: Schema.Types.ObjectId
  }
}

const transform = ({ api = {}, lists = [], items = [], ...core } = {}) => ({
  ...core,
  lists: Object.keys(lists).reduce(...schemify(lists)),
  items: Object.keys(items).reduce(...schemify(items)),
  api: { ...api, _print: false }
})

const schemify = (original = {}) => [
  (obj, key) => ({ ...obj, [key]: new Schema(original[key]) }),
  {}
]

const getFullUrl = ({ api = {} } = {}) =>
  `http://localhost:${api.port || process.env.PORT || 3000}${api.prefix || ''}`
