const jangle = require('../index.js')

jangle.start({
  lists: {
    Author: {
      name: {
        type: String,
        required: true
      },
      email: String
    },
    'Blog Post': {
      title: {
        type: String,
        required: true
      },
      author: {
        type: jangle.types.relationship,
        ref: 'Author'
      }
    }
  }
})
