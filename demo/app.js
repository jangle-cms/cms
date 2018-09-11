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
      name: {
        label: 'Title',
        type: String,
        required: true
      },
      date: {
        type: Date,
        required: false
      },
      relatedPosts: [{
        type: jangle.types.relationship,
        ref: 'Blog Post'
      }],
      fullnames: [{
        first: {
          type: String
        },
        last: [{
          middle: String
        }]
      }],
      fullname: {
        first: {
          type: String
        },
        last: String
      },
      scores: [Number],
      author: {
        type: jangle.types.relationship,
        ref: 'Author'
      }
    }
  }
})
