const jangle = require('../index.js')

jangle.start({
  lists: {
    Author: {
      name: {
        type: String,
        required: true
      },
      contact: {
        email: {
          type: String,
          required: true
        },
        phone: String,
        fax: String
      },
      bio: {
        type: String,
        richText: true
      }
    },
    'Blog Post': {
      name: {
        label: 'Title',
        type: String,
        required: true
      },
      content: {
        type: String,
        required: true,
        richText: true
      }
    }
  }
})
