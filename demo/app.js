const jangle = require('../index.js')

jangle.start({
  lists: {
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
