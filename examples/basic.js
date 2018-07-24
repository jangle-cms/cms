const jangle = require('../index')
const Schema = jangle.Schema

jangle.start({
  core: {
    lists: {
      Person: new Schema({
        name: {
          type: String,
          required: true
        },
        email: {
          type: String
        },
        bio: {
          type: String
        }
      }),
      BlogPost: new Schema({
        title: {
          type: String,
          required: true
        },
        author: {
          type: Schema.Types.ObjectId,
          ref: 'Person',
          required: true
        }
      })
    }
  }
}).catch(error => console.error(error) || process.exit(1))
