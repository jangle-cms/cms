const express = require('express')
const app = express()
const path = require('path')

app.use('/public', express.static(path.join(__dirname, 'public')))
app.get('*', (_req, res) => res.sendFile(path.join(__dirname, 'public', 'index.html')))

const port = process.env.PORT || 3000
app.listen(port, () => console.log(`Jangle ready at http://localhost:${port}`))
