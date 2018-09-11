window.customElements.define('tinymce-editor', class extends window.HTMLElement {
  constructor () {
    super()
    this._editorValue = this.getAttribute('editorValue')
  }
  get editorValue () {
    return this._editorValue
  }
  set editorValue (value) {
    if (this.editorValue === value) return
    this._editorValue = value
    if (!this.editor) return
    this._editor.setContent(value)
  }
  connectedCallback () {
    const self = this
    window.tinymce.init({
      target: this,
      min_height: 200,
      plugins: 'image link paste contextmenu textpattern autolink lists',
      menubar: false,
      statusbar: false,
      toolbar: 'formatselect | bold italic underline | link unlink | bullist numlist blockquote',
      init_instance_callback: function (editor) {
        self._editor = editor
        editor.on('change', function (e) {
          self._editorValue = editor.getContent()
          self.dispatchEvent(new window.CustomEvent('editorChanged'))
        })
        editor.setContent(self.editorValue)
      }
    })
  }
})

const local = {
  read: (key) => {
    const value = window.localStorage.getItem(key)
    return JSON.parse(!value || value === 'undefined' ? null : value)
  },
  write: (key, value) => window.localStorage.setItem(key, JSON.stringify(value)),
  clear: (key) => window.localStorage.clear(key)
}
const app = window.Elm.Main.init({
  flags: {
    baseUrl: '',
    user: local.read('user')
  }
})
app.ports.outgoing.subscribe((message) => {
  switch (message.action) {
    case 'SetUser':
      return local.write('user', message.data)
    case 'ResetUser':
      return local.clear('user')
  }
})
