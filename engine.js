import {
  eventListenersModule,
  init as initSnabbdom,
  propsModule,
  styleModule
} from 'https://unpkg.com/snabbdom@3.0.3?module'

import { parser } from './parser.js'

export class Engine {
  static files = ['iris', 'query', 'commit', 'work', 'todos']

  patch = initSnabbdom([propsModule, styleModule, eventListenersModule])
  container = document.getElementById('container')

  vDOM

  constructor(swipl) {
    this.parse = parser(this.handleEvent.bind(this))
    this.swipl = swipl
  }

  async init() {
    for (const file of this.constructor.files) {
      let resp = await fetch(file + '.pl')
      if (!resp.ok) console.error(resp)
      this.consult(file, await resp.text())
    }
    this.tick()
  }

  consult(name, contents) {
    const path = `/${name}.pl`
    FS.writeFile(path, contents)
    this.query(`consult('${path}').`)
  }

  handleEvent(ev) {
    this.query(`write_event('${ev.target.id}', '${ev.type}').`)
    this.tick()
  }

  query(q) {
    return this.swipl.query(q)
  }

  render(line) {
    const match = line.match(/^Html = (.+)$/)
    if (match) {
      const newVDOM = this.parse(match[1].trim())
      setTimeout(() => {
        debugger
        this.patch(this.vDOM ?? this.container, newVDOM)
        this.vDOM = newVDOM
      }, 0)
    } else console.log(line)
  }

  tick() {
    this.query('process_tick(Html).')
  }
}
