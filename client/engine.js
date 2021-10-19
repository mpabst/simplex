import {
  eventListenersModule,
  init as initSnabbdom,
  propsModule,
  styleModule
} from 'https://unpkg.com/snabbdom@3.0.3?module'

import { parser } from './parser.js'

export class Engine {
  static RENDERING_PATH = '/rendering.pl'
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
    // TODO: Put in /usr/lib ?
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

  render() {
    const rendering =
      new TextDecoder().decode(FS.readFile(Engine.RENDERING_PATH))
    console.log(rendering)
    const newVDOM = this.parse(rendering.trim())
    this.patch(this.vDOM ?? this.container, newVDOM)
    this.vDOM = newVDOM
  }

  tick() {
    this.query('process_tick.')
    this.render()
  }
}
