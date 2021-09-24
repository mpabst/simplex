import {
  eventListenersModule,
  init as initSnabbdom,
  h,
  propsModule,
  styleModule
} from 'https://unpkg.com/snabbdom@3.0.3?module'

export class Engine {
  static files = ['iris', 'query', 'commit', 'work', 'todos']

  patch = initSnabbdom([propsModule, styleModule, eventListenersModule])
  container = document.getElementById('container')

  vDOM

  constructor(swipl) {
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

  handleEventBound = this.handleEvent.bind(this)

  prolog2h(pl) {
    if (!pl.indicator) return [pl.value]

    // else we're a Term
    const { id, indicator, args } = pl

    switch (indicator) {
      case 'h/2':
        return [h(args[0].id, this.term2props(args[1]), [])]
      case 'h/3':
        return [h(args[0].id, this.term2props(args[1]), this.prolog2h(args[2]))]
      case './2':
        return this.prolog2h(args[0]).concat(this.prolog2h(args[1]))
      case '[]/0':
        return []
    }

    switch (indicator.match(/\/(\d+)$/)[1]) {
      case '0':
        return [id]
      case '1':
        // props
        return [[id, args[0].id]]
    }

    throw `unknown: ${indicator}`
  }

  query(q) {
    return this.swipl.query(q)
  }

  term2props(term) {
    const on = {}
    const style = {}
    const props = {}
    for (const [k, v] of this.prolog2h(term))
      if (k === 'on') on[v] = this.handleEventBound
      else props[k] = v
    return { on, style, props }
  }

  tick() {
    console.log(this.query('process_tick(Html).'))
    // const rendering = await this.answer()
    // const newVDOM = this.prolog2h(rendering.links.Html)[0]
    // this.patch(this.vDOM || this.container, newVDOM)
    // this.vDOM = newVDOM
  }
}
