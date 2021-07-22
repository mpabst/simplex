import {
  eventListenersModule,
  init as initSnabbdom,
  h,
  propsModule,
  styleModule
} from 'https://unpkg.com/snabbdom@3.0.3?module'

class Engine {
  patch = initSnabbdom([propsModule, styleModule, eventListenersModule])
  container = document.getElementById('container')

  session
  vDOM

  async init() {
    let resp = await fetch('index.pl')
    if (!resp.ok) console.error(resp)
    let src = await resp.text()

    this.session = pl.create()
    await this.consult(src)
    await this.tick()
  }

  async answer() {
    return new Promise((resolve, reject) => {
      this.session.answer({
        success: resolve,
        error: reject,
        fail: reject,
        limit: reject
      })
    })
  }

  async consult(src) {
    return new Promise((success, error) => {
      this.session.consult(src, { success, error })
    })
  }

  async handleEvent(ev) {
    await this.query(`write_event('${ev.target.id}', '${ev.type}').`)
    await this.answer()
    await this.tick()
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

  async query(q) {
    return new Promise((success, error) =>
      this.session.query(q, { success, error })
    )
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

  async tick() {
    await this.query('process_tick(Html).')
    const rendering = await this.answer()
    const newVDOM = this.prolog2h(rendering.links.Html)[0]
    this.patch(this.vDOM || this.container, newVDOM)
    this.vDOM = newVDOM
  }
}

new Engine().init()
