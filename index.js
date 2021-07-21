import {
  eventListenersModule,
  init as initSnabbdom,
  h,
  propsModule,
  styleModule
} from 'https://unpkg.com/snabbdom@3.0.3?module'

let session

const patch = initSnabbdom([propsModule, styleModule, eventListenersModule])
const container = document.getElementById('container')
let vDOM

async function init() {
  let resp = await fetch('index.pl')
  if (!resp.ok) console.error(resp)
  let src = await resp.text()

  session = pl.create()
  await consult(src)
  await tick()
}

async function answer() {
  return new Promise((resolve, reject) => {
    session.answer({
      success: resolve,
      error: reject,
      fail: reject,
      limit: reject
    })
  })
}

async function consult(src) {
  return new Promise((success, error) => {
    session.consult(src, { success, error })
  })
}

async function handleEvent(ev) {
  await query(`write_event('${ev.target.id}', '${ev.type}').`)
  await answer()
  await tick()
}

async function query(q) {
  return new Promise((success, error) => session.query(q, { success, error }))
}

function term2props(term) {
  const on = {}
  const style = {}
  const props = {}
  for (const [k, v] of prolog2h(term))
    if (k === 'on') on[v] = handleEvent
    else props[k] = v
  return { on, style, props }
}

function prolog2h(pl) {
  if (!pl.indicator) return [pl.value]

  // else we're a Term
  const { id, indicator, args } = pl

  switch (indicator) {
    case 'h/2':
      return [h(args[0].id, term2props(args[1]), [])]
    case 'h/3':
      return [h(args[0].id, term2props(args[1]), prolog2h(args[2]))]
    case './2':
      return prolog2h(args[0]).concat(prolog2h(args[1]))
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

async function tick() {
  await query('process_tick(Html).')
  const rendering = await answer()
  const newVDOM = prolog2h(rendering.links.Html)[0]
  patch(vDOM || container, newVDOM)
  vDOM = newVDOM
}

init()
